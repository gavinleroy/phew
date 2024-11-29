module Blockchain

using SHA

num2hex(n) = string(n, base=16)
sha(msg) = join(num2hex.(sha256(msg)))

# global-prefix-length = 3
global_prefix_length = 3

# expected-prefix = string-repeat("0", global-prefix-length)
expected_prefix = "0" ^ global_prefix_length

# data Block:
#   | block(
#       ref hash :: String,
#       prev-hash :: String,
#       ref datum :: String,
#       timestamp :: String,
#       ref nonce :: Number)
# end
mutable struct Block
    hash::String
    prev_hash::String
    datum::String
    timestamp::String
    nonce::Number
end

# fun mine-block(datum :: String, prev-hash :: String) -> Block:
#   timestamp = "fixed for testing"
#   fun iter(nonce :: Number):
#     data-to-hash =
#       prev-hash + datum + timestamp + num-to-string(nonce)
#     h = sha(data-to-hash)
#     actual-prefix = string-substring(h, 0, global-prefix-length)
#     if actual-prefix == expected-prefix:
#       block(h, prev-hash, datum, timestamp, nonce)
#     else:
#       iter(nonce + 1)
#     end
#   end
#   iter(0)
# end
function mine_block(datum::String, prev_hash::String)
    timestamp = "fixed for testing"
    nonce = 0
    while true
        data_to_hash = prev_hash * datum * timestamp * string(nonce)
        h = sha(data_to_hash)
        actual_prefix = h[1:global_prefix_length]
        if actual_prefix == expected_prefix
            return Block(h, prev_hash, datum, timestamp, nonce)
        end
        nonce += 1
    end
end

# fun make-genesis-block() -> Block:
#   mine-block("genesis block", "0")
# end
function make_genesis_block()
    return mine_block("genesis block", "0")
end

# data Blockchain:
#   | blkchn(ref lob :: List<Block>)
# end
mutable struct Bchain
    lob::Vector{Block}
end

# fun make-blockchain() -> Blockchain:
#   first-b = make-genesis-block()
#   blkchn([list: first-b])
# end
function make_blockchain()
    first_b = make_genesis_block()
    return Bchain([first_b])
end

# fun add-data-to-blockchain(bc :: Blockchain, datum :: String):
#   old-newest-block =
#     cases (List) bc!lob:
#       | empty => raise("should never be empty")
#       | link(f, _) => f
#     end
#   new-block = mine-block(datum, old-newest-block!hash)
#   bc!{lob: link(new-block, bc!lob)}
# end
function add_data_to_blockchain(bc::Bchain, datum::String)
    old_newest_block = bc.lob[end]
    new_block = mine_block(datum, old_newest_block.hash)
    push!(bc.lob, new_block);
end

# fun block-hash-prefix-okay(b :: Block) -> Boolean:
#   actual-prefix = string-substring(b!hash, 0, global-prefix-length)
#   expected-prefix == actual-prefix
# end
function block_hash_prefix_okay(b::Block)
    actual_prefix = b.hash[1:global_prefix_length]
    return expected_prefix == actual_prefix
end

# fun calc-block-hash(b :: Block) -> String:
#   data-to-hash =
#     b.prev-hash + b!datum + b.timestamp + num-to-string(b!nonce)
#   sha(data-to-hash)
# end
function calc_block_hash(b::Block)
    data_to_hash = b.prev_hash * b.datum * b.timestamp * string(b.nonce)
    return sha(data_to_hash)
end

# fun verify-blockchain(bc :: Blockchain) -> Boolean:
#   fun verify-blocks(bs :: List<Block>) -> Boolean:
#     cases (List) bs:
#       | empty => raise("should never be empty")
#       | link(first-block, rest-blocks) =>
#         (first-block!hash == calc-block-hash(first-block))
#         and
#         (block-hash-prefix-okay(first-block))
#         and
#         cases (List) rest-blocks:
#           | empty => true
#           | link(next-block, _) =>
#             (first-block.prev-hash == next-block!hash)
#             and
#             (verify-blocks(rest-blocks))
#         end
#     end
#   end
#   verify-blocks(bc!lob)
# end
function verify_blockchain(bc::Bchain)
    bs = bc.lob

    if isempty(bs)
        throw("should never be empty")
    end

    first_block = bs[end]

    if first_block.hash != calc_block_hash(first_block) || !block_hash_prefix_okay(first_block)
        return false
    end

    prev_hash = first_block.hash
    for block in reverse(bs[1:end-1])
        if prev_hash == block.hash || block.hash != calc_block_hash(block)
            return false
        end
        prev_hash = block.hash
    end
    return true
end

# fun get-block(bc :: Blockchain, idx :: Number) -> Block:
#   bc!lob.get(idx)
# end
function get_block(bc::Bchain, idx::Number)
    return bc.lob[idx]
end

# fun re-mine-block(b :: Block) -> Nothing block:

#   fun set-block-hash():
#     h = calc-block-hash(b)
#     b!{hash: h}
#   end

#   b!{nonce: 1 + b!nonce}
#   set-block-hash()
#   if block-hash-prefix-okay(b) block:
#     nothing
#   else:
#     re-mine-block(b)
#   end
# end
function re_mine_block(b::Block)
    b.nonce += 1

    # INLINED set_block_hash
    h = calc_block_hash(b)
    b.hash = h

    if block_hash_prefix_okay(b)
        return
    else
        return re_mine_block(b)
    end
end

# fun tamper(bc :: Blockchain, idx :: Number, new-datum :: String,
#     redo-pow :: Boolean) -> Nothing block:
#   b = get-block(bc, idx)
#   b!{datum: new-datum}
#   when redo-pow:
#     re-mine-block(b)
#   end
# end
function tamper(bc::Bchain, idx::Number, new_datum::String, redo_pow::Bool)
    b = get_block(bc, idx)
    b.datum = new_datum
    if redo_pow
        re_mine_block(b)
    end
end

# check:
#   tamper(bc1, 0, "World", false)
#   bc1 violates verify-blockchain

#   tamper(bc1, 0, "world", false)
#   bc1 satisfies verify-blockchain

#   tamper(bc1, 1, "Hello", false)
#   bc1 violates verify-blockchain

#   tamper(bc1, 1, "hello", false)
#   bc1 satisfies verify-blockchain

#   tamper(bc1, 0, "World", true)
#   bc1 satisfies verify-blockchain # because it's the *newest* block

#   tamper(bc1, 0, "world", true)
#   bc1 satisfies verify-blockchain # because it's the *newest* block

#   tamper(bc1, 1, "Hello", true)
#   bc1 violates verify-blockchain

#   tamper(bc1, 1, "hello", false)
#   bc1 violates verify-blockchain

#   tamper(bc1, 1, "hello", true)
#   bc1 violates verify-blockchain
# end

end

using .Blockchain

function make_simple()
    # # bc1 = make-blockchain()
    bc1 = Blockchain.make_blockchain()

    # # add-data-to-blockchain(bc1, "hello")
    Blockchain.add_data_to_blockchain(bc1, "hello")

    # # add-data-to-blockchain(bc1, "world")
    Blockchain.add_data_to_blockchain(bc1, "world")

    bc1
end

bc1 = make_simple()

bc2 = make_simple()

Blockchain.tamper(bc2, 1, "World", false)

@assert Blockchain.verify_blockchain(bc1)
@assert !Blockchain.verify_blockchain(bc2)
