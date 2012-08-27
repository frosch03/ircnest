module IrcLine.Command.ModeString
    ( ModeString(..)
    )
where

type NickOrChan = String

data ChannelModes
    = Operator String
    | FlagPrivate
    | FlagSecret
    | FlagInviteOnly
    | FlagTopicSettableOperatorOnly
    | NoMsgFromOutside
    | Moderated
    | UserLimit Int
    | BanMask String
    | Voice String
    | Password String

data UserModes
    = Invisible
    | ReceiveServerNotice 
    | ReceiveWallops
    | Op

data CombinedModes
    = ChannelMode ChannelModes
    | UserMode    UserModes

data ModeString
    = NickOrChan :+: CombinedModes
    | NickOrChan :-: CombinedModes


instance Show ChannelModes where
    show (Operator nick)                 = "o " ++ ' ':nick
    show (FlagPrivate)                   = "p"
    show (FlagSecret)                    = "s"
    show (FlagInviteOnly)                = "i"
    show (FlagTopicSettableOperatorOnly) = "t"
    show (NoMsgFromOutside)              = "n"
    show (Moderated)                     = "m"
    show (UserLimit limit)               = "l " ++ (show limit)
    show (BanMask mask)                  = "b " ++ mask
    show (Voice v)                       = "v " ++ v
    show (Password pass)                 = "k " ++ pass


instance Show UserModes where
    show (Invisible)           = "i" 
    show (ReceiveServerNotice) = "s"
    show (ReceiveWallops)      = "w"
    show (Op)                  = "o"


instance  Show CombinedModes where
    show (ChannelMode c) = show c
    show (UserMode u)    = show u

 
instance Show ModeString where
    show (s :+: cm) = s ++ " +" ++ (show cm)
    show (s :-: cm) = s ++ " -" ++ (show cm)


instance Read ChannelModes where
    readsPrec _ ('o': ' ': nick) = [(Operator nick, "")]
    readsPrec _ ['p']            = [(FlagPrivate, "")]
    readsPrec _ ['s']            = [(FlagSecret, "")]
    readsPrec _ ['i']            = [(FlagInviteOnly, "")]
    readsPrec _ ['t']            = [(FlagTopicSettableOperatorOnly, "")]
    readsPrec _ ['n']            = [(NoMsgFromOutside, "")]
    readsPrec _ ['m']            = [(Moderated, "")]
    readsPrec _ ('l': limit)     = [(UserLimit ((read limit) :: Int), "")]
    readsPrec _ ('b': mask)      = [(BanMask mask, "")]
    readsPrec _ ('v': ' ': nick) = [(Voice nick, "")]
    readsPrec _ ('k': ' ': pass) = [(Password pass, "")]


instance Read UserModes where
    readsPrec _ ['i'] = [(Invisible, "")]
    readsPrec _ ['s'] = [(ReceiveServerNotice , "")]
    readsPrec _ ['w'] = [(ReceiveWallops, "")]
    readsPrec _ ['o'] = [(Op, "")]

instance Read ModeString where
    readsPrec _ ('#':rst) 
        = [(op chan (ChannelMode (read mode)), "")]
        where chan      = takeWhile (/= ' ') $ '#':rst
              (pm:mode) = skipColon . tail . dropWhile (/= ' ') $ rst
              op        = if pm == '+' then (:+:) else (:-:)
              skipColon = (\(x:xs) -> if x == ':' then xs else (x:xs))

    readsPrec _ rst
        = [(op nick (UserMode (read mode)), "")]
        where nick      = takeWhile (/= ' ') $ rst
              (pm:mode) = skipColon . tail . dropWhile (/= ' ') $ rst
              op        = if pm == '+' then (:+:) else (:-:)
              skipColon = (\(x:xs) -> if x == ':' then xs else (x:xs))
    

