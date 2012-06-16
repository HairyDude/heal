module EveApiValues where

import Prelude (map, (++), ($))

import EveApiTypes

import Data.Map (Map)
import qualified Data.Map as M

----------------------------------------------
-- List of calls by valid set of arguments. --
----------------------------------------------

apiCalls :: Map (Call, Scope) CallArgs
apiCalls = M.fromList $
    -- Public calls that need no auth and take no arguments
    [((CallList, APIScope), CallArgs NoKey [])] ++
    map (\name -> ((name, EVEScope), CallArgs NoKey []))
        [CertificateTree        -- static
        ,ErrorList              -- static
        ,RefTypes               -- static
        ,SkillTree              -- static
        ,ConquerableStationList
        ,FacWarStats
        ,FacWarTopStats
        ,FacWarSystems
        ,Jumps
        ,Kills
        ,Sovereignty
        ,ServerStatus
        ] ++
    map (\name -> ((name, MapScope), CallArgs NoKey []))
        [FacWarSystems
        ,Jumps
        ,Kills
        ,Sovereignty
        ] ++
    [((ServerStatus, ServerScope), CallArgs NoKey [])
    -- Public calls that need no auth, but take arguments
    ,((TypeName, EVEScope), CallArgs NoKey [Arg ArgTIDs AIntList Mandatory])
    ,((AllianceList, EVEScope), CallArgs NoKey [Arg ArgTVersion AInteger Optional])
    ,((CharacterID, EVEScope), CallArgs NoKey [Arg ArgTNames AStringList Mandatory])
    ,((CharacterName, EVEScope), CallArgs NoKey [Arg ArgTIDs AIntList Mandatory])
    -- Public calls with optional auth (exposing extra information)
    -- Character or corp must be specified, but should be inferred from the key
    -- if provided, not explicitly given.
    ,((CharacterInfo, EVEScope),
        CallArgs (CharKey Optional) [Arg ArgTCharacterID AInteger FromKey])
    ,((CorporationSheet, CorpScope),
        CallArgs (CorpKey Optional) [Arg ArgTCorporationID AInteger FromKey])
    -- Private calls, requiring a key in all cases.
    ,((APIKeyInfo, AcctScope), CallArgs (AnyKey Mandatory) [])
    ,((AccountStatus, AcctScope), CallArgs (CharKey Mandatory) [])
    ,((Characters, AcctScope), CallArgs (CharKey Mandatory) [])] ++
    -- Corp calls, no args
    map (\name -> ((name, CorpScope), CallArgs (CorpKey Mandatory) []))
        [AccountBalance   -- ignores accountKey
        ,AssetList
        ,ContactList
        ,ContainerLog
        ,ContractBids
        ,FacWarStats
        ,IndustryJobs
        ,Medals
        ,MemberMedals
        ,MemberSecurity
        ,MemberSecurityLog
        ,OutpostList
        ,Shareholders
        ,Standings
        ,StarbaseList
        ,Titles] ++
    -- Character calls, no args
    map (\name -> ((name, CharScope), CallArgs (CharKey Mandatory) []))
        [AccountBalance
        ,AssetList
        ,CharacterSheet
        ,ContactList
        ,ContactNotifications
        ,ContractBids
        ,FacWarStats
        ,IndustryJobs
        ,MailingLists
        ,MailMessages
        ,Medals
        ,Notifications
        ,Research
        ,SkillInTraining
        ,SkillQueue
        ,Standings
        ,UpcomingCalendarEvents] ++
    -- Mail bodies, notification texts and item locations; take list of IDs
    map (\name -> ((name, CharScope),
            CallArgs (CharKey Mandatory) [Arg ArgTIDs AIntList Mandatory]))
        [Locations, MailBodies, NotificationTexts] ++
    [((Locations, CorpScope),
        CallArgs (CharKey Mandatory) [Arg ArgTIDs AIntList Mandatory])
    -- Calendar event attendees, takes list of event IDs
    ,((CalendarEventAttendees, CharScope),
        CallArgs (CharKey Mandatory) [Arg ArgTEventIDs AIntList Mandatory])
    -- Contracts. contractID to check only one contract
    ,((Contracts, CharScope),
        CallArgs (CharKey Mandatory) [Arg ArgTContractID AInteger Optional])
    ,((ContractItems, CharScope),
        CallArgs (CharKey Mandatory) [Arg ArgTContractID AInteger Mandatory])
    ,((Contracts, CorpScope),
        CallArgs (CorpKey Mandatory) [Arg ArgTContractID AInteger Optional])
    ,((ContractItems, CorpScope),
        CallArgs (CorpKey Mandatory) [Arg ArgTContractID AInteger Mandatory])
    -- Kill log. beforeKillID to check old kills
    ,((KillLog, CharScope),
        CallArgs (CharKey Mandatory) [Arg ArgTBeforeKillID AInteger Optional])
    ,((KillLog, CorpScope),
        CallArgs (CorpKey Mandatory) [Arg ArgTBeforeKillID AInteger Optional])
    --  Market orders. orderID to fetch a closed order
    ,((MarketOrders, CharScope),
        CallArgs (CharKey Mandatory) [Arg ArgTOrderID AInteger Optional])
    ,((MarketOrders, CorpScope),
        CallArgs (CorpKey Mandatory) [Arg ArgTOrderID AInteger Optional])
    -- Corp member tracking. extended=1 for extended info
    ,((MemberTracking, CorpScope),
        CallArgs (CorpKey Mandatory) [Arg ArgTExtended AInteger Optional])
    -- Wallet journal / transactions, optional args:
    -- - number of rows to return (default 50 / 1000, max 2560)
    -- - last entry to work back from (for fetching >2560 entries)
    -- - code for account division, 1000-1006 (default all (that character has
    --   access to?), not needed for characters)
    ,((WalletJournal, CharScope),
        CallArgs (CharKey Mandatory)
            [Arg ArgTRowCount AInteger Optional
            ,Arg ArgTFromID AInteger Optional])
    ,((WalletJournal, CorpScope),
        CallArgs (CorpKey Mandatory)
            [Arg ArgTRowCount AInteger Optional
            ,Arg ArgTFromID AInteger Optional
            ,Arg ArgTAccountKey AInteger Optional])
    ,((WalletTransactions, CharScope),
        CallArgs (CharKey Mandatory)
            [Arg ArgTRowCount AInteger Optional
            ,Arg ArgTFromID AInteger Optional])
    ,((WalletTransactions, CorpScope),
        CallArgs (CorpKey Mandatory)
            [Arg ArgTRowCount AInteger Optional
            ,Arg ArgTFromID AInteger Optional
            ,Arg ArgTAccountKey AInteger Optional])
    -- Starbase / outpost details, takes itemID of control tower / outpost
    ,((StarbaseDetail, CorpScope),
        CallArgs (CorpKey Mandatory) [Arg ArgTItemID AInteger Mandatory])
    ,((OutpostServiceDetail, CorpScope),
        CallArgs (CorpKey Mandatory) [Arg ArgTItemID AInteger Mandatory])
    ]
