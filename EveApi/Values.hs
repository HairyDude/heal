module EveApi.Values where

import Prelude (map, (++), ($))

import EveApi.Types

import Data.Map (Map)
import qualified Data.Map as M

----------------------------------------------
-- List of calls by valid set of arguments. --
----------------------------------------------

apiCalls :: Map (Call, Scope) CallParams
apiCalls = M.fromList $
    -- Public calls that need no auth and take no arguments
    [((CallList, APIScope), CallParams APIScope CallList NoKey [])] ++
    map (\name -> ((name, EVEScope), CallParams EVEScope name NoKey []))
        [CertificateTree        -- static
        ,ErrorList              -- static
        ,RefTypes               -- static
        ,SkillTree              -- static
        ,ConquerableStationList
        ,FacWarStats
        ,FacWarTopStats
        ] ++
    map (\name -> ((name, MapScope), CallParams MapScope name NoKey []))
        [FacWarSystems
        ,Jumps
        ,Kills
        ,Sovereignty
        ] ++
    [((ServerStatus, ServerScope), CallParams ServerScope ServerStatus NoKey [])
    -- Public calls that need no auth, but take arguments
    ,((TypeName, EVEScope),
        CallParams EVEScope TypeName NoKey
            [Arg ArgTIDs AIntList Required])
    ,((AllianceList, EVEScope),
        CallParams EVEScope AllianceList NoKey
            [Arg ArgTVersion AInteger Optional])
    ,((CharacterID, EVEScope),
        CallParams EVEScope CharacterID NoKey
            [Arg ArgTNames AStringList Required])
    ,((CharacterName, EVEScope),
        CallParams EVEScope CharacterName NoKey
            [Arg ArgTIDs AIntList Required])
    -- Public calls with optional auth (exposing extra information)
    -- Character or corp must be specified, but should be inferred from the key
    -- if provided, not explicitly given.
    ,((CharacterInfo, EVEScope),
        CallParams EVEScope CharacterInfo (CharKey Optional)
            [Arg ArgTCharacterID AInteger FromKey])
    ,((CorporationSheet, CorpScope),
        CallParams CorpScope CorporationSheet (CorpKey Optional)
            [Arg ArgTCorporationID AInteger FromKey])
    -- Private calls, requiring a key in all cases.
    ,((APIKeyInfo, AcctScope),
        CallParams AcctScope APIKeyInfo (AnyKey Required) [])
    ,((AccountStatus, AcctScope),
        CallParams AcctScope AccountStatus (CharKey Required) [])
    ,((Characters, AcctScope),
        CallParams AcctScope Characters (CharKey Required) [])] ++
    -- Corp calls, no args
    map (\name -> ((name, CorpScope),
                    CallParams CorpScope name (CorpKey Required) []))
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
    -- (except characterID to specify which character you're interested in)
    map (\name -> ((name, CharScope),
                    CallParams CharScope name (CharKey Optional)
                        [Arg ArgTCharacterID AInteger FromKey]))
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
        CallParams CharScope name (CharKey Required)
            [Arg ArgTCharacterID AInteger FromKey
            ,Arg ArgTIDs AIntList Required]))
        [Locations, MailBodies, NotificationTexts] ++
    [((Locations, CorpScope),
        CallParams CorpScope Locations (CharKey Required)
            [Arg ArgTIDs AIntList Required])
    -- Calendar event attendees, takes list of event IDs
    ,((CalendarEventAttendees, CharScope),
        CallParams CharScope CalendarEventAttendees (CharKey Required)
            [Arg ArgTCharacterID AInteger FromKey
            ,Arg ArgTEventIDs AIntList Required])
    -- Contracts. contractID to check only one contract
    ,((Contracts, CharScope),
        CallParams CharScope Contracts (CharKey Required)
            [Arg ArgTCharacterID AInteger FromKey
            ,Arg ArgTContractID AInteger Optional])
    ,((ContractItems, CharScope),
        CallParams CharScope ContractItems (CharKey Required)
            [Arg ArgTCharacterID AInteger FromKey
            ,Arg ArgTContractID AInteger Required])
    ,((Contracts, CorpScope),
        CallParams CorpScope Contracts (CorpKey Required)
            [Arg ArgTCharacterID AInteger FromKey
            ,Arg ArgTContractID AInteger Optional])
    ,((ContractItems, CorpScope),
        CallParams CorpScope ContractItems (CorpKey Required)
            [Arg ArgTContractID AInteger Required])
    -- Kill log. beforeKillID to check old kills
    ,((KillLog, CharScope),
        CallParams CharScope KillLog (CharKey Required)
            [Arg ArgTCharacterID AInteger FromKey
            ,Arg ArgTBeforeKillID AInteger Optional])
    ,((KillLog, CorpScope),
        CallParams CorpScope KillLog (CorpKey Required)
            [Arg ArgTBeforeKillID AInteger Optional])
    --  Market orders. orderID to fetch a closed order
    ,((MarketOrders, CharScope),
        CallParams CharScope MarketOrders (CharKey Required)
            [Arg ArgTCharacterID AInteger FromKey
            ,Arg ArgTOrderID AInteger Optional])
    ,((MarketOrders, CorpScope),
        CallParams CorpScope MarketOrders (CorpKey Required)
            [Arg ArgTOrderID AInteger Optional])
    -- Corp member tracking. extended=1 for extended info
    ,((MemberTracking, CorpScope),
        CallParams CorpScope MemberTracking (CorpKey Required)
            [Arg ArgTExtended AInteger Optional])
    -- Wallet journal / transactions, optional args:
    -- - number of rows to return (default 50 / 1000, max 2560)
    -- - last entry to work back from (for fetching >2560 entries)
    -- - code for account division, 1000-1006 (default all (that character has
    --   access to?), not needed for characters)
    ,((WalletJournal, CharScope),
        CallParams CharScope WalletJournal (CharKey Required)
            [Arg ArgTCharacterID AInteger FromKey
            ,Arg ArgTRowCount AInteger Optional
            ,Arg ArgTFromID AInteger Optional])
    ,((WalletJournal, CorpScope),
        CallParams CorpScope WalletJournal (CorpKey Required)
            [Arg ArgTRowCount AInteger Optional
            ,Arg ArgTFromID AInteger Optional
            ,Arg ArgTAccountKey AInteger Optional])
    ,((WalletTransactions, CharScope),
        CallParams CharScope WalletTransactions (CharKey Required)
            [Arg ArgTCharacterID AInteger FromKey
            ,Arg ArgTRowCount AInteger Optional
            ,Arg ArgTFromID AInteger Optional])
    ,((WalletTransactions, CorpScope),
        CallParams CorpScope WalletTransactions (CorpKey Required)
            [Arg ArgTRowCount AInteger Optional
            ,Arg ArgTFromID AInteger Optional
            ,Arg ArgTAccountKey AInteger Optional])
    -- Starbase / outpost details, takes itemID of control tower / outpost
    ,((StarbaseDetail, CorpScope),
        CallParams CorpScope StarbaseDetail (CorpKey Required)
            [Arg ArgTItemID AInteger Required])
    ,((OutpostServiceDetail, CorpScope),
        CallParams CorpScope OutpostServiceDetail (CorpKey Required)
            [Arg ArgTItemID AInteger Required])
    ]
