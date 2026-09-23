000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4022500.                                                
000300 AUTHOR.         LINDA NILSSON.                                           
000400 DATE-WRITTEN.   03/02/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VOR BEHANDLINGSBILD SOM ERSÄTTER 4224                            
000900*        VIA CMD PÅ BILDEN ANGER MAN VAD MAN VILL GÖRA:                   
001000*        CMD S = SPECIALORDER; 4231 & FLVOR = J                           
001100*        CMD N = NORMAL ORDER; 4221 & FLVOR = J                           
001200*        CMD F = FÖRBIORDER;   4221 & FLVOR = J                           
001300*                                  & FLVORBI = J                          
001400*        CMD D = ANNULLERA RADEN FRÅN VOR-KÖN (NY STATUS)                 
001500*        CMD T = HOPP TILL TEXT-BILD 4275                                 
001600*                                                                         
001700*        TEVORMRK ÄR VOR-GRUPPENS VALFRIA MÄRKNING                        
001800*        AV DEN RAD SOM ÄR UNDER UTREDNING.                               
001900*                                                                         
002000*                                                                         
002100*        KDVORATG ANGER VAD MAN GJORT.                                    
002200*        KDVORATG = 0 : RADEN OBEHANDLAD PÅ VOR-KÖN                       
002300*        KDVORATG = 1 : RADEN TILL NY ORDER                               
002400*        KDVORATG = 8 : RADEN ANNULLERAS                                  
002500*                                                                         
002600*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002700*                                                                         
002800*        PROGRAMMET LÄSER      WDP7                                       
002900*        PROGRAMMET UPPDATERAR WDA6                                       
003000*        PROGRAMMET UPPDATERAR WDR6                                       
003100*        PROGRAMMET UPPDATERAR WDK9                                       
003200*        PROGRAMMET UPPDATERAR WDK7                                       
003300*        PROGRAMMET LÄSER      WDK6                                       
003400*        PROGRAMMET LÄSER      WDP5                                       
003500*        PROGRAMMET LÄSER      WDL2                                       
003600*        PROGRAMMET LÄSER      WDL6                                       
003700*        PROGRAMMET LÄSER      WDB6                                       
003800*                                                                         
003900*        STARTAR BMP W440S4 VIA PGM W00606                                
004000*                                                                         
004100*    INDATA.                                                              
004200*        TRANSAKTION: W4T225                                              
004300*                     W4T225U                                             
004400*        MID:         W4I22501                                            
004500*                                                                         
004600*    UTDATA.                                                              
004700*        MOD:         W4O22501                                            
004800                                                                          
004900     SKIP3                                                                
005000 ENVIRONMENT DIVISION.                                                    
005100                                                                          
005200 DATA DIVISION.                                                           
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500 77  IDPGM                          PIC X(08) VALUE 'W4022500'.           
005600                                                                          
005700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005800 77  FELTEXT                        PIC X(80) VALUE SPACE.                
005900 77  RKOD-FELLOG                    PIC S9(4) COMP VALUE +33.             
006000                                                                          
006100 77  JA                             PIC X     VALUE 'J'.                  
006200 77  NEJ                            PIC X     VALUE 'N'.                  
006300                                                                          
006400 77  CURRENT-SECTION                PIC X(16) VALUE SPACE.                
006500 77  CURRENT-IMS-SECTION            PIC X(16) VALUE SPACE.                
006600 01  -COPY WWDCKONS                                                       
006700                                                                          
006800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006900 77  INDX                           PIC S9(4) VALUE +0  COMP SYNC.        
007000 77  MAX-INDX                       PIC S9(4) VALUE +13 COMP SYNC.        
007100*77  MAX-INDX                       PIC S9(4) VALUE +4  COMP SYNC.        
007200                                                                          
007300 77  AKT-INDX                       PIC S9(4) VALUE +0  COMP SYNC.        
007400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007500                                                                          
007600 77  FLERA-CMD-SW                   PIC X     VALUE 'N'.                  
007700     88  FLERA-CMD                            VALUE 'J'.                  
007800                                                                          
007900 77  P2P-SW                         PIC X     VALUE 'N'.                  
008000     88  P2P                                  VALUE 'J'.                  
008100                                                                          
008200 77  WDA611-SW                      PIC X     VALUE 'N'.                  
008300     88  WDA611-FOUND                         VALUE 'J'.                  
008400                                                                          
008500 77  WDA612-SW                      PIC X     VALUE 'N'.                  
008600     88  WDA612-FOUND                         VALUE 'J'.                  
008700                                                                          
008800 77  WDA613-SW                      PIC X     VALUE 'N'.                  
008900     88  WDA613-FOUND                         VALUE 'J'.                  
009000                                                                          
009100 77  UPDATE-SW                      PIC X     VALUE 'N'.                  
009200     88  UPDATE-OK                            VALUE 'J'.                  
009300                                                                          
009400 77  ANNULL-SW                      PIC X     VALUE 'N'.                  
009500     88  ANNULL-OK                            VALUE 'J'.                  
009600                                                                          
009700 77  INDATA-SW                      PIC X     VALUE 'J'.                  
009800     88  INDATA-OK                            VALUE 'J'.                  
009900     88  INDATA-FEL                           VALUE 'N'.                  
010000                                                                          
010100 77  NYCKLAR-SW                     PIC X     VALUE 'J'.                  
010200     88  NYCKLAR-OK                           VALUE 'J'.                  
010300     88  NYCKLAR-FEL                          VALUE 'N'.                  
010400                                                                          
010500 77  W-IDTRANS                      PIC X(4)  VALUE SPACE.                
010600     88  EGEN-MID                             VALUE '4225'.               
010700     88  GODK-MID                             VALUE '4225' '4275'.        
010800     88  HELP-MID                             VALUE '0551'.               
010900                                                                          
011000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011100 01  GENERELLA-SUBPROGRAM.                                                
011200     03  WMEDKONV                   PIC X(8) VALUE 'WMEDKONV'.            
011300     03  W005INIT                   PIC X(8) VALUE 'W005INIT'.            
011400     03  CBLTDLI                    PIC X(8) VALUE 'CBLTDLI '.            
011500     03  FELLOG                     PIC X(8) VALUE 'FELLOG  '.            
011600     03  ABEND                      PIC X(8) VALUE 'ABEND   '.            
011700     03  CEEISEC                    PIC X(8) VALUE 'CEEISEC '.            
011800*                                                                         
011900     EJECT                                                                
012000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012100*01 -COPY WMEDAREA                                                        
012200     SKIP3                                                                
012300 01  MESSAGE-CODES.                                                       
012400     03  ERR-CORR-HILITE-FLDS       PIC X(3) VALUE '001'.                 
012500     03  INF-PRESS-PF11             PIC X(3) VALUE '003'.                 
012600     03  ERR-KEYS-ARE-MISSING       PIC X(3) VALUE '005'.                 
012700     03  INF-FIRST-PAGE             PIC X(3) VALUE '006'.                 
012800     03  ERR-PF11-AND-NO-DATA       PIC X(3) VALUE '011'.                 
012900     03  ERR-PF9-AND-NO-DATA        PIC X(3) VALUE '???'.                 
013000     03  ERR-WRONG-SELECTION-CODE   PIC X(3) VALUE '416'.                 
013100     03  ERR-PART-MISSING           PIC X(3) VALUE '017'.                 
013200     03  ERR-ENTER-CMD              PIC X(3) VALUE '048'.                 
013300     03  ERR-LINES-MISSING          PIC X(3) VALUE '059'.                 
013400     03  INF-AVSL-ORDER             PIC X(3) VALUE '082'.                 
013500     03  ERR-TWO-FUNCTIONS          PIC X(3) VALUE '097'.                 
013600     03  INF-UPDATE-DONE            PIC X(3) VALUE '101'.                 
013700     03  INF-MORE-INFO-EXISTS       PIC X(3) VALUE '105'.                 
013800     03  INF-LAST-PAGE              PIC X(3) VALUE '106'.                 
013900     03  ERR-LAST-PAGE-SHOWN        PIC X(3) VALUE '115'.                 
014000     03  INF-PRESS-PF9-TO-SPLIT     PIC X(3) VALUE '127'.                 
014100     03  ERR-ALREADY-EXIST          PIC X(3) VALUE '245'.                 
014200     03  ERR-WRONG-KEY              PIC X(3) VALUE '401'.                 
014300     03  INF-WRONG-KEY              PIC X(3) VALUE '749'.                 
014400     EJECT                                                                
014500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014600*                                                                         
014700 01  FILLER                         PIC X(16) VALUE 'WMSGINIT'.           
014800     SKIP3                                                                
014900*01 -COPY WMSGINIT                                                        
015000     EJECT                                                                
015100 77  WS-DIFF                        PIC  9(6) VALUE ZERO.                 
015200 77  WS-WDK6-IDARTNR                PIC S9(9) VALUE ZERO COMP-3.          
015300 77  WS-WDK7-IDARTNR                PIC S9(9) VALUE ZERO COMP-3.          
015400 77  WS-WDK7-IDDC                   PIC X(2)  VALUE SPACE.                
015500 77  WS-ENTRY                       PIC X     VALUE SPACE.                
015600 77  WS-IDARTNR-NUM                 PIC 9(9)  VALUE ZERO.                 
015700 77  WS-IDARTNR-ALPHA               PIC X(8)  VALUE SPACE.                
015800 77  WS-TEVORMRK                    PIC X(2)  VALUE SPACE.                
015900 77  WS-KVRETUR                     PIC S9(9) VALUE ZERO COMP-3.          
016000 77  WS-KVINLEV                     PIC S9(9) VALUE ZERO COMP-3.          
016100 77  W-FIRST-NFST                   PIC X     VALUE SPACE.                
016200 77  W-KOLL-NFST                    PIC X     VALUE SPACE.                
016300 77  W-KOLL-DISTR                   PIC 9(4)  VALUE ZERO.                 
016400 77  W-KOLL-KUND                    PIC 9(6)  VALUE ZERO.                 
016500 01  WS-TIREGDAT-KOD.                                                     
016600     03 WS-TIREGDAT                 PIC 9(6).                             
016700     03 WS-KOD                      PIC X.                                
016800 01  WS-TIREGDAT-D9                 PIC S9(7) COMP-3 VALUE ZERO.          
016900 01  WS-TIREGTID-D9                 PIC S9(7) COMP-3 VALUE ZERO.          
017000                                                                          
017100*    --- PARAMETRAR TILL CEEISEC SUBMODUL                                 
017200 01  FILLER                         PIC X(16) VALUE 'CEEISEC '.           
017300                                                                          
017400 01  YEAR                           PIC S9(9) COMP.                       
017500 01  MONTH                          PIC S9(9) COMP.                       
017600 01  DAYS                           PIC S9(9) COMP.                       
017700 01  HOURS                          PIC S9(9) COMP.                       
017800 01  MINUTES                        PIC S9(9) COMP.                       
017900 01  SECONDS                        PIC S9(9) COMP.                       
018000 01  MILLSEC                        PIC S9(9) COMP.                       
018100 01  OUTSEC                         COMP-2.                               
018200 01  FBC.                                                                 
018300     03 SEV          PIC S9(4) BINARY.                                    
018400     03 MSGNO        PIC S9(4) BINARY.                                    
018500     03 FILLER       PIC X(8).                                            
018600                                                                          
018700 01  OUTSEC-SVAR                    COMP-2.                               
018800 01  OUTSEC-NU                      COMP-2.                               
018900                                                                          
019000 01  FILLER                         PIC X(16) VALUE 'P5-DATTID'.          
019100 01  WS-DAT                         PIC 9(6).                             
019200 01  FILLER REDEFINES WS-DAT.                                             
019300     03 WS-YEAR                     PIC 9(2).                             
019400     03 WS-MONTH                    PIC 9(2).                             
019500     03 WS-DAYS                     PIC 9(2).                             
019600 01  WS-TID                         PIC 9(6).                             
019700 01  FILLER REDEFINES WS-TID.                                             
019800     03 WS-HOURS                    PIC 9(2).                             
019900     03 WS-MINUTES                  PIC 9(2).                             
020000     03 WS-SECONDS                  PIC 9(2).                             
020100                                                                          
020200 01  WS-NU-TID                      PIC 9(8).                             
020300 01  FILLER REDEFINES WS-NU-TID.                                          
020400     03 WS-NU-TID-6                 PIC 9(6).                             
020500     03 FILLER                      PIC 9(2).                             
020600                                                                          
020700*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
020800 01  SPAR-AREA.                                                           
020900     03  SPAR-IDTRANS                      PIC X(4)  VALUE '4225'.        
021000     03  SPAR-ENTRY                        PIC X     VALUE SPACE.         
021100     03  SPAR-NYCKLAR-ENTER                PIC X(21) VALUE SPACE.         
021200     03  SPAR-WDA6CSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
021300         05  SPAR-IDROLL-CSEQ-ENTER        PIC X(5).                      
021400         05  SPAR-TIREGDAT-AVV9-CSEQ-ENTER PIC S9(7) COMP-3.              
021500         05  SPAR-TIREGTID-AVV9-CSEQ-ENTER PIC S9(9) COMP-3.              
021600                                                                          
021700     03  SPAR-WDA6DSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
021800         05  SPAR-IDROLL-DSEQ-ENTER        PIC X(5).                      
021900         05  SPAR-IDARTNR-DSEQ-ENTER       PIC S9(9) COMP-3.              
022000         05  SPAR-TIREGDAT-AVV9-DSEQ-ENTER PIC S9(7) COMP-3.              
022100         05  SPAR-TIREGTID-AVV9-DSEQ-ENTER PIC S9(9) COMP-3.              
022200                                                                          
022300     03  SPAR-WDA6ESEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
022400         05  SPAR-IDDISTR-ESEQ-ENTER       PIC S9(5) COMP-3.              
022500         05  SPAR-TIREGDAT-AVV9-ESEQ-ENTER PIC S9(7) COMP-3.              
022600         05  SPAR-TIREGTID-AVV9-ESEQ-ENTER PIC S9(9) COMP-3.              
022700                                                                          
022800     03  SPAR-WDA6FSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
022900         05  SPAR-IDDISTR-FSEQ-ENTER       PIC S9(5) COMP-3.              
023000         05  SPAR-IDKUNDNR-FSEQ-ENTER      PIC S9(7) COMP-3.              
023100         05  SPAR-TIREGDAT-AVV9-FSEQ-ENTER PIC S9(7) COMP-3.              
023200         05  SPAR-TIREGTID-AVV9-FSEQ-ENTER PIC S9(9) COMP-3.              
023300                                                                          
023400     03  SPAR-WDA6GSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
023500         05  SPAR-IDDISTR-GSEQ-ENTER       PIC S9(5) COMP-3.              
023600         05  SPAR-IDKUNDNR-GSEQ-ENTER      PIC S9(7) COMP-3.              
023700         05  SPAR-IDARTNR-GSEQ-ENTER       PIC S9(9) COMP-3.              
023800         05  SPAR-TIREGDAT-AVV9-GSEQ-ENTER PIC S9(7) COMP-3.              
023900         05  SPAR-TIREGTID-AVV9-GSEQ-ENTER PIC S9(9) COMP-3.              
024000                                                                          
024100     03  SPAR-WDA6HSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
024200         05  SPAR-IDDISTR-HSEQ-ENTER       PIC S9(5) COMP-3.              
024300         05  SPAR-IDARTNR-HSEQ-ENTER       PIC S9(9) COMP-3.              
024400         05  SPAR-TIREGDAT-AVV9-HSEQ-ENTER PIC S9(7) COMP-3.              
024500         05  SPAR-TIREGTID-AVV9-HSEQ-ENTER PIC S9(9) COMP-3.              
024600                                                                          
024700     03  SPAR-WDA6ISEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
024800         05  SPAR-IDANSK-ISEQ-ENTER        PIC S9(3) COMP-3.              
024900         05  SPAR-TIREGDAT-AVV9-ISEQ-ENTER PIC S9(7) COMP-3.              
025000         05  SPAR-TIREGTID-AVV9-ISEQ-ENTER PIC S9(9) COMP-3.              
025100                                                                          
025200     03  SPAR-WDA6JSEQ-ENTER REDEFINES SPAR-NYCKLAR-ENTER.                
025300         05  SPAR-IDARTNR-JSEQ-ENTER       PIC S9(9) COMP-3.              
025400         05  SPAR-TIREGDAT-AVV9-JSEQ-ENTER PIC S9(7) COMP-3.              
025500         05  SPAR-TIREGTID-AVV9-JSEQ-ENTER PIC S9(9) COMP-3.              
025600                                                                          
025700     03  SPAR-NYCKLAR-NEXT        PIC X(21)   VALUE SPACE.                
025800     03  SPAR-WDA6CSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
025900         05  SPAR-IDROLL-CSEQ-NEXT         PIC X(5).                      
026000         05  SPAR-TIREGDAT-AVV9-CSEQ-NEXT  PIC S9(7) COMP-3.              
026100         05  SPAR-TIREGTID-AVV9-CSEQ-NEXT  PIC S9(9) COMP-3.              
026200                                                                          
026300     03  SPAR-WDA6DSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
026400         05  SPAR-IDROLL-DSEQ-NEXT         PIC X(5).                      
026500         05  SPAR-IDARTNR-DSEQ-NEXT        PIC S9(9) COMP-3.              
026600         05  SPAR-TIREGDAT-AVV9-DSEQ-NEXT  PIC S9(7) COMP-3.              
026700         05  SPAR-TIREGTID-AVV9-DSEQ-NEXT  PIC S9(9) COMP-3.              
026800                                                                          
026900     03  SPAR-WDA6ESEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
027000         05  SPAR-IDDISTR-ESEQ-NEXT        PIC S9(5) COMP-3.              
027100         05  SPAR-TIREGDAT-AVV9-ESEQ-NEXT  PIC S9(7) COMP-3.              
027200         05  SPAR-TIREGTID-AVV9-ESEQ-NEXT  PIC S9(9) COMP-3.              
027300                                                                          
027400     03  SPAR-WDA6FSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
027500         05  SPAR-IDDISTR-FSEQ-NEXT        PIC S9(5) COMP-3.              
027600         05  SPAR-IDKUNDNR-FSEQ-NEXT       PIC S9(7) COMP-3.              
027700         05  SPAR-TIREGDAT-AVV9-FSEQ-NEXT  PIC S9(7) COMP-3.              
027800         05  SPAR-TIREGTID-AVV9-FSEQ-NEXT  PIC S9(9) COMP-3.              
027900                                                                          
028000     03  SPAR-WDA6GSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
028100         05  SPAR-IDDISTR-GSEQ-NEXT        PIC S9(5) COMP-3.              
028200         05  SPAR-IDKUNDNR-GSEQ-NEXT       PIC S9(7) COMP-3.              
028300         05  SPAR-IDARTNR-GSEQ-NEXT        PIC S9(9) COMP-3.              
028400         05  SPAR-TIREGDAT-AVV9-GSEQ-NEXT  PIC S9(7) COMP-3.              
028500         05  SPAR-TIREGTID-AVV9-GSEQ-NEXT  PIC S9(9) COMP-3.              
028600                                                                          
028700     03  SPAR-WDA6HSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
028800         05  SPAR-IDDISTR-HSEQ-NEXT        PIC S9(5) COMP-3.              
028900         05  SPAR-IDARTNR-HSEQ-NEXT        PIC S9(9) COMP-3.              
029000         05  SPAR-TIREGDAT-AVV9-HSEQ-NEXT  PIC S9(7) COMP-3.              
029100         05  SPAR-TIREGTID-AVV9-HSEQ-NEXT  PIC S9(9) COMP-3.              
029200                                                                          
029300     03  SPAR-WDA6ISEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
029400         05  SPAR-IDANSK-ISEQ-NEXT         PIC S9(3) COMP-3.              
029500         05  SPAR-TIREGDAT-AVV9-ISEQ-NEXT  PIC S9(7) COMP-3.              
029600         05  SPAR-TIREGTID-AVV9-ISEQ-NEXT  PIC S9(9) COMP-3.              
029700                                                                          
029800     03  SPAR-WDA6JSEQ-NEXT  REDEFINES SPAR-NYCKLAR-NEXT.                 
029900         05  SPAR-IDARTNR-JSEQ-NEXT        PIC S9(9) COMP-3.              
030000         05  SPAR-TIREGDAT-AVV9-JSEQ-NEXT  PIC S9(7) COMP-3.              
030100         05  SPAR-TIREGTID-AVV9-JSEQ-NEXT  PIC S9(9) COMP-3.              
030200                                                                          
030300     03  SPAR-DAT-TID-AVV-RAD OCCURS 13.                                  
030400         05  SPAR-TIREGDAT-AVV-RAD         PIC S9(7) COMP-3.              
030500         05  SPAR-TIREGTID-AVV-RAD         PIC S9(9) COMP-3.              
030600     03  SPAR-DAT-TID-URSP-RAD OCCURS 13.                                 
030700         05  SPAR-TIREGDAT-URSP-RAD        PIC S9(7) COMP-3.              
030800         05  SPAR-TIREGTID-URSP-RAD        PIC S9(9) COMP-3.              
030900     EJECT                                                                
031000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
031100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
031200     SKIP3                                                                
031300*01  MID -COPY W4I22501                                                   
031400     EJECT                                                                
031500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
031600     SKIP3                                                                
031700*01  -COPY WMSGAREA                                                       
031800     EJECT                                                                
031900*    --- FÖR HOPP TILL 4221-FÖRBI/VOR-ORDER                               
032000       05  4221-MID REDEFINES MSG-MID-OUT.                                
032100*          07  -COPY W4I22101 -PRE 4221-MID-                              
032200*    --- FÖR HOPP TILL 4231-SPECIAL-ORDER                                 
032300       05  4231-MID REDEFINES MSG-MID-OUT.                                
032400*          07  -COPY W4I23101 -PRE 4231-                                  
032500*    --- FÖR HOPP TILL 4275-TEXTBILD                                      
032600       05  4275-MID REDEFINES MSG-MID-OUT.                                
032700*          07  -COPY W4I27501 -PRE 4275-                                  
032800     03  MOD REDEFINES MSG-AREA.                                          
032900*      05  -COPY W4O22501                                                 
033000     EJECT                                                                
033100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
033200     SKIP3                                                                
033300*01  -COPY WMFSAREA                                                       
033400                                                                          
033500 01  W-PROG-TO-PROG-SW.                                                   
033600*  03    -COPY WMSGSOP                                                    
033700     EJECT                                                                
033800                                                                          
033900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
034000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
034100     SKIP3                                                                
034200 01  NYCKLAR-TILL-DLI.                                                    
034300                                                                          
034400      03 W-WDP501KY-X.                                                    
034500         05  W-IDSKYLT               PIC X(3)  VALUE SPACE.               
034600         05  W-IDDOKTYP              PIC X(8)  VALUE SPACE.               
034700         05  W-IDDOK                 PIC X(8)  VALUE SPACE.               
034800                                                                          
034900     03  W-IDDC-X.                                                        
035000         05  W-IDDC                  PIC X(2)  VALUE SPACE.               
035100                                                                          
035200*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
035300     03  W-WDA601KY-X.                                                    
035400         05  W-IDDISTR-A601          PIC S9(5) VALUE ZERO COMP-3.         
035500         05  W-IDKUNDNR-A601         PIC S9(7) VALUE ZERO COMP-3.         
035600         05  W-IDKUNDRF-A601         PIC X(10) VALUE SPACE.               
035700         05  W-TIREGDAT-URSP-A601    PIC S9(7) VALUE ZERO COMP-3.         
035800         05  W-IDARTNR-X.                                                 
035900             07 W-IDARTNR            PIC S9(9) VALUE ZERO COMP-3.         
036000         05  W-TIREGTID-URSP-A601    PIC S9(9) VALUE ZERO COMP-3.         
036100         05  W-TIREGDAT-AVV-A601     PIC S9(7) VALUE ZERO COMP-3.         
036200         05  W-TIREGTID-AVV-A601     PIC S9(9) VALUE ZERO COMP-3.         
036300                                                                          
036400     03  W-WDA611KY-X.                                                    
036500         05  W-KDSEGKEY-WDA611       PIC X     VALUE '1'.                 
036600                                                                          
036700     03  W-WDA612KY-X.                                                    
036800         05  W-KDSEGKEY-WDA612       PIC X     VALUE '1'.                 
036900                                                                          
037000     03  W-WDA613KY-X.                                                    
037100         05  W-KDSEGKEY-WDA613       PIC X     VALUE '1'.                 
037200                                                                          
037300     03  W-WDA6CSEQ-MIN-X.                                                
037400         05  W-IDROLL-CSEQ-MIN       PIC X(5)  VALUE SPACE.               
037500         05  FILLER                  PIC X(9)  VALUE LOW-VALUE.           
037600                                                                          
037700     03  W-WDA6CSEQ-MAX-X.                                                
037800         05  W-IDROLL-CSEQ-MAX       PIC X(5)  VALUE SPACE.               
037900         05  FILLER                  PIC X(9)  VALUE HIGH-VALUE.          
038000                                                                          
038100     03  W-WDA6DSEQ-MIN-X.                                                
038200         05  W-IDROLL-DSEQ-MIN       PIC X(5)  VALUE SPACE.               
038300         05  W-IDARTNR-DSEQ-MIN      PIC S9(9) VALUE ZERO COMP-3.         
038400*        05  FILLER                  PIC X(9)  VALUE LOW-VALUE.           
038500         05  W-DSEQ-MIN-TID          PIC X(9)  VALUE LOW-VALUE.           
038600         05  FILLER REDEFINES W-DSEQ-MIN-TID.                             
038700             07 W-TIREGDAT-DSEQ-MIN  PIC S9(7) COMP-3.                    
038800             07 W-TIREGTID-DSEQ-MIN  PIC S9(9) COMP-3.                    
038900                                                                          
039000     03  W-WDA6DSEQ-MAX-X.                                                
039100         05  W-IDROLL-DSEQ-MAX       PIC X(5)  VALUE SPACE.               
039200         05  W-IDARTNR-DSEQ-MAX      PIC S9(9) VALUE ZERO COMP-3.         
039300         05  FILLER                  PIC X(9)  VALUE HIGH-VALUE.          
039400                                                                          
039500     03  W-WDA6ESEQ-MIN-X.                                                
039600         05  W-IDDISTR-ESEQ-MIN      PIC S9(5) VALUE ZERO COMP-3.         
039700         05  FILLER                  PIC X(9)  VALUE LOW-VALUE.           
039800                                                                          
039900     03  W-WDA6ESEQ-MAX-X.                                                
040000         05  W-IDDISTR-ESEQ-MAX      PIC S9(5) VALUE ZERO COMP-3.         
040100         05  FILLER                  PIC X(9)  VALUE HIGH-VALUE.          
040200                                                                          
040300     03  W-WDA6FSEQ-MIN-X.                                                
040400         05  W-IDDISTR-FSEQ-MIN      PIC S9(5) VALUE ZERO COMP-3.         
040500         05  W-IDKUNDNR-FSEQ-MIN     PIC S9(7) VALUE ZERO COMP-3.         
040600         05  FILLER                  PIC X(9)  VALUE LOW-VALUE.           
040700                                                                          
040800     03  W-WDA6FSEQ-MAX-X.                                                
040900         05  W-IDDISTR-FSEQ-MAX      PIC S9(5) VALUE ZERO COMP-3.         
041000         05  W-IDKUNDNR-FSEQ-MAX     PIC S9(7) VALUE ZERO COMP-3.         
041100         05  FILLER                  PIC X(9)  VALUE HIGH-VALUE.          
041200                                                                          
041300     03  W-WDA6GSEQ-MIN-X.                                                
041400         05  W-IDDISTR-GSEQ-MIN      PIC S9(5) VALUE ZERO COMP-3.         
041500         05  W-IDKUNDNR-GSEQ-MIN     PIC S9(7) VALUE ZERO COMP-3.         
041600         05  W-IDARTNR-GSEQ-MIN      PIC S9(9) VALUE ZERO COMP-3.         
041700         05  FILLER                  PIC X(9)  VALUE LOW-VALUE.           
041800                                                                          
041900     03  W-WDA6GSEQ-MAX-X.                                                
042000         05  W-IDDISTR-GSEQ-MAX      PIC S9(5) VALUE ZERO COMP-3.         
042100         05  W-IDKUNDNR-GSEQ-MAX     PIC S9(7) VALUE ZERO COMP-3.         
042200         05  W-IDARTNR-GSEQ-MAX      PIC S9(9) VALUE ZERO COMP-3.         
042300         05  FILLER                  PIC X(9)  VALUE HIGH-VALUE.          
042400                                                                          
042500     03  W-WDA6HSEQ-MIN-X.                                                
042600         05  W-IDDISTR-HSEQ-MIN      PIC S9(5) VALUE ZERO COMP-3.         
042700         05  W-IDARTNR-HSEQ-MIN      PIC S9(9) VALUE ZERO COMP-3.         
042800         05  FILLER                  PIC X(9)  VALUE LOW-VALUE.           
042900                                                                          
043000     03  W-WDA6HSEQ-MAX-X.                                                
043100         05  W-IDDISTR-HSEQ-MAX      PIC S9(5) VALUE ZERO COMP-3.         
043200         05  W-IDARTNR-HSEQ-MAX      PIC S9(9) VALUE ZERO COMP-3.         
043300         05  FILLER                  PIC X(9)  VALUE HIGH-VALUE.          
043400                                                                          
043500     03  W-WDA6ISEQ-MIN-X.                                                
043600         05  W-IDANSK-ISEQ-MIN       PIC S9(3) VALUE ZERO COMP-3.         
043700         05  FILLER                  PIC X(9)  VALUE LOW-VALUE.           
043800                                                                          
043900     03  W-WDA6ISEQ-MAX-X.                                                
044000         05  W-IDANSK-ISEQ-MAX       PIC S9(3) VALUE ZERO COMP-3.         
044100         05  FILLER                  PIC X(9)  VALUE HIGH-VALUE.          
044200                                                                          
044300     03  W-WDA6JSEQ-MIN-X.                                                
044400         05  W-IDARTNR-JSEQ-MIN      PIC S9(9) VALUE ZERO COMP-3.         
044500         05  FILLER                  PIC X(9)  VALUE LOW-VALUE.           
044600                                                                          
044700     03  W-WDA6JSEQ-MAX-X.                                                
044800         05  W-IDARTNR-JSEQ-MAX      PIC S9(9) VALUE ZERO COMP-3.         
044900         05  FILLER                  PIC X(9)  VALUE HIGH-VALUE.          
045000                                                                          
045100     03  W-IDARTNR-K6-X.                                                  
045200         05  W-IDARTNR-K6            PIC S9(9) VALUE ZERO COMP-3.         
045300                                                                          
045400     03  W-IDDC-B6-X.                                                     
045500         05 W-IDDC-B6                PIC X(2).                            
045600                                                                          
045700     03  W-WDD901KY-X.                                                    
045800         05  W-IDARTNR-D9            PIC S9(9) VALUE ZERO COMP-3.         
045900         05  W-IDDC-D9               PIC X(2) VALUE SPACE.                
046000     03  W-IDLEVNR-X.                                                     
046100         05  W-IDLEVNR               PIC  X(5) VALUE SPACE.               
046200                                                                          
046300*    --- STATUS-KOD FRÅN IMS                                              
046400 01  STATUS-WS                   PIC XX.                                  
046500     88  SEGMENT-FOUND                       VALUE '  '.                  
046600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
046700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
046800     SKIP2                                                                
046900 01  GODK-STATUSKODER.                                                    
047000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
047100     SKIP3                                                                
047200 01  ALL-SSA.                                                             
047300     03 SSA1                     PIC X(96).                               
047400     03 SSA2                     PIC X(64).                               
047500     EJECT                                                                
047600*    --- IMS FUNKTIONSKODER                                               
047700*01  -COPY W0003                                                          
047800     EJECT                                                                
047900*    ---  DLI INPUT-OUTPUT AREA                                           
048000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA601'.                      
048100 01  DLI-IO-WDA601.                                                       
048200*    03  -COPY WDA601                                                     
048300     EJECT                                                                
048400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA611'.                      
048500 01  DLI-IO-WDA611.                                                       
048600*    03  -COPY WDA611                                                     
048700     EJECT                                                                
048800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA612'.                      
048900 01  DLI-IO-WDA612.                                                       
049000*    03  -COPY WDA612                                                     
049100     EJECT                                                                
049200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA613'.                      
049300 01  DLI-IO-WDA613.                                                       
049400*    03  -COPY WDA613                                                     
049500     EJECT                                                                
049600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
049700 01  DLI-IO-WDK611.                                                       
049800*    03  -COPY WDK611                                                     
049900     EJECT                                                                
050000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
050100 01  DLI-IO-WDK711.                                                       
050200*    03  -COPY WDK711                                                     
050300     EJECT                                                                
050400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
050500 01  DLI-IO-WDK901.                                                       
050600*    03  -COPY WDK901                                                     
050700     EJECT                                                                
050800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP501'.                      
050900 01  DLI-IO-WDP501.                                                       
051000*    03  -COPY WDP501                                                     
051100     EJECT                                                                
051200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR601'.                      
051300 01  DLI-IO-WDR601.                                                       
051400*    03  -COPY WDR601                                                     
051500       05  -COPY W414205A -RED FIL-WDR601-DATA                            
051600     EJECT                                                                
051700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
051800 01  DLI-IO-WDL201.                                                       
051900*    03  -COPY WDL201  -PRE L2-                                           
052000                                                                          
052100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL221'.                      
052200 01  DLI-IO-WDL221.                                                       
052300*    03  -COPY WDL221  -PRE L2-                                           
052400                                                                          
052500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
052600 01  DLI-IO-WDL601.                                                       
052700*    03  -COPY WDL601  -PRE L6-                                           
052800                                                                          
052900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
053000 01  DLI-IO-WDL611.                                                       
053100*    03  -COPY WDL611  -PRE L6-                                           
053200                                                                          
053300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
053400 01   DLI-IO-AREA-B601.                                                   
053500*     03  -COPY WDB601                                                    
053600                                                                          
053700 01  FILLER               PIC X(16)   VALUE 'WDD902 AREA'.                
053800 01   DLI-IO-AREA-D902.                                                   
053900*     03  -COPY WDD902                                                    
054000                                                                          
054100 01  FILLER               PIC X(16)   VALUE 'WDD924 AREA'.                
054200 01   DLI-IO-AREA-D924.                                                   
054300*     03  -COPY WDD924  -PRE D9-                                          
054400                                                                          
054500 01  FILLER               PIC X(16)   VALUE 'WDD925 AREA'.                
054600 01   DLI-IO-AREA-D925.                                                   
054700*     03  -COPY WDD925  -PRE D9-                                          
054800                                                                          
054900                                                                          
055000 LINKAGE SECTION.                                                         
055100*01  -COPY W0009  -PRE MSG-                                               
055200*01  -COPY W0009  -PRE ALT-                                               
055300*01  -COPY W0009  -PRE 4221-                                              
055400*01  -COPY W0009  -PRE 4231-                                              
055500*01  -COPY W0009  -PRE 4275-                                              
055600*01  -COPY W0008  -PRE WDP7-                                              
055700     05  FILLER                  PIC X.                                   
055800*01  -COPY W0008  -PRE WDA6-                                              
055900     05  FILLER                  PIC X.                                   
056000*01  -COPY W0008  -PRE WDA6C-                                             
056100     05  FILLER                  PIC X.                                   
056200*01  -COPY W0008  -PRE WDA6D-                                             
056300     05  FILLER                  PIC X.                                   
056400*01  -COPY W0008  -PRE WDA6E-                                             
056500     05  FILLER                  PIC X.                                   
056600*01  -COPY W0008  -PRE WDA6F-                                             
056700     05  FILLER                  PIC X.                                   
056800*01  -COPY W0008  -PRE WDA6G-                                             
056900     05  FILLER                  PIC X.                                   
057000*01  -COPY W0008  -PRE WDA6H-                                             
057100     05  FILLER                  PIC X.                                   
057200*01  -COPY W0008  -PRE WDA6I-                                             
057300     05  FILLER                  PIC X.                                   
057400*01  -COPY W0008  -PRE WDA6J-                                             
057500     05  FILLER                  PIC X.                                   
057600*01  -COPY W0008  -PRE WDK6-                                              
057700     05  FILLER                  PIC X.                                   
057800*01  -COPY W0008  -PRE WDK7-                                              
057900     05  FILLER                  PIC X.                                   
058000*01  -COPY W0008  -PRE WDK9-                                              
058100     05  FILLER                  PIC X.                                   
058200*01  -COPY W0008  -PRE WDP5-                                              
058300     05  FILLER                  PIC X.                                   
058400*01  -COPY W0008  -PRE WDR6-                                              
058500     05  FILLER                  PIC X.                                   
058600     EJECT                                                                
058700*01  -COPY W0008  -PRE WDL2-                                              
058800     05  FILLER                  PIC X.                                   
058900     EJECT                                                                
059000*01  -COPY W0008  -PRE WDL6-                                              
059100     05  FILLER                  PIC X.                                   
059200     EJECT                                                                
059300*01  -COPY W0008  -PRE WDB6-                                              
059400     05  FILLER                  PIC X.                                   
059500*01  -COPY W0008  -PRE WDD9-                                              
059600     05  FILLER                  PIC X.                                   
059700     EJECT                                                                
059800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
059900                 4221-PCB 4231-PCB 4275-PCB                               
060000                 WDP7-PCB WDA6-PCB WDA6C-PCB WDA6D-PCB WDA6E-PCB          
060100                 WDA6F-PCB WDA6G-PCB WDA6H-PCB WDA6I-PCB WDA6J-PCB        
060200                 WDK6-PCB WDK7-PCB WDK9-PCB WDP5-PCB WDR6-PCB             
060300                 WDL2-PCB WDL6-PCB WDB6-PCB WDD9-PCB.                     
060400                                                                          
060500 MAIN SECTION.                                                            
060600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
060700                 4221-PCB 4231-PCB 4275-PCB                               
060800                 WDP7-PCB WDA6-PCB WDA6C-PCB WDA6D-PCB WDA6E-PCB          
060900                 WDA6F-PCB WDA6G-PCB WDA6H-PCB WDA6I-PCB WDA6J-PCB        
061000                 WDK6-PCB WDK7-PCB WDK9-PCB WDP5-PCB WDR6-PCB             
061100                 WDL2-PCB WDL6-PCB WDB6-PCB WDD9-PCB.                     
061200                                                                          
061300     PERFORM IMS-GET-MSG                                                  
061400     IF SEGMENT-FOUND                                                     
061500       PERFORM A-INIT                                                     
061600       PERFORM B-CHECK-KEYS                                               
061700       IF NYCKLAR-OK                                                      
061800         IF MFS-UPDATE                                                    
061900           PERFORM G-CHECK-INPUT                                          
062000           IF INDATA-OK                                                   
062100             PERFORM H-UPDATE                                             
062200           END-IF                                                         
062300         ELSE                                                             
062400           IF MFS-UPD-V                                                   
062500             PERFORM K-CREATE-REPORT                                      
062600             MOVE 'BMP STARTED' TO MOD-TEMFSINF                           
062700           ELSE                                                           
062800             IF MFS-FIRST                                                 
062900               PERFORM C-FIRST-PAGE                                       
063000             ELSE                                                         
063100               IF MFS-NEXT                                                
063200                 PERFORM D-NEXT-PAGE                                      
063300               ELSE                                                       
063400                 IF MFS-SPLIT                                             
063500                   PERFORM J-CHECK-INPUT                                  
063600                   IF INDATA-OK                                           
063700                     PERFORM I-SPLIT-SCREEN                               
063800                   END-IF                                                 
063900                 ELSE                                                     
064000                   PERFORM E-SAME-PAGE                                    
064100                 END-IF                                                   
064200               END-IF                                                     
064300             END-IF                                                       
064400           END-IF                                                         
064500         END-IF                                                           
064600         IF INDATA-OK AND NOT P2P                                         
064700           PERFORM F-READ-SHOW-INFO                                       
064800         END-IF                                                           
064900       END-IF                                                             
065000                                                                          
065100       IF NOT P2P                                                         
065200         PERFORM IMS-INSERT-MSG                                           
065300       END-IF                                                             
065400     END-IF                                                               
065500                                                                          
065600     MOVE ZERO TO RETURN-CODE                                             
065700     GOBACK                                                               
065800     .                                                                    
065900     EJECT                                                                
066000 A-INIT SECTION.                                                          
066100     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
066200                                                                          
066300     IF MSG-DUBBLA-TRANSKODER                                             
066400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I22501                 
066500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
066600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
066700     ELSE                                                                 
066800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I22501                  
066900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
067000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
067100     END-IF                                                               
067200                                                                          
067300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
067400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
067500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
067600                                                                          
067700     MOVE LOW-VALUE TO MSG-AREA                                           
067800     MOVE 'W4O225N1' TO MFS-IDMOD                                         
067900     MOVE '4225' TO MOD-IDTRANS                                           
068000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
068100                                                                          
068200     IF EGEN-MID OR HELP-MID                                              
068300       CONTINUE                                                           
068400     ELSE                                                                 
068500       MOVE SPACE TO MFS-KDTRTYP                                          
068600       MOVE '7' TO MFS-IDPFK                                              
068700     END-IF                                                               
068800                                                                          
068900     IF W-IDTRANS = '4275'                                                
069000       MOVE SPACE TO MFS-IDPFK                                            
069100       MOVE ALL '+' TO MID-W4I22501                                       
069200     END-IF                                                               
069300                                                                          
069400     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O22501 + 4                        
069500                                                                          
069600*    --- DAGENS DATUM + TID IN I CEEISEC                                  
069700     ACCEPT WS-DAT    FROM  DATE                                          
069800     ACCEPT WS-NU-TID FROM  TIME                                          
069900     MOVE WS-NU-TID-6 TO WS-TID                                           
070000     PERFORM S01-CALL-CEEISEC                                             
070100     MOVE OUTSEC-SVAR TO OUTSEC-NU                                        
070200     .                                                                    
070300     EJECT                                                                
070400 B-CHECK-KEYS SECTION.                                                    
070500     MOVE 'B-CHECK-KEYS    ' TO CURRENT-SECTION                           
070600                                                                          
070700     MOVE ALL '+' TO MSGI-WMSGINIT                                        
070800     MOVE '001' TO MSGI-KDCALL                                            
070900     MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                             
071000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
071100     MOVE '4225' TO MSGI-IDTRANS                                          
071200     IF EGEN-MID                                                          
071300       MOVE MID-IDROLL-IN   TO MSGI-IDROLL                                
071400       MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                               
071500       MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                              
071600       MOVE MID-IDANSK-IN   TO MSGI-IDANSK                                
071700       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
071800       MOVE MID-KDSORT-IN   TO MSGI-KDSORT                                
071900       MOVE MID-TEVORMRK-IN TO MSGI-TEVORMRK                              
072000     END-IF                                                               
072100     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
072200                                                                          
072300     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
072400     MOVE JA TO NYCKLAR-SW                                                
072500                                                                          
072600*    --- KOLLA IDROLL                                                     
072700     MOVE MFS-RENSA-FAELT TO MOD-IDROLL-IN                                
072800     IF MID-IDROLL-IN  NOT = ALL '+'                                      
072900       MOVE '7' TO MFS-IDPFK                                              
073000       MOVE SPACE TO MFS-KDTRTYP                                          
073100     END-IF                                                               
073200                                                                          
073300*    --- KOLLA IDDISTR                                                    
073400     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
073500     IF MID-IDDISTR-IN NOT = ALL '+'                                      
073600       MOVE '7' TO MFS-IDPFK                                              
073700       MOVE SPACE TO MFS-KDTRTYP                                          
073800     END-IF                                                               
073900                                                                          
074000*    --- KOLLA IDKUNDNR                                                   
074100     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
074200     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
074300       MOVE '7' TO MFS-IDPFK                                              
074400       MOVE SPACE TO MFS-KDTRTYP                                          
074500     END-IF                                                               
074600                                                                          
074700*    --- KOLLA IDANSK                                                     
074800     MOVE MFS-RENSA-FAELT TO MOD-IDANSK-IN                                
074900     IF MID-IDANSK-IN  NOT = ALL '+'                                      
075000       MOVE '7' TO MFS-IDPFK                                              
075100       MOVE SPACE TO MFS-KDTRTYP                                          
075200     END-IF                                                               
075300                                                                          
075400*    --- KOLLA IDARTNR                                                    
075500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
075600     IF MID-IDARTNR-IN NOT = ALL '+'                                      
075700       MOVE '7' TO MFS-IDPFK                                              
075800       MOVE SPACE TO MFS-KDTRTYP                                          
075900     END-IF                                                               
076000                                                                          
076100*    --- KOLLA KDSORT                                                     
076200     MOVE MFS-RENSA-FAELT TO MOD-KDSORT-IN                                
076300     IF MID-KDSORT-IN NOT = ALL '+'                                       
076400       MOVE '7' TO MFS-IDPFK                                              
076500       MOVE SPACE TO MFS-KDTRTYP                                          
076600     END-IF                                                               
076700                                                                          
076800*    --- KOLLA TEVORMRK                                                   
076900     MOVE MFS-RENSA-FAELT TO MOD-TEVORMRK-IN                              
077000     IF MID-TEVORMRK-IN NOT = ALL '+'                                     
077100       MOVE '7' TO MFS-IDPFK                                              
077200       MOVE SPACE TO MFS-KDTRTYP                                          
077300*      MOVE MID-TEVORMRK-IN TO WS-TEVORMRK                                
077400*    ELSE                                                                 
077500*      MOVE SPACE           TO WS-TEVORMRK                                
077600     END-IF                                                               
077700                                                                          
077800     IF MSGI-IDDISTR = SPACE OR                                           
077900       (MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO)                     
078010       CONTINUE                                                           
078100     ELSE                                                                 
078200       MOVE NEJ TO NYCKLAR-SW                                             
078300     END-IF                                                               
078400                                                                          
078500     IF MSGI-IDKUNDNR = SPACE OR                                          
078600        MSGI-IDKUNDNR NUMERIC                                             
078700       CONTINUE                                                           
078800     ELSE                                                                 
078900       MOVE NEJ TO NYCKLAR-SW                                             
079000     END-IF                                                               
079100                                                                          
079200     IF MSGI-IDANSK = SPACE OR                                            
079300       (MSGI-IDANSK NUMERIC AND MSGI-IDANSK > ZERO)                       
079400       CONTINUE                                                           
079500     ELSE                                                                 
079600       MOVE NEJ TO NYCKLAR-SW                                             
079700     END-IF                                                               
079800                                                                          
079900     IF MSGI-IDARTNR = SPACE OR                                           
080000       (MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO)                     
080100       CONTINUE                                                           
080200     ELSE                                                                 
080300       MOVE NEJ TO NYCKLAR-SW                                             
080400     END-IF                                                               
080500                                                                          
080600     IF MSGI-KDSORT = SPACE OR 'PA'                                       
080700       CONTINUE                                                           
080800     ELSE                                                                 
080900       MOVE NEJ TO NYCKLAR-SW                                             
081000     END-IF                                                               
081100                                                                          
081200*                                                                         
081300     IF MID-KDSORT-IN = 'PA'                                              
081400        IF MID-IDDISTR-IN = ALL '+' AND                                   
081500           MID-IDKUNDNR-IN = ALL '+' AND                                  
081600           MID-IDANSK-IN = ALL '+'                                        
081700           CONTINUE                                                       
081800        ELSE                                                              
081900           MOVE NEJ TO NYCKLAR-SW                                         
082000        END-IF                                                            
082100     END-IF                                                               
082200                                                                          
082300     IF NYCKLAR-FEL                                                       
082400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
082500       CALL WMEDKONV USING MED-WMEDAREA                                   
082600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
082700       PERFORM MFS-RENSA-FAELT-IN                                         
082800       PERFORM MFS-RENSA-FAELT-UT                                         
082900     ELSE                                                                 
083000       IF GODK-MID                                                        
083100         PERFORM BA-DECIDE-ENTRY                                          
083200       ELSE                                                               
083300         PERFORM BB-DEFAULT-ENTRY                                         
083400       END-IF                                                             
083500     END-IF                                                               
083600                                                                          
083700     IF GODK-MID OR NYCKLAR-OK                                            
083800       MOVE MSGI-IDROLL TO MOD-IDROLL-UT                                  
083900       MOVE MSGI-IDDISTR TO MOD-IDDISTR-UT                                
084000       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
084100       MOVE MSGI-IDKUNDNR TO MOD-IDKUNDNR-UT                              
084200       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
084300       MOVE MSGI-IDANSK TO MOD-IDANSK-UT                                  
084400       INSPECT MOD-IDANSK-UT REPLACING LEADING ZERO BY SPACE              
084500       MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                                
084600       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
084700       MOVE MSGI-KDSORT TO MOD-KDSORT-UT                                  
084800       MOVE MSGI-TEVORMRK TO MOD-TEVORMRK-UT                              
084900     ELSE                                                                 
085000       MOVE MFS-RENSA-FAELT TO MOD-IDROLL-UT                              
085100                               MOD-IDDISTR-UT                             
085200                               MOD-IDKUNDNR-UT                            
085300                               MOD-IDANSK-UT                              
085400                               MOD-IDARTNR-UT                             
085500                               MOD-KDSORT-UT                              
085600                               MOD-TEVORMRK-UT                            
085700     END-IF                                                               
085800     .                                                                    
085900     EJECT                                                                
086000 BA-DECIDE-ENTRY SECTION.                                                 
086100     MOVE 'BA-DECIDE-ENTRY ' TO CURRENT-SECTION                           
086200                                                                          
086300     IF MID-IDROLL-IN NOT = ALL '+'                                       
086400       IF MID-IDARTNR-IN = ALL '+'                                        
086500         IF MSGI-KDSORT = 'PA'                                            
086600            MOVE 'D'     TO WS-ENTRY                                      
086700         ELSE                                                             
086800            MOVE 'C'     TO WS-ENTRY                                      
086900         END-IF                                                           
087000       ELSE                                                               
087100         MOVE 'D'        TO WS-ENTRY                                      
087200       END-IF                                                             
087300     ELSE                                                                 
087400       IF MID-IDARTNR-IN NOT = ALL '+'                                    
087500         IF MID-IDDISTR-IN NOT = ALL '+'                                  
087600           IF MID-IDKUNDNR-IN NOT = ALL '+'                               
087700             MOVE 'G'    TO WS-ENTRY                                      
087800           ELSE                                                           
087900             MOVE 'H'    TO WS-ENTRY                                      
088000           END-IF                                                         
088100         ELSE                                                             
088200           MOVE 'J'      TO WS-ENTRY                                      
088300         END-IF                                                           
088400       ELSE                                                               
088500         IF MID-IDDISTR-IN NOT = ALL '+'                                  
088600           IF MID-IDKUNDNR-IN NOT = ALL '+'                               
088700             MOVE 'F'    TO WS-ENTRY                                      
088800           ELSE                                                           
088900             MOVE 'E'    TO WS-ENTRY                                      
089000           END-IF                                                         
089100         ELSE                                                             
089200           IF MID-IDANSK-IN NOT = ALL '+'                                 
089300             MOVE 'I'    TO WS-ENTRY                                      
089400           END-IF                                                         
089500         END-IF                                                           
089600       END-IF                                                             
089700     END-IF                                                               
089800                                                                          
089900     IF WS-ENTRY = SPACE                                                  
090000       IF SPAR-ENTRY > SPACE AND SPAR-IDTRANS = '4225'                    
090100         IF SPAR-ENTRY = 'C'                                              
090200            IF MSGI-KDSORT = 'PA'                                         
090300               MOVE 'D' TO SPAR-ENTRY                                     
090400               MOVE '7' TO MFS-IDPFK                                      
090500               MOVE SPACE TO MFS-KDTRTYP                                  
090600            END-IF                                                        
090700         ELSE                                                             
090800            IF SPAR-ENTRY = 'D'                                           
090900               IF MSGI-KDSORT NOT = 'PA'                                  
091000                  MOVE 'C' TO SPAR-ENTRY                                  
091100                  MOVE '7' TO MFS-IDPFK                                   
091200                  MOVE SPACE TO MFS-KDTRTYP                               
091300               END-IF                                                     
091400            END-IF                                                        
091500         END-IF                                                           
091600         MOVE SPAR-ENTRY TO WS-ENTRY                                      
091700       ELSE                                                               
091800                                                                          
091900         MOVE NEJ TO NYCKLAR-SW                                           
092000         MOVE INF-WRONG-KEY  TO MED-IDMFSFEL                              
092100         CALL WMEDKONV USING MED-WMEDAREA                                 
092200         MOVE MED-MFSFEL TO MOD-TEMFSINF                                  
092300       END-IF                                                             
092400     END-IF                                                               
092500                                                                          
092600     IF NYCKLAR-OK                                                        
092700        EVALUATE WS-ENTRY                                                 
092800        WHEN 'C'                                                          
092900          MOVE SPACE     TO MSGI-IDDISTR                                  
093000                            MSGI-IDKUNDNR                                 
093100                            MSGI-IDANSK                                   
093200                            MSGI-IDARTNR                                  
093300        WHEN 'D'                                                          
093400          MOVE SPACE     TO MSGI-IDDISTR                                  
093500                            MSGI-IDKUNDNR                                 
093600                            MSGI-IDANSK                                   
093700          IF MSGI-KDSORT = 'PA'                                           
093800             MOVE SPACE  TO MSGI-IDARTNR                                  
093900          END-IF                                                          
094000        WHEN 'E'                                                          
094100          MOVE SPACE     TO MSGI-IDROLL                                   
094200                            MSGI-IDKUNDNR                                 
094300                            MSGI-IDANSK                                   
094400                            MSGI-IDARTNR                                  
094500                            MSGI-KDSORT                                   
094600        WHEN 'F'                                                          
094700          MOVE SPACE     TO MSGI-IDROLL                                   
094800                            MSGI-IDANSK                                   
094900                            MSGI-IDARTNR                                  
095000                            MSGI-KDSORT                                   
095100        WHEN 'G'                                                          
095200          MOVE SPACE     TO MSGI-IDROLL                                   
095300                            MSGI-IDANSK                                   
095400                            MSGI-KDSORT                                   
095500        WHEN 'H'                                                          
095600          MOVE SPACE     TO MSGI-IDROLL                                   
095700                            MSGI-IDKUNDNR                                 
095800                            MSGI-IDANSK                                   
095900                            MSGI-KDSORT                                   
096000        WHEN 'I'                                                          
096100          MOVE SPACE     TO MSGI-IDROLL                                   
096200                            MSGI-IDDISTR                                  
096300                            MSGI-IDKUNDNR                                 
096400                            MSGI-IDARTNR                                  
096500                            MSGI-KDSORT                                   
096600        WHEN 'J'                                                          
096700          MOVE SPACE     TO MSGI-IDROLL                                   
096800                            MSGI-IDKUNDNR                                 
096900                            MSGI-IDDISTR                                  
097000                            MSGI-IDANSK                                   
097100                            MSGI-KDSORT                                   
097200        END-EVALUATE                                                      
097300     END-IF                                                               
097400     .                                                                    
097500     EJECT                                                                
097600 BB-DEFAULT-ENTRY SECTION.                                                
097700     MOVE 'BB-DEFAULT-ENTRY' TO CURRENT-SECTION                           
097800                                                                          
097900     MOVE 'C'             TO WS-ENTRY                                     
098000                             SPAR-ENTRY                                   
098100     MOVE MSGI-IDROLL     TO W-IDROLL-CSEQ-MIN                            
098200                             W-IDROLL-CSEQ-MAX                            
098300     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                               
098400                             MOD-IDKUNDNR-UT                              
098500                             MOD-IDANSK-UT                                
098600                             MOD-IDARTNR-UT                               
098700                             MOD-KDSORT-UT                                
098800                             MOD-TEVORMRK-UT                              
098900     MOVE SPACE           TO MSGI-IDDISTR                                 
099000                             MSGI-IDKUNDNR                                
099100                             MSGI-IDANSK                                  
099200                             MSGI-IDARTNR                                 
099300                             MSGI-KDSORT                                  
099400                             MSGI-TEVORMRK                                
099500     .                                                                    
099600 C-FIRST-PAGE SECTION.                                                    
099700     MOVE 'C-FIRST-PAGE    ' TO CURRENT-SECTION                           
099800                                                                          
099900     IF W-IDTRANS = '4223' OR W-IDTRANS = '4233'                          
100000       MOVE INF-AVSL-ORDER TO MED-IDMFSINF                                
100100       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
100200       MOVE '   NR=> ' TO MOD-TEMFSINF (17:8)                             
100300       MOVE MID-IDORDNR-VOR (3:5) TO MOD-TEMFSINF (25:5)                  
100400     END-IF                                                               
100500                                                                          
100600     PERFORM S02-FIX-A6SEQ-KEYS                                           
100700                                                                          
100800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
100900     CALL WMEDKONV USING MED-WMEDAREA                                     
101000*    MOVE MED-MFSINF TO MOD-TEMFSFEL (1:12)                               
101100     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
101200     PERFORM MFS-RENSA-FAELT-IN                                           
101300     .                                                                    
101400     EJECT                                                                
101500 D-NEXT-PAGE SECTION.                                                     
101600     MOVE 'D-NEXT-PAGE     ' TO CURRENT-SECTION                           
101700                                                                          
101800     IF SPAR-IDTRANS = '4225'                                             
101900        EVALUATE SPAR-ENTRY                                               
102000        WHEN 'C'                                                          
102100           MOVE SPAR-WDA6CSEQ-NEXT        TO W-WDA6CSEQ-MIN-X             
102200           MOVE SPAR-IDROLL-CSEQ-NEXT     TO W-IDROLL-CSEQ-MAX            
102300                                                                          
102400        WHEN 'D'                                                          
102500           MOVE SPAR-WDA6DSEQ-NEXT        TO W-WDA6DSEQ-MIN-X             
102600           MOVE SPAR-IDROLL-DSEQ-NEXT     TO W-IDROLL-DSEQ-MAX            
102700           IF MSGI-KDSORT = 'PA'                                          
102800              MOVE SPAR-IDARTNR-DSEQ-NEXT  TO W-IDARTNR-DSEQ-MIN          
102900              MOVE SPAR-TIREGDAT-AVV9-DSEQ-NEXT                           
103000                                           TO W-TIREGDAT-DSEQ-MIN         
103100              MOVE SPAR-TIREGTID-AVV9-DSEQ-NEXT                           
103200                                           TO W-TIREGTID-DSEQ-MIN         
103300              MOVE +999999999              TO W-IDARTNR-DSEQ-MAX          
103400           ELSE                                                           
103500              MOVE SPAR-IDARTNR-DSEQ-NEXT TO W-IDARTNR-DSEQ-MAX           
103600           END-IF                                                         
103700                                                                          
103800        WHEN 'E'                                                          
103900           MOVE SPAR-WDA6ESEQ-NEXT        TO W-WDA6ESEQ-MIN-X             
104000           MOVE SPAR-IDDISTR-ESEQ-NEXT    TO W-IDDISTR-ESEQ-MAX           
104100                                                                          
104200        WHEN 'F'                                                          
104300           MOVE SPAR-WDA6FSEQ-NEXT        TO W-WDA6FSEQ-MIN-X             
104400           MOVE SPAR-IDDISTR-FSEQ-NEXT    TO W-IDDISTR-FSEQ-MAX           
104500           MOVE SPAR-IDKUNDNR-FSEQ-NEXT   TO W-IDKUNDNR-FSEQ-MAX          
104600                                                                          
104700        WHEN 'G'                                                          
104800           MOVE SPAR-WDA6GSEQ-NEXT        TO W-WDA6GSEQ-MIN-X             
104900           MOVE SPAR-IDDISTR-GSEQ-NEXT    TO W-IDDISTR-GSEQ-MAX           
105000           MOVE SPAR-IDKUNDNR-GSEQ-NEXT   TO W-IDKUNDNR-GSEQ-MAX          
105100           MOVE SPAR-IDARTNR-GSEQ-NEXT    TO W-IDARTNR-GSEQ-MAX           
105200                                                                          
105300        WHEN 'H'                                                          
105400           MOVE SPAR-WDA6HSEQ-NEXT        TO W-WDA6HSEQ-MIN-X             
105500           MOVE SPAR-IDDISTR-HSEQ-NEXT    TO W-IDDISTR-HSEQ-MAX           
105600           MOVE SPAR-IDARTNR-HSEQ-NEXT    TO W-IDARTNR-HSEQ-MAX           
105700                                                                          
105800        WHEN 'I'                                                          
105900           MOVE SPAR-WDA6ISEQ-NEXT        TO W-WDA6ISEQ-MIN-X             
106000           MOVE SPAR-IDANSK-ISEQ-NEXT     TO W-IDANSK-ISEQ-MAX            
106100                                                                          
106200        WHEN 'J'                                                          
106300           MOVE SPAR-WDA6JSEQ-NEXT        TO W-WDA6JSEQ-MIN-X             
106400           MOVE SPAR-IDARTNR-JSEQ-NEXT    TO W-IDARTNR-JSEQ-MAX           
106500       END-EVALUATE                                                       
106600       MOVE SPAR-ENTRY TO WS-ENTRY                                        
106700     ELSE                                                                 
106800       MOVE ERR-LAST-PAGE-SHOWN TO MED-IDMFSFEL                           
106900       CALL WMEDKONV USING MED-WMEDAREA                                   
107000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
107100     END-IF                                                               
107200                                                                          
107300     PERFORM MFS-RENSA-FAELT-IN                                           
107400     .                                                                    
107500     EJECT                                                                
107600 E-SAME-PAGE SECTION.                                                     
107700     MOVE 'E-SAME-PAGE     ' TO CURRENT-SECTION                           
107800                                                                          
107900     IF GODK-MID OR HELP-MID                                              
108000       IF MID-INPUT = ALL '+'                                             
108100         PERFORM MFS-RENSA-FAELT-IN                                       
108200         IF SPAR-IDTRANS = '4225' OR '0551'                               
108300           EVALUATE SPAR-ENTRY                                            
108400           WHEN 'C'                                                       
108500             MOVE SPAR-WDA6CSEQ-ENTER      TO W-WDA6CSEQ-MIN-X            
108600             MOVE SPAR-IDROLL-CSEQ-ENTER   TO W-IDROLL-CSEQ-MAX           
108700                                                                          
108800           WHEN 'D'                                                       
108900             MOVE SPAR-WDA6DSEQ-ENTER      TO W-WDA6DSEQ-MIN-X            
109000             MOVE SPAR-IDROLL-DSEQ-ENTER   TO W-IDROLL-DSEQ-MAX           
109100             IF MSGI-KDSORT = 'PA'                                        
109200                MOVE +999999999            TO W-IDARTNR-DSEQ-MAX          
109300             ELSE                                                         
109400                MOVE SPAR-IDARTNR-DSEQ-ENTER                              
109500                                           TO W-IDARTNR-DSEQ-MAX          
109600             END-IF                                                       
109700                                                                          
109800           WHEN 'E'                                                       
109900             MOVE SPAR-WDA6ESEQ-ENTER      TO W-WDA6ESEQ-MIN-X            
110000             MOVE SPAR-IDDISTR-ESEQ-ENTER  TO W-IDDISTR-ESEQ-MAX          
110100                                                                          
110200           WHEN 'F'                                                       
110300             MOVE SPAR-WDA6FSEQ-ENTER      TO W-WDA6FSEQ-MIN-X            
110400             MOVE SPAR-IDDISTR-FSEQ-ENTER  TO W-IDDISTR-FSEQ-MAX          
110500             MOVE SPAR-IDKUNDNR-FSEQ-ENTER TO W-IDKUNDNR-FSEQ-MAX         
110600                                                                          
110700           WHEN 'G'                                                       
110800             MOVE SPAR-WDA6GSEQ-ENTER      TO W-WDA6GSEQ-MIN-X            
110900             MOVE SPAR-IDDISTR-GSEQ-ENTER  TO W-IDDISTR-GSEQ-MAX          
111000             MOVE SPAR-IDKUNDNR-GSEQ-ENTER TO W-IDKUNDNR-GSEQ-MAX         
111100             MOVE SPAR-IDARTNR-GSEQ-ENTER  TO W-IDARTNR-GSEQ-MAX          
111200                                                                          
111300           WHEN 'H'                                                       
111400             MOVE SPAR-WDA6HSEQ-ENTER      TO W-WDA6HSEQ-MIN-X            
111500             MOVE SPAR-IDDISTR-HSEQ-ENTER  TO W-IDDISTR-HSEQ-MAX          
111600             MOVE SPAR-IDARTNR-HSEQ-ENTER  TO W-IDARTNR-HSEQ-MAX          
111700                                                                          
111800           WHEN 'I'                                                       
111900             MOVE SPAR-WDA6ISEQ-ENTER      TO W-WDA6ISEQ-MIN-X            
112000             MOVE SPAR-IDANSK-ISEQ-ENTER   TO W-IDANSK-ISEQ-MAX           
112100                                                                          
112200           WHEN 'J'                                                       
112300             MOVE SPAR-WDA6JSEQ-ENTER      TO W-WDA6JSEQ-MIN-X            
112400             MOVE SPAR-IDARTNR-JSEQ-ENTER  TO W-IDARTNR-JSEQ-MAX          
112500           END-EVALUATE                                                   
112600           MOVE SPAR-ENTRY TO WS-ENTRY                                    
112700         END-IF                                                           
112800       ELSE                                                               
112900         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
113000         CALL WMEDKONV USING MED-WMEDAREA                                 
113100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
113200         PERFORM EA-MID-INDATA-TO-MOD                                     
113300       END-IF                                                             
113400     ELSE                                                                 
113500       PERFORM MFS-RENSA-FAELT-IN                                         
113600     END-IF                                                               
113700     .                                                                    
113800     EJECT                                                                
113900 EA-MID-INDATA-TO-MOD SECTION.                                            
114000     MOVE 'EA-MID-TO-MOD   ' TO CURRENT-SECTION                           
114100                                                                          
114200     MOVE +1 TO INDX                                                      
114300     PERFORM UNTIL INDX > MAX-INDX                                        
114400       IF MID-CMD(INDX) = ALL '+'                                         
114500          MOVE MFS-RENSA-FAELT TO MOD-CMD(INDX)                           
114600       ELSE                                                               
114700          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR(INDX)                
114800          MOVE MFS-ROER-EJ-FAELT TO MOD-CMD(INDX)                         
114900       END-IF                                                             
115000                                                                          
115100       IF MID-TEVORMRK(INDX) = ALL '+'                                    
115200          MOVE MFS-RENSA-FAELT TO MOD-TEVORMRK(INDX)                      
115300       ELSE                                                               
115400          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEVORMRK-ATTR(INDX)           
115500          MOVE MFS-ROER-EJ-FAELT TO MOD-TEVORMRK(INDX)                    
115600       END-IF                                                             
115700       ADD 1 TO INDX                                                      
115800     END-PERFORM                                                          
115900     .                                                                    
116000     EJECT                                                                
116100 F-READ-SHOW-INFO SECTION.                                                
116200     MOVE 'F-READ-SHOW-INFO' TO CURRENT-SECTION                           
116300                                                                          
116400     PERFORM FA-READ-WITH-ENTRY                                           
116500     IF SEGMENT-MISSING                                                   
116600       MOVE ERR-KEYS-ARE-MISSING TO MED-MFSFEL                            
116700       CALL WMEDKONV USING MED-WMEDAREA                                   
116800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
116900       PERFORM MFS-RENSA-FAELT-UT                                         
117000       IF W-IDTRANS = '4223' OR W-IDTRANS = '4233'                        
117100          MOVE INF-AVSL-ORDER TO MED-IDMFSINF                             
117200          CALL WMEDKONV USING MED-WMEDAREA                                
117300          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
117400          MOVE '   NR=> ' TO MOD-TEMFSINF (17:8)                          
117500          MOVE MID-IDORDNR-VOR (3:5) TO MOD-TEMFSINF (25:5)               
117600       END-IF                                                             
117700     ELSE                                                                 
117800       MOVE +1 TO INDX                                                    
117900       PERFORM UNTIL INDX > MAX-INDX                                      
118000         IF SEGMENT-FOUND                                                 
118100           MOVE VOR-TIREGDAT-AVV  TO SPAR-TIREGDAT-AVV-RAD(INDX)          
118200           MOVE VOR-TIREGTID-AVV  TO SPAR-TIREGTID-AVV-RAD(INDX)          
118300           MOVE VOR-TIREGDAT-URSP TO SPAR-TIREGDAT-URSP-RAD(INDX)         
118400           MOVE VOR-TIREGTID-URSP TO SPAR-TIREGTID-URSP-RAD(INDX)         
118500*          MOVE VOR-TIREGDAT-AVV  TO MOD-TIREGDAT-KOD(INDX)               
118600           MOVE VOR-TIREGDAT-AVV  TO WS-TIREGDAT                          
118700           MOVE SPACE             TO WS-KOD                               
118800           PERFORM FE-CHECK-WAREHOUSE-AK                                  
118900*          MOVE WS-TIREGDAT-KOD   TO MOD-TIREGDAT-KOD(INDX)               
119000           MOVE WS-TIREGDAT       TO MOD-TIREGDAT(INDX)                   
119100           MOVE WS-KOD            TO MOD-TIREGDAT-KOD(INDX)               
119200           IF VOR-TEVORMRK = '**'                                         
119300             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEVORMRK-ATTR(INDX)        
119400             IF NOT MFS-SPLIT                                             
119500               MOVE 'ATTEMPT TO ORDER HAS FAILED! TRY AGAIN'              
119600                                  TO MOD-TEMFSINF                         
119700               MOVE 'ASTERIX MARKED ORDERLINE IS WRONG'                   
119800                                  TO MOD-TEMFSFEL                         
119900                                                                          
120000             END-IF                                                       
120100           END-IF                                                         
120200           MOVE VOR-TEVORMRK      TO MOD-TEVORMRK(INDX)                   
120300           IF VOR-FLVORFK = JA                                            
120400             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-ATTR(INDX)         
120500             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKUNDNR-ATTR(INDX)        
120600             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDORDNR7-ATTR(INDX)        
120700           ELSE                                                           
120800             MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-ATTR(INDX)            
120900             MOVE MFS-FORMATETS-ATTR TO MOD-IDKUNDNR-ATTR(INDX)           
121000             MOVE MFS-FORMATETS-ATTR TO MOD-IDORDNR7-ATTR(INDX)           
121100           END-IF                                                         
121200           MOVE VOR-IDDISTR       TO MOD-IDDISTR(INDX)                    
121300           MOVE VOR-IDKUNDNR      TO MOD-IDKUNDNR(INDX)                   
121400           MOVE VOR-IDORDNR7      TO MOD-IDORDNR7(INDX)                   
121500           MOVE VOR-IDANSK        TO MOD-IDANSK(INDX)                     
121600           MOVE VOR-IDARTNR       TO MOD-IDARTNR(INDX)                    
121700           MOVE VOR-KVBEART-URSP  TO MOD-KVBEART(INDX)                    
121800           COMPUTE WS-DIFF = VOR-KVBEART-Q - VOR-KVPREAVB                 
121900           MOVE WS-DIFF           TO MOD-DIFF(INDX)                       
122000           MOVE VOR-KDORDBEK      TO MOD-KDORDBEK(INDX)                   
122100           MOVE VOR-IDDC          TO MOD-IDDC(INDX)                       
122200                                                                          
122300           PERFORM FB-READ-VOR-TEXTS                                      
122400           PERFORM FC-READ-NEXT-SEGMENT                                   
122500         ELSE                                                             
122600         MOVE MFS-RENSA-FAELT TO MOD-CMD(INDX)                            
122700                                 MOD-TIREGDAT(INDX)                       
122800                                 MOD-TIREGDAT-KOD(INDX)                   
122900                                 MOD-TEVORMRK(INDX)                       
123000                                 MOD-IDDISTR(INDX)                        
123100                                 MOD-IDKUNDNR(INDX)                       
123200                                 MOD-IDORDNR7(INDX)                       
123300                                 MOD-IDANSK(INDX)                         
123400                                 MOD-IDARTNR(INDX)                        
123500                                 MOD-KVBEART(INDX)                        
123600                                 MOD-DIFF(INDX)                           
123700                                 MOD-KDORDBEK(INDX)                       
123800                                 MOD-IDDC(INDX)                           
123900                                 MOD-TEVORTXT-FL(INDX)                    
124000                                 MOD-TEVORSC-FL(INDX)                     
124100                                 MOD-TEVORNOT-FL(INDX)                    
124200                                 MOD-TELOSNOT-FL(INDX)                    
124300           MOVE ZERO          TO SPAR-TIREGDAT-AVV-RAD(INDX)              
124400                                 SPAR-TIREGTID-AVV-RAD(INDX)              
124500                                 SPAR-TIREGDAT-URSP-RAD(INDX)             
124600                                 SPAR-TIREGTID-URSP-RAD(INDX)             
124700         END-IF                                                           
124800         ADD 1 TO INDX                                                    
124900       END-PERFORM                                                        
125000       IF SEGMENT-FOUND                                                   
125100         PERFORM FD-SAVE-KEYS-NEXT                                        
125200         IF W-IDTRANS = '4223' OR W-IDTRANS = '4233'                      
125300            MOVE INF-AVSL-ORDER TO MED-IDMFSINF                           
125400            CALL WMEDKONV USING MED-WMEDAREA                              
125500            MOVE MED-MFSINF TO MOD-TEMFSINF                               
125600            MOVE '   NR=> ' TO MOD-TEMFSINF (17:8)                        
125700            MOVE MID-IDORDNR-VOR (3:5) TO MOD-TEMFSINF (25:5)             
125800         ELSE                                                             
125900            MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                     
126000            CALL WMEDKONV USING MED-WMEDAREA                              
126100            MOVE MED-MFSINF TO MOD-TEMFSINF                               
126200         END-IF                                                           
126300       ELSE                                                               
126400         MOVE SPAR-NYCKLAR-ENTER TO SPAR-NYCKLAR-NEXT                     
126500         IF W-IDTRANS = '4223' OR W-IDTRANS = '4233'                      
126600            MOVE INF-AVSL-ORDER TO MED-IDMFSINF                           
126700            CALL WMEDKONV USING MED-WMEDAREA                              
126800            MOVE MED-MFSINF TO MOD-TEMFSINF                               
126900            MOVE '   NR=> ' TO MOD-TEMFSINF (17:8)                        
127000            MOVE MID-IDORDNR-VOR (3:5) TO MOD-TEMFSINF (25:5)             
127100         ELSE                                                             
127200            MOVE INF-LAST-PAGE TO MED-IDMFSINF                            
127300            CALL WMEDKONV USING MED-WMEDAREA                              
127400            MOVE MED-MFSINF TO MOD-TEMFSINF                               
127500         END-IF                                                           
127600       END-IF                                                             
127700       MOVE '002' TO MSGI-KDCALL                                          
127800       MOVE '4225' TO SPAR-IDTRANS                                        
127900       MOVE WS-ENTRY TO SPAR-ENTRY                                        
128000       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
128100       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
128200     END-IF                                                               
128300     .                                                                    
128400     EJECT                                                                
128500 FA-READ-WITH-ENTRY SECTION.                                              
128600     MOVE 'FA-READ-W-ENTRY ' TO CURRENT-SECTION                           
128700                                                                          
128800     EVALUATE WS-ENTRY                                                    
128900     WHEN 'C'                                                             
129000       PERFORM FAA-READ-FIRST-WDA601-CSEQ                                 
129100       IF SEGMENT-FOUND                                                   
129200         MOVE VOR-IDROLL        TO SPAR-IDROLL-CSEQ-ENTER                 
129300         MOVE VOR-TIREGDAT-AVV9 TO SPAR-TIREGDAT-AVV9-CSEQ-ENTER          
129400         MOVE VOR-TIREGTID-AVV9 TO SPAR-TIREGTID-AVV9-CSEQ-ENTER          
129500       END-IF                                                             
129600                                                                          
129700     WHEN 'D'                                                             
129800       PERFORM FAB-READ-FIRST-WDA601-DSEQ                                 
129900       IF SEGMENT-FOUND                                                   
130000         MOVE VOR-IDROLL        TO SPAR-IDROLL-DSEQ-ENTER                 
130100         MOVE VOR-IDARTNR       TO SPAR-IDARTNR-DSEQ-ENTER                
130200         MOVE VOR-TIREGDAT-AVV9 TO SPAR-TIREGDAT-AVV9-DSEQ-ENTER          
130300         MOVE VOR-TIREGTID-AVV9 TO SPAR-TIREGTID-AVV9-DSEQ-ENTER          
130400       END-IF                                                             
130500                                                                          
130600     WHEN 'E'                                                             
130700       PERFORM FAC-READ-FIRST-WDA601-ESEQ                                 
130800       IF SEGMENT-FOUND                                                   
130900         MOVE VOR-IDDISTR       TO SPAR-IDDISTR-ESEQ-ENTER                
131000         MOVE VOR-TIREGDAT-AVV9 TO SPAR-TIREGDAT-AVV9-ESEQ-ENTER          
131100         MOVE VOR-TIREGTID-AVV9 TO SPAR-TIREGTID-AVV9-ESEQ-ENTER          
131200       END-IF                                                             
131300                                                                          
131400     WHEN 'F'                                                             
131500       PERFORM FAD-READ-FIRST-WDA601-FSEQ                                 
131600       IF SEGMENT-FOUND                                                   
131700         MOVE VOR-IDDISTR       TO SPAR-IDDISTR-FSEQ-ENTER                
131800         MOVE VOR-IDKUNDNR      TO SPAR-IDKUNDNR-FSEQ-ENTER               
131900         MOVE VOR-TIREGDAT-AVV9 TO SPAR-TIREGDAT-AVV9-FSEQ-ENTER          
132000         MOVE VOR-TIREGTID-AVV9 TO SPAR-TIREGTID-AVV9-FSEQ-ENTER          
132100       END-IF                                                             
132200                                                                          
132300     WHEN 'G'                                                             
132400       PERFORM FAE-READ-FIRST-WDA601-GSEQ                                 
132500       IF SEGMENT-FOUND                                                   
132600         MOVE VOR-IDDISTR       TO SPAR-IDDISTR-GSEQ-ENTER                
132700         MOVE VOR-IDKUNDNR      TO SPAR-IDKUNDNR-GSEQ-ENTER               
132800         MOVE VOR-IDARTNR       TO SPAR-IDARTNR-GSEQ-ENTER                
132900         MOVE VOR-TIREGDAT-AVV9 TO SPAR-TIREGDAT-AVV9-GSEQ-ENTER          
133000         MOVE VOR-TIREGTID-AVV9 TO SPAR-TIREGTID-AVV9-GSEQ-ENTER          
133100       END-IF                                                             
133200                                                                          
133300     WHEN 'H'                                                             
133400       PERFORM FAH-READ-FIRST-WDA601-HSEQ                                 
133500       IF SEGMENT-FOUND                                                   
133600         MOVE VOR-IDDISTR       TO SPAR-IDDISTR-HSEQ-ENTER                
133700         MOVE VOR-IDARTNR       TO SPAR-IDARTNR-HSEQ-ENTER                
133800         MOVE VOR-TIREGDAT-AVV9 TO SPAR-TIREGDAT-AVV9-HSEQ-ENTER          
133900         MOVE VOR-TIREGTID-AVV9 TO SPAR-TIREGTID-AVV9-HSEQ-ENTER          
134000       END-IF                                                             
134100                                                                          
134200     WHEN 'I'                                                             
134300       PERFORM FAI-READ-FIRST-WDA601-ISEQ                                 
134400       IF SEGMENT-FOUND                                                   
134500         MOVE VOR-IDANSK         TO SPAR-IDANSK-ISEQ-ENTER                
134600         MOVE VOR-TIREGDAT-AVV9  TO SPAR-TIREGDAT-AVV9-ISEQ-ENTER         
134700         MOVE VOR-TIREGTID-AVV9  TO SPAR-TIREGTID-AVV9-ISEQ-ENTER         
134800       END-IF                                                             
134900                                                                          
135000     WHEN 'J'                                                             
135100       PERFORM FAJ-READ-FIRST-WDA601-JSEQ                                 
135200       IF SEGMENT-FOUND                                                   
135300         MOVE VOR-IDARTNR        TO SPAR-IDARTNR-JSEQ-ENTER               
135400         MOVE VOR-TIREGDAT-AVV9  TO SPAR-TIREGDAT-AVV9-JSEQ-ENTER         
135500         MOVE VOR-TIREGTID-AVV9  TO SPAR-TIREGTID-AVV9-JSEQ-ENTER         
135600       END-IF                                                             
135700     END-EVALUATE                                                         
135800     .                                                                    
135900     EJECT                                                                
136000 FAA-READ-FIRST-WDA601-CSEQ   SECTION.                                    
136100     MOVE 'FAA-FIRST-A6-CEQ' TO CURRENT-SECTION                           
136200                                                                          
136300     PERFORM IMS-GU-WDA601-CSEQ                                           
136400     IF SEGMENT-FOUND                                                     
136500        IF MSGI-TEVORMRK NOT = SPACE                                      
136600           PERFORM UNTIL SEGMENT-MISSING OR                               
136700                         VOR-TEVORMRK = MSGI-TEVORMRK                     
136800              PERFORM IMS-GN-WDA601-CSEQ                                  
136900           END-PERFORM                                                    
137000        END-IF                                                            
137100     END-IF                                                               
137200     .                                                                    
137300 FAB-READ-FIRST-WDA601-DSEQ   SECTION.                                    
137400     MOVE 'FAB-FIRST-A6-DEQ' TO CURRENT-SECTION                           
137500                                                                          
137600     PERFORM IMS-GU-WDA601-DSEQ                                           
137700     IF SEGMENT-FOUND                                                     
137800        IF MSGI-TEVORMRK NOT = SPACE                                      
137900           PERFORM UNTIL SEGMENT-MISSING OR                               
138000                         VOR-TEVORMRK = MSGI-TEVORMRK                     
138100              PERFORM IMS-GN-WDA601-DSEQ                                  
138200           END-PERFORM                                                    
138300        END-IF                                                            
138400     END-IF                                                               
138500     .                                                                    
138600 FAC-READ-FIRST-WDA601-ESEQ SECTION.                                      
138700     MOVE 'FAC-FIRST-A6-EEQ' TO CURRENT-SECTION                           
138800                                                                          
138900     PERFORM IMS-GU-WDA601-ESEQ                                           
139000     IF SEGMENT-FOUND                                                     
139100        IF MSGI-TEVORMRK NOT = SPACE                                      
139200           PERFORM UNTIL SEGMENT-MISSING OR                               
139300                         VOR-TEVORMRK = MSGI-TEVORMRK                     
139400              PERFORM IMS-GN-WDA601-ESEQ                                  
139500           END-PERFORM                                                    
139600        END-IF                                                            
139700     END-IF                                                               
139800     .                                                                    
139900 FAD-READ-FIRST-WDA601-FSEQ SECTION.                                      
140000     MOVE 'FAD-FIRST-A6-FEQ' TO CURRENT-SECTION                           
140100                                                                          
140200     PERFORM IMS-GU-WDA601-FSEQ                                           
140300     IF SEGMENT-FOUND                                                     
140400        IF MSGI-TEVORMRK NOT = SPACE                                      
140500           PERFORM UNTIL SEGMENT-MISSING OR                               
140600                         VOR-TEVORMRK = MSGI-TEVORMRK                     
140700              PERFORM IMS-GN-WDA601-FSEQ                                  
140800           END-PERFORM                                                    
140900        END-IF                                                            
141000     END-IF                                                               
141100     .                                                                    
141200 FAE-READ-FIRST-WDA601-GSEQ SECTION.                                      
141300     MOVE 'FAE-FIRST-A6-GEQ' TO CURRENT-SECTION                           
141400                                                                          
141500     PERFORM IMS-GU-WDA601-GSEQ                                           
141600     IF SEGMENT-FOUND                                                     
141700        IF MSGI-TEVORMRK NOT = SPACE                                      
141800           PERFORM UNTIL SEGMENT-MISSING OR                               
141900                         VOR-TEVORMRK = MSGI-TEVORMRK                     
142000              PERFORM IMS-GN-WDA601-GSEQ                                  
142100           END-PERFORM                                                    
142200        END-IF                                                            
142300     END-IF                                                               
142400     .                                                                    
142500 FAH-READ-FIRST-WDA601-HSEQ SECTION.                                      
142600     MOVE 'FAH-FIRST-A6-HEQ' TO CURRENT-SECTION                           
142700                                                                          
142800     PERFORM IMS-GU-WDA601-HSEQ                                           
142900     IF SEGMENT-FOUND                                                     
143000        IF MSGI-TEVORMRK NOT = SPACE                                      
143100           PERFORM UNTIL SEGMENT-MISSING OR                               
143200                         VOR-TEVORMRK = MSGI-TEVORMRK                     
143300              PERFORM IMS-GN-WDA601-HSEQ                                  
143400           END-PERFORM                                                    
143500        END-IF                                                            
143600     END-IF                                                               
143700     .                                                                    
143800 FAI-READ-FIRST-WDA601-ISEQ SECTION.                                      
143900     MOVE 'FAI-FIRST-A6-IEQ' TO CURRENT-SECTION                           
144000                                                                          
144100     PERFORM IMS-GU-WDA601-ISEQ                                           
144200     IF SEGMENT-FOUND                                                     
144300        IF MSGI-TEVORMRK NOT = SPACE                                      
144400           PERFORM UNTIL SEGMENT-MISSING OR                               
144500                         VOR-TEVORMRK = MSGI-TEVORMRK                     
144600              PERFORM IMS-GN-WDA601-ISEQ                                  
144700           END-PERFORM                                                    
144800        END-IF                                                            
144900     END-IF                                                               
145000     .                                                                    
145100 FAJ-READ-FIRST-WDA601-JSEQ SECTION.                                      
145200     MOVE 'FAJ-FIRST-A6-JEQ' TO CURRENT-SECTION                           
145300                                                                          
145400     PERFORM IMS-GU-WDA601-JSEQ                                           
145500     IF SEGMENT-FOUND                                                     
145600        IF MSGI-TEVORMRK NOT = SPACE                                      
145700           PERFORM UNTIL SEGMENT-MISSING OR                               
145800                         VOR-TEVORMRK = MSGI-TEVORMRK                     
145900              PERFORM IMS-GN-WDA601-JSEQ                                  
146000           END-PERFORM                                                    
146100        END-IF                                                            
146200     END-IF                                                               
146300     .                                                                    
146400                                                                          
146500 FB-READ-VOR-TEXTS SECTION.                                               
146600     MOVE 'FB-READ-VOR-TEXT' TO CURRENT-SECTION                           
146700                                                                          
146800     EVALUATE WS-ENTRY                                                    
146900       WHEN 'C'                                                           
147000         PERFORM IMS-GNP-WDA611-CSEQ                                      
147100         IF SEGMENT-FOUND                                                 
147200           MOVE JA TO WDA611-SW                                           
147300         END-IF                                                           
147400         PERFORM IMS-GNP-WDA612-CSEQ                                      
147500         IF SEGMENT-FOUND                                                 
147600           MOVE JA TO WDA612-SW                                           
147700         END-IF                                                           
147800         PERFORM IMS-GNP-WDA613-CSEQ                                      
147900         IF SEGMENT-FOUND                                                 
148000           MOVE JA TO WDA613-SW                                           
148100         END-IF                                                           
148200       WHEN 'D'                                                           
148300         PERFORM IMS-GNP-WDA611-DSEQ                                      
148400         IF SEGMENT-FOUND                                                 
148500           MOVE JA TO WDA611-SW                                           
148600         END-IF                                                           
148700         PERFORM IMS-GNP-WDA612-DSEQ                                      
148800         IF SEGMENT-FOUND                                                 
148900           MOVE JA TO WDA612-SW                                           
149000         END-IF                                                           
149100         PERFORM IMS-GNP-WDA613-DSEQ                                      
149200         IF SEGMENT-FOUND                                                 
149300           MOVE JA TO WDA613-SW                                           
149400         END-IF                                                           
149500       WHEN 'E'                                                           
149600         PERFORM IMS-GNP-WDA611-ESEQ                                      
149700         IF SEGMENT-FOUND                                                 
149800           MOVE JA TO WDA611-SW                                           
149900         END-IF                                                           
150000         PERFORM IMS-GNP-WDA612-ESEQ                                      
150100         IF SEGMENT-FOUND                                                 
150200           MOVE JA TO WDA612-SW                                           
150300         END-IF                                                           
150400         PERFORM IMS-GNP-WDA613-ESEQ                                      
150500         IF SEGMENT-FOUND                                                 
150600           MOVE JA TO WDA613-SW                                           
150700         END-IF                                                           
150800       WHEN 'F'                                                           
150900         PERFORM IMS-GNP-WDA611-FSEQ                                      
151000         IF SEGMENT-FOUND                                                 
151100           MOVE JA TO WDA611-SW                                           
151200         END-IF                                                           
151300         PERFORM IMS-GNP-WDA612-FSEQ                                      
151400         IF SEGMENT-FOUND                                                 
151500           MOVE JA TO WDA612-SW                                           
151600         END-IF                                                           
151700         PERFORM IMS-GNP-WDA613-FSEQ                                      
151800         IF SEGMENT-FOUND                                                 
151900           MOVE JA TO WDA613-SW                                           
152000         END-IF                                                           
152100       WHEN 'G'                                                           
152200         PERFORM IMS-GNP-WDA611-GSEQ                                      
152300         IF SEGMENT-FOUND                                                 
152400           MOVE JA TO WDA611-SW                                           
152500         END-IF                                                           
152600         PERFORM IMS-GNP-WDA612-GSEQ                                      
152700         IF SEGMENT-FOUND                                                 
152800           MOVE JA TO WDA612-SW                                           
152900         END-IF                                                           
153000         PERFORM IMS-GNP-WDA613-GSEQ                                      
153100         IF SEGMENT-FOUND                                                 
153200           MOVE JA TO WDA613-SW                                           
153300         END-IF                                                           
153400       WHEN 'H'                                                           
153500         PERFORM IMS-GNP-WDA611-HSEQ                                      
153600         IF SEGMENT-FOUND                                                 
153700           MOVE JA TO WDA611-SW                                           
153800         END-IF                                                           
153900         PERFORM IMS-GNP-WDA612-HSEQ                                      
154000         IF SEGMENT-FOUND                                                 
154100           MOVE JA TO WDA612-SW                                           
154200         END-IF                                                           
154300         PERFORM IMS-GNP-WDA613-HSEQ                                      
154400         IF SEGMENT-FOUND                                                 
154500           MOVE JA TO WDA613-SW                                           
154600         END-IF                                                           
154700       WHEN 'I'                                                           
154800         PERFORM IMS-GNP-WDA611-ISEQ                                      
154900         IF SEGMENT-FOUND                                                 
155000           MOVE JA TO WDA611-SW                                           
155100         END-IF                                                           
155200         PERFORM IMS-GNP-WDA612-ISEQ                                      
155300         IF SEGMENT-FOUND                                                 
155400           MOVE JA TO WDA612-SW                                           
155500         END-IF                                                           
155600         PERFORM IMS-GNP-WDA613-ISEQ                                      
155700         IF SEGMENT-FOUND                                                 
155800           MOVE JA TO WDA613-SW                                           
155900         END-IF                                                           
156000       WHEN 'J'                                                           
156100         PERFORM IMS-GNP-WDA611-JSEQ                                      
156200         IF SEGMENT-FOUND                                                 
156300           MOVE JA TO WDA611-SW                                           
156400         END-IF                                                           
156500         PERFORM IMS-GNP-WDA612-JSEQ                                      
156600         IF SEGMENT-FOUND                                                 
156700           MOVE JA TO WDA612-SW                                           
156800         END-IF                                                           
156900         PERFORM IMS-GNP-WDA613-JSEQ                                      
157000         IF SEGMENT-FOUND                                                 
157100           MOVE JA TO WDA613-SW                                           
157200         END-IF                                                           
157300     END-EVALUATE                                                         
157400                                                                          
157500     IF WDA611-FOUND OR WDA612-FOUND                                      
157600       MOVE 'T' TO MOD-TEVORTXT-FL(INDX)                                  
157700       MOVE NEJ TO WDA611-SW                                              
157800                   WDA612-SW                                              
157900     ELSE                                                                 
158000       MOVE MFS-RENSA-FAELT TO MOD-TEVORTXT-FL(INDX)                      
158100     END-IF                                                               
158200                                                                          
158300     IF WDA613-FOUND                                                      
158400       MOVE 'T' TO MOD-TEVORSC-FL(INDX)                                   
158500       IF VTS-FLLAEST NOT = JA                                            
158600         MOVE MFS-ADD-LYS-UPP-FAELT                                       
158700                              TO MOD-TEVORSC-FL-ATTR(INDX)                
158800       END-IF                                                             
158900       MOVE NEJ TO WDA613-SW                                              
159000     ELSE                                                                 
159100       MOVE MFS-RENSA-FAELT TO MOD-TEVORSC-FL(INDX)                       
159200     END-IF                                                               
159300                                                                          
159400     PERFORM FBA-READ-PROCURER-TEXTS                                      
159500     .                                                                    
159600     EJECT                                                                
159700 FBA-READ-PROCURER-TEXTS SECTION.                                         
159800     MOVE 'FBA-READ-PROC-TX' TO CURRENT-SECTION                           
159900                                                                          
160000     MOVE VOR-IDARTNR TO W-IDARTNR-D9                                     
160100     MOVE WC-CDC-SE   TO W-IDDC-D9                                        
160200     MOVE VOR-IDLEVNR TO W-IDLEVNR                                        
160300                                                                          
160400     PERFORM IMS-GU-WDD902                                                
160500     MOVE ZERO TO WS-TIREGDAT-D9                                          
160600                  WS-TIREGTID-D9                                          
160700     IF SEGMENT-FOUND                                                     
160800*WDD924                                                                   
160900       PERFORM IMS-GNP-WDD924                                             
161000       IF SEGMENT-FOUND                                                   
161100          PERFORM UNTIL SEGMENT-MISSING                                   
161200             IF D9-LEV-TIREGDAT > WS-TIREGDAT-D9                          
161300             OR (D9-LEV-TIREGDAT = WS-TIREGDAT-D9 AND                     
161400                 D9-LEV-TIREGTID > WS-TIREGTID-D9)                        
161500                                                                          
161600                MOVE D9-LEV-TIREGDAT TO WS-TIREGDAT-D9                    
161700                MOVE D9-LEV-TIREGTID TO WS-TIREGTID-D9                    
161800                                                                          
161900             END-IF                                                       
162000             PERFORM IMS-GNP-WDD924                                       
162100          END-PERFORM                                                     
162200          MOVE WS-TIREGDAT-D9 TO WS-DAT                                   
162300          MOVE WS-TIREGTID-D9 TO WS-TID                                   
162400* WE CAN SKIP CALLING CEEISEC FOR OLD RECORDS BECAUSE THE NEW DATE        
162401* FIELD 'LEV-TIREGDAT' IS INITIALIZED TO ZERO AND WOULD ABEND IF          
162402* WE CALL CEEISEC WITH ZERO DATE. OLD RECORDS WILL NOT DISPLAY            
162403* TIME ANYWAY SINCE THEY ARE OLDER THAN 24HRS.                            
162410          IF WS-DAT = ZEROES AND WS-TID = ZEROES                          
162420            MOVE 'S  ' TO MOD-TEVORNOT-FL(INDX)                           
162430          ELSE                                                            
162500            PERFORM S01-CALL-CEEISEC                                      
162600            PERFORM FBAA-CHECK-NOTE-TIME                                  
162610          END-IF                                                          
162700       ELSE                                                               
162800*WDD925-READ ONLY IF WDD924 IS ABSENT.                                    
162900         MOVE ZERO TO WS-TIREGDAT-D9                                      
163000                      WS-TIREGTID-D9                                      
163100         PERFORM IMS-GNP-WDD925                                           
163200         IF SEGMENT-FOUND                                                 
163300            PERFORM UNTIL SEGMENT-MISSING                                 
163400               IF D9-INFO-TIREGDAT > WS-TIREGDAT-D9                       
163500               OR (D9-INFO-TIREGDAT = WS-TIREGDAT-D9 AND                  
163600                   D9-INFO-TIREGTID > WS-TIREGTID-D9)                     
163700                                                                          
163800                  MOVE D9-INFO-TIREGDAT TO WS-TIREGDAT-D9                 
163900                  MOVE D9-INFO-TIREGTID TO WS-TIREGTID-D9                 
164000                                                                          
164100               END-IF                                                     
164200               PERFORM IMS-GNP-WDD925                                     
164300            END-PERFORM                                                   
164400            MOVE WS-TIREGDAT-D9 TO WS-DAT                                 
164500            MOVE WS-TIREGTID-D9 TO WS-TID                                 
164600            PERFORM S01-CALL-CEEISEC                                      
164700            PERFORM FBAB-CHECK-TEXT-TIME                                  
164800         END-IF                                                           
164900       END-IF                                                             
165000     END-IF                                                               
165100                                                                          
165200     MOVE 'S' TO W-IDSKYLT                                                
165300     MOVE 'LOSNOT' TO W-IDDOKTYP                                          
165400     MOVE VOR-IDARTNR TO WS-IDARTNR-NUM                                   
165500     MOVE WS-IDARTNR-NUM(2:8) TO WS-IDARTNR-ALPHA                         
165600     INSPECT WS-IDARTNR-ALPHA REPLACING LEADING ZERO BY SPACE             
165700     MOVE WS-IDARTNR-ALPHA TO W-IDDOK                                     
165800     PERFORM IMS-GU-WDP501                                                
165900     IF SEGMENT-FOUND                                                     
166000       MOVE 'L' TO MOD-TELOSNOT-FL(INDX)                                  
166100     ELSE                                                                 
166200       MOVE MFS-RENSA-FAELT TO MOD-TELOSNOT-FL(INDX)                      
166300     END-IF                                                               
166400     .                                                                    
166500     EJECT                                                                
166600 FBAA-CHECK-NOTE-TIME SECTION.                                            
166700     MOVE 'FBAA-CHECK-NOTE-TIME ' TO CURRENT-SECTION                      
166800*  ---    2 HRS = 7200 SEC, 8 HRS = 28800 SEC, 24 HRS = 86400 SEC         
166900     IF (OUTSEC-NU - OUTSEC-SVAR) < 7200                                  
167000       MOVE 'S02' TO MOD-TEVORNOT-FL(INDX)                                
167100     ELSE                                                                 
167200       IF (OUTSEC-NU - OUTSEC-SVAR) > 7200 AND < 28800                    
167300         MOVE 'S08' TO MOD-TEVORNOT-FL(INDX)                              
167400       ELSE                                                               
167500         IF (OUTSEC-NU - OUTSEC-SVAR) > 28800 AND < 86400                 
167600           MOVE 'S24' TO MOD-TEVORNOT-FL(INDX)                            
167700         ELSE                                                             
167800           MOVE 'S  ' TO MOD-TEVORNOT-FL(INDX)                            
167900         END-IF                                                           
168000       END-IF                                                             
168100     END-IF                                                               
168200     .                                                                    
168300     EJECT                                                                
168400 FBAB-CHECK-TEXT-TIME SECTION.                                            
168500     MOVE 'FBAB-CHECK-TEXT-TIME ' TO CURRENT-SECTION                      
168600*  ---    2 HRS = 7200 SEC, 8 HRS = 28800 SEC, 24 HRS = 86400 SEC         
168700     IF (OUTSEC-NU - OUTSEC-SVAR) < 7200                                  
168800       MOVE 'T02' TO MOD-TEVORNOT-FL(INDX)                                
168900     ELSE                                                                 
169000       IF (OUTSEC-NU - OUTSEC-SVAR) > 7200 AND < 28800                    
169100         MOVE 'T08' TO MOD-TEVORNOT-FL(INDX)                              
169200       ELSE                                                               
169300         IF (OUTSEC-NU - OUTSEC-SVAR) > 28800 AND < 86400                 
169400           MOVE 'T24' TO MOD-TEVORNOT-FL(INDX)                            
169500         ELSE                                                             
169600           MOVE 'T  ' TO MOD-TEVORNOT-FL(INDX)                            
169700         END-IF                                                           
169800       END-IF                                                             
169900     END-IF                                                               
170000     .                                                                    
170100     EJECT                                                                
170200 FC-READ-NEXT-SEGMENT SECTION.                                            
170300     MOVE 'FC-READ-NEXT-SEG' TO CURRENT-SECTION                           
170400                                                                          
170500     EVALUATE WS-ENTRY                                                    
170600     WHEN 'C'                                                             
170700       PERFORM FCA-READ-NEXT-WDA601-CSEQ                                  
170800                                                                          
170900     WHEN 'D'                                                             
171000       PERFORM FCB-READ-NEXT-WDA601-DSEQ                                  
171100                                                                          
171200     WHEN 'E'                                                             
171300       PERFORM FCC-READ-NEXT-WDA601-ESEQ                                  
171400                                                                          
171500     WHEN 'F'                                                             
171600       PERFORM FCD-READ-NEXT-WDA601-FSEQ                                  
171700                                                                          
171800     WHEN 'G'                                                             
171900       PERFORM FCE-READ-NEXT-WDA601-GSEQ                                  
172000                                                                          
172100     WHEN 'H'                                                             
172200       PERFORM FCF-READ-NEXT-WDA601-HSEQ                                  
172300                                                                          
172400     WHEN 'I'                                                             
172500       PERFORM FCG-READ-NEXT-WDA601-ISEQ                                  
172600                                                                          
172700     WHEN 'J'                                                             
172800       PERFORM FCH-READ-NEXT-WDA601-JSEQ                                  
172900     END-EVALUATE                                                         
173000     .                                                                    
173100     EJECT                                                                
173200 FCA-READ-NEXT-WDA601-CSEQ   SECTION.                                     
173300     MOVE 'FCA-NEXT-A6-CSEQ' TO CURRENT-SECTION                           
173400                                                                          
173500     PERFORM IMS-GN-WDA601-CSEQ                                           
173600     IF SEGMENT-FOUND                                                     
173700        IF MSGI-TEVORMRK NOT = SPACE                                      
173800           PERFORM UNTIL SEGMENT-MISSING OR                               
173900                         VOR-TEVORMRK = MSGI-TEVORMRK                     
174000              PERFORM IMS-GN-WDA601-CSEQ                                  
174100           END-PERFORM                                                    
174200        END-IF                                                            
174300     END-IF                                                               
174400     .                                                                    
174500     EJECT                                                                
174600 FCB-READ-NEXT-WDA601-DSEQ   SECTION.                                     
174700     MOVE 'FCB-NEXT-A6-DSEQ' TO CURRENT-SECTION                           
174800                                                                          
174900     PERFORM IMS-GN-WDA601-DSEQ                                           
175000     IF SEGMENT-FOUND                                                     
175100        IF MSGI-TEVORMRK NOT = SPACE                                      
175200           PERFORM UNTIL SEGMENT-MISSING OR                               
175300                         VOR-TEVORMRK = MSGI-TEVORMRK                     
175400              PERFORM IMS-GN-WDA601-DSEQ                                  
175500           END-PERFORM                                                    
175600        END-IF                                                            
175700     END-IF                                                               
175800     .                                                                    
175900     EJECT                                                                
176000 FCC-READ-NEXT-WDA601-ESEQ SECTION.                                       
176100     MOVE 'FCC-NEXT-A6-ESEQ' TO CURRENT-SECTION                           
176200                                                                          
176300     PERFORM IMS-GN-WDA601-ESEQ                                           
176400     IF SEGMENT-FOUND                                                     
176500        IF MSGI-TEVORMRK NOT = SPACE                                      
176600           PERFORM UNTIL SEGMENT-MISSING OR                               
176700                         VOR-TEVORMRK = MSGI-TEVORMRK                     
176800              PERFORM IMS-GN-WDA601-ESEQ                                  
176900           END-PERFORM                                                    
177000        END-IF                                                            
177100     END-IF                                                               
177200     .                                                                    
177300     EJECT                                                                
177400 FCD-READ-NEXT-WDA601-FSEQ SECTION.                                       
177500     MOVE 'FCD-NEXT-A6-FSEQ' TO CURRENT-SECTION                           
177600                                                                          
177700     PERFORM IMS-GN-WDA601-FSEQ                                           
177800     IF SEGMENT-FOUND                                                     
177900        IF MSGI-TEVORMRK NOT = SPACE                                      
178000           PERFORM UNTIL SEGMENT-MISSING OR                               
178100                         VOR-TEVORMRK = MSGI-TEVORMRK                     
178200              PERFORM IMS-GN-WDA601-FSEQ                                  
178300           END-PERFORM                                                    
178400        END-IF                                                            
178500     END-IF                                                               
178600     .                                                                    
178700     EJECT                                                                
178800 FCE-READ-NEXT-WDA601-GSEQ SECTION.                                       
178900     MOVE 'FCE-NEXT-A6-GSEQ' TO CURRENT-SECTION                           
179000                                                                          
179100     PERFORM IMS-GN-WDA601-GSEQ                                           
179200     IF SEGMENT-FOUND                                                     
179300        IF MSGI-TEVORMRK NOT = SPACE                                      
179400           PERFORM UNTIL SEGMENT-MISSING OR                               
179500                         VOR-TEVORMRK = MSGI-TEVORMRK                     
179600              PERFORM IMS-GN-WDA601-GSEQ                                  
179700           END-PERFORM                                                    
179800        END-IF                                                            
179900     END-IF                                                               
180000     .                                                                    
180100     EJECT                                                                
180200 FCF-READ-NEXT-WDA601-HSEQ SECTION.                                       
180300     MOVE 'FCF-NEXT-A6-HSEQ' TO CURRENT-SECTION                           
180400                                                                          
180500     PERFORM IMS-GN-WDA601-HSEQ                                           
180600     IF SEGMENT-FOUND                                                     
180700        IF MSGI-TEVORMRK NOT = SPACE                                      
180800           PERFORM UNTIL SEGMENT-MISSING OR                               
180900                         VOR-TEVORMRK = MSGI-TEVORMRK                     
181000              PERFORM IMS-GN-WDA601-HSEQ                                  
181100           END-PERFORM                                                    
181200        END-IF                                                            
181300     END-IF                                                               
181400     .                                                                    
181500     EJECT                                                                
181600 FCG-READ-NEXT-WDA601-ISEQ SECTION.                                       
181700     MOVE 'FCG-NEXT-A6-ISEQ' TO CURRENT-SECTION                           
181800                                                                          
181900     PERFORM IMS-GN-WDA601-ISEQ                                           
182000     IF SEGMENT-FOUND                                                     
182100        IF MSGI-TEVORMRK NOT = SPACE                                      
182200           PERFORM UNTIL SEGMENT-MISSING OR                               
182300                         VOR-TEVORMRK = MSGI-TEVORMRK                     
182400              PERFORM IMS-GN-WDA601-ISEQ                                  
182500           END-PERFORM                                                    
182600        END-IF                                                            
182700     END-IF                                                               
182800     .                                                                    
182900     EJECT                                                                
183000 FCH-READ-NEXT-WDA601-JSEQ SECTION.                                       
183100     MOVE 'FCH-NEXT-A6-JSEQ' TO CURRENT-SECTION                           
183200                                                                          
183300     PERFORM IMS-GN-WDA601-JSEQ                                           
183400     IF SEGMENT-FOUND                                                     
183500        IF MSGI-TEVORMRK NOT = SPACE                                      
183600           PERFORM UNTIL SEGMENT-MISSING OR                               
183700                         VOR-TEVORMRK = MSGI-TEVORMRK                     
183800              PERFORM IMS-GN-WDA601-JSEQ                                  
183900           END-PERFORM                                                    
184000        END-IF                                                            
184100     END-IF                                                               
184200     .                                                                    
184300 FD-SAVE-KEYS-NEXT SECTION.                                               
184400     MOVE 'FD-SAVE-KEYS-NEX' TO CURRENT-SECTION                           
184500                                                                          
184600     IF SEGMENT-FOUND                                                     
184700       IF WS-ENTRY = 'C'                                                  
184800         MOVE W-WDA6CSEQ-MIN-X  TO SPAR-NYCKLAR-NEXT                      
184900         MOVE VOR-TIREGDAT-AVV9 TO SPAR-TIREGDAT-AVV9-CSEQ-NEXT           
185000         MOVE VOR-TIREGTID-AVV9 TO SPAR-TIREGTID-AVV9-CSEQ-NEXT           
185100       ELSE                                                               
185200         IF WS-ENTRY = 'D'                                                
185300           MOVE W-WDA6DSEQ-MIN-X  TO SPAR-NYCKLAR-NEXT                    
185400           MOVE VOR-IDARTNR       TO SPAR-IDARTNR-DSEQ-NEXT               
185500           MOVE VOR-TIREGDAT-AVV9 TO SPAR-TIREGDAT-AVV9-DSEQ-NEXT         
185600           MOVE VOR-TIREGTID-AVV9 TO SPAR-TIREGTID-AVV9-DSEQ-NEXT         
185700         ELSE                                                             
185800           IF WS-ENTRY = 'E'                                              
185900             MOVE W-WDA6ESEQ-MIN-X TO SPAR-NYCKLAR-NEXT                   
186000             MOVE VOR-TIREGDAT-AVV9                                       
186100                                   TO SPAR-TIREGDAT-AVV9-ESEQ-NEXT        
186200             MOVE VOR-TIREGTID-AVV9                                       
186300                                   TO SPAR-TIREGTID-AVV9-ESEQ-NEXT        
186400           ELSE                                                           
186500             IF WS-ENTRY = 'F'                                            
186600               MOVE W-WDA6FSEQ-MIN-X TO SPAR-NYCKLAR-NEXT                 
186700               MOVE VOR-TIREGDAT-AVV9                                     
186800                                   TO SPAR-TIREGDAT-AVV9-FSEQ-NEXT        
186900               MOVE VOR-TIREGTID-AVV9                                     
187000                                   TO SPAR-TIREGTID-AVV9-FSEQ-NEXT        
187100             ELSE                                                         
187200               IF WS-ENTRY = 'G'                                          
187300                 MOVE W-WDA6GSEQ-MIN-X TO SPAR-NYCKLAR-NEXT               
187400                 MOVE VOR-TIREGDAT-AVV9                                   
187500                                   TO SPAR-TIREGDAT-AVV9-GSEQ-NEXT        
187600                 MOVE VOR-TIREGTID-AVV9                                   
187700                                   TO SPAR-TIREGTID-AVV9-GSEQ-NEXT        
187800               ELSE                                                       
187900                 IF WS-ENTRY = 'H'                                        
188000                   MOVE W-WDA6HSEQ-MIN-X TO SPAR-NYCKLAR-NEXT             
188100                   MOVE VOR-TIREGDAT-AVV9                                 
188200                                   TO SPAR-TIREGDAT-AVV9-HSEQ-NEXT        
188300                   MOVE VOR-TIREGTID-AVV9                                 
188400                                   TO SPAR-TIREGTID-AVV9-HSEQ-NEXT        
188500                 ELSE                                                     
188600                   IF WS-ENTRY = 'I'                                      
188700                     MOVE W-WDA6ISEQ-MIN-X TO SPAR-NYCKLAR-NEXT           
188800                     MOVE VOR-TIREGDAT-AVV9                               
188900                                   TO SPAR-TIREGDAT-AVV9-ISEQ-NEXT        
189000                     MOVE VOR-TIREGTID-AVV9                               
189100                                   TO SPAR-TIREGTID-AVV9-ISEQ-NEXT        
189200                   ELSE                                                   
189300                     IF WS-ENTRY = 'J'                                    
189400                       MOVE W-WDA6JSEQ-MIN-X TO SPAR-NYCKLAR-NEXT         
189500                       MOVE VOR-TIREGDAT-AVV9                             
189600                                   TO SPAR-TIREGDAT-AVV9-JSEQ-NEXT        
189700                       MOVE VOR-TIREGTID-AVV9                             
189800                                   TO SPAR-TIREGTID-AVV9-JSEQ-NEXT        
189900                     END-IF                                               
190000                   END-IF                                                 
190100                 END-IF                                                   
190200               END-IF                                                     
190300             END-IF                                                       
190400           END-IF                                                         
190500         END-IF                                                           
190600       END-IF                                                             
190700     END-IF                                                               
190800     .                                                                    
190900     EJECT                                                                
191000 FE-CHECK-WAREHOUSE-AK SECTION.                                           
191100     MOVE 'FE-CHECK-WH-AK  ' TO CURRENT-SECTION                           
191200                                                                          
191300     MOVE VOR-IDARTNR TO W-IDARTNR                                        
191400     MOVE VOR-IDDC    TO W-IDDC                                           
191500                                                                          
191600     PERFORM IMS-GU-WDK611                                                
191700     IF SEGMENT-FOUND                                                     
191800        IF CLAG-KVLS > ZERO                                               
191900           MOVE 'P'              TO WS-KOD                                
192000           MOVE MFS-ADD-LYS-UPP-FAELT                                     
192100                                 TO MOD-TIREGDAT-KOD-ATTR(INDX)           
192200        ELSE                                                              
192300           IF CLAG-KVAKS-CDC > ZERO                                       
192400*          MOVE MFS-ADD-LYS-UPP-FAELT                                     
192500*                                TO MOD-TIREGDAT-ATTR(INDX)               
192600              PERFORM FEA-KOLLA-AKS-FROM-WDL2                             
192700           END-IF                                                         
192800        END-IF                                                            
192900     END-IF                                                               
193000     .                                                                    
193100     EJECT                                                                
193200 FEA-KOLLA-AKS-FROM-WDL2   SECTION.                                       
193300     MOVE 'FEA-KOLLA-AKS   ' TO CURRENT-SECTION                           
193400                                                                          
193500     MOVE ZERO TO WS-KVRETUR                                              
193600     MOVE ZERO TO WS-KVINLEV                                              
193700     PERFORM IMS-GU-WDL201                                                
193800     IF SEGMENT-FOUND                                                     
193900        PERFORM IMS-GNP-WDL221                                            
194000        PERFORM UNTIL SEGMENT-MISSING OR                                  
194100                      WS-KVINLEV > ZERO                                   
194200           IF L2-MOT-IDPTYP = 'R31'                                       
194300              ADD L2-MOT-KVAVIS TO WS-KVINLEV                             
194400           END-IF                                                         
194500           PERFORM IMS-GNP-WDL221                                         
194600        END-PERFORM                                                       
194700     END-IF                                                               
194800                                                                          
194900     IF WS-KVINLEV = ZERO                                                 
195000        PERFORM IMS-GU-WDL201                                             
195100        IF SEGMENT-FOUND                                                  
195200           PERFORM IMS-GNP-WDL221                                         
195300           PERFORM UNTIL SEGMENT-MISSING OR                               
195400              WS-KVRETUR > ZERO                                           
195500              IF L2-MOT-IDPTYP = '310'                                    
195600                 IF L2-MOT-KDRT   = 7 OR 77                               
195700                    ADD L2-MOT-KVAVIS   TO WS-KVRETUR                     
195800                 END-IF                                                   
195900              END-IF                                                      
196000              PERFORM IMS-GNP-WDL221                                      
196100           END-PERFORM                                                    
196200           IF WS-KVRETUR > ZERO                                           
196300              MOVE MFS-ADD-LYS-UPP-FAELT                                  
196400                                 TO MOD-TIREGDAT-KOD-ATTR(INDX)           
196500              MOVE 'R' TO WS-KOD                                          
196600           END-IF                                                         
196700        END-IF                                                            
196800        IF WS-KVRETUR = ZERO                                              
196900           MOVE MFS-ADD-LYS-UPP-FAELT                                     
197000                                 TO MOD-TIREGDAT-ATTR(INDX)               
197100        END-IF                                                            
197200     ELSE                                                                 
197300        MOVE MFS-ADD-LYS-UPP-FAELT                                        
197400                                 TO MOD-TIREGDAT-ATTR(INDX)               
197500     END-IF                                                               
197600     .                                                                    
197700                                                                          
197800 G-CHECK-INPUT SECTION.                                                   
197900     MOVE 'G-CHECK-INPUT   ' TO CURRENT-SECTION                           
198000                                                                          
198100     MOVE JA    TO INDATA-SW                                              
198200     IF MID-INPUT = ALL '+'                                               
198300       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
198400       CALL WMEDKONV USING MED-WMEDAREA                                   
198500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
198600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
198700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
198800       MOVE NEJ TO INDATA-SW                                              
198900     ELSE                                                                 
199000       MOVE +1 TO INDX                                                    
199100       PERFORM UNTIL INDX > MAX-INDX                                      
199200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEVORMRK-ATTR(INDX)             
199300         IF MID-CMD(INDX) = ALL '+'                                       
199400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR(INDX)                
199500         ELSE                                                             
199600           IF MID-CMD(INDX) = 'D'                                         
199700             IF  MID-IDDISTR(INDX) = ALL '+'                              
199800               MOVE NEJ              TO INDATA-SW                         
199900               MOVE MFS-ALFA-FAELT-FEL                                    
200000                                     TO MOD-CMD-ATTR(INDX)                
200100             ELSE                                                         
200200               MOVE MFS-ALFA-FAELT-RAETT                                  
200300                                     TO MOD-CMD-ATTR(INDX)                
200400             END-IF                                                       
200500           ELSE                                                           
200600             MOVE NEJ                TO INDATA-SW                         
200700             MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR(INDX)                
200800           END-IF                                                         
200900         END-IF                                                           
201000         ADD 1 TO INDX                                                    
201100       END-PERFORM                                                        
201200                                                                          
201300       IF INDATA-FEL                                                      
201400         MOVE ERR-WRONG-SELECTION-CODE TO MED-IDMFSFEL                    
201500         CALL WMEDKONV USING MED-WMEDAREA                                 
201600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
201700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
201800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
201900       END-IF                                                             
202000     END-IF                                                               
202100     .                                                                    
202200     EJECT                                                                
202300 H-UPDATE SECTION.                                                        
202400     MOVE 'H-UPDATE        ' TO CURRENT-SECTION                           
202500                                                                          
202600     MOVE +1 TO INDX                                                      
202700     PERFORM UNTIL INDX > MAX-INDX                                        
202800       IF MID-CMD(INDX) = 'D'                                             
202900       OR MID-TEVORMRK(INDX) NOT = ALL '+'                                
203000         MOVE MID-IDDISTR(INDX)    TO W-IDDISTR-A601                      
203100         MOVE MID-IDKUNDNR(INDX) TO W-IDKUNDNR-A601                       
203200         INSPECT MID-IDORDNR7(INDX)                                       
203300                               REPLACING LEADING SPACE BY ZERO            
203400         MOVE MID-IDORDNR7(INDX) TO W-IDKUNDRF-A601                       
203500         MOVE SPAR-TIREGDAT-URSP-RAD(INDX)                                
203600                                 TO W-TIREGDAT-URSP-A601                  
203700         INSPECT MID-IDARTNR(INDX)                                        
203800                               REPLACING LEADING SPACE BY ZERO            
203900         MOVE MID-IDARTNR(INDX)    TO W-IDARTNR                           
204000         MOVE SPAR-TIREGTID-URSP-RAD(INDX)                                
204100                                 TO W-TIREGTID-URSP-A601                  
204200         MOVE SPAR-TIREGDAT-AVV-RAD(INDX)                                 
204300                                 TO W-TIREGDAT-AVV-A601                   
204400         MOVE SPAR-TIREGTID-AVV-RAD(INDX)                                 
204500                                 TO W-TIREGTID-AVV-A601                   
204600                                                                          
204700         PERFORM IMS-GHU-WDA601                                           
204800         IF SEGMENT-FOUND                                                 
204900           IF MID-CMD(INDX) = 'D'                                         
205000             MOVE INDX TO AKT-INDX                                        
205100             PERFORM HA-UPDATE-WDK9-WDR6                                  
205200             MOVE '8' TO VOR-KDVORATG                                     
205300             IF VOR-TIKLAR = ZERO                                         
205400                ACCEPT VOR-TIKLAR FROM DATE                               
205500                MOVE WS-NU-TID-6 TO VOR-TIKLATID                          
205600             END-IF                                                       
205700             MOVE 83 TO VOR-KDORDBEK                                      
205800             PERFORM IMS-REPL-WDA601                                      
205900             MOVE VOR-IDARTNR TO W-IDARTNR-K6                             
206000             PERFORM IMS-GHU-WDK611                                       
206100             IF SEGMENT-FOUND                                             
206200               COMPUTE CLAG-KVVORKO = CLAG-KVVORKO                        
206300                            - (VOR-KVBEART-Q - VOR-KVPREAVB)              
206400               PERFORM IMS-REPL-WDK6                                      
206500             END-IF                                                       
206600           ELSE                                                           
206700             IF MID-TEVORMRK(INDX) NOT = ALL '+'                          
206800               MOVE MID-TEVORMRK(INDX) TO VOR-TEVORMRK                    
206900               PERFORM IMS-REPL-WDA601                                    
207000             END-IF                                                       
207100           END-IF                                                         
207200         END-IF                                                           
207300       END-IF                                                             
207400       ADD 1 TO INDX                                                      
207500     END-PERFORM                                                          
207600                                                                          
207700     PERFORM S03-FIXA-SAMMA-SIDA                                          
207800                                                                          
207900     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
208000     CALL WMEDKONV USING MED-WMEDAREA                                     
208100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
208200     PERFORM MFS-FORM-ATTR                                                
208300     PERFORM MFS-RENSA-FAELT-IN                                           
208400     .                                                                    
208500     EJECT                                                                
208600 HA-UPDATE-WDK9-WDR6 SECTION.                                             
208700     MOVE 'HA-UPD-WDK9-WDR6' TO CURRENT-SECTION                           
208800                                                                          
208900*    --- MINSKA OKS (VOR-KÖ) PÅ WDK9                                      
209000                                                                          
209100     IF DCS-IDDC NOT = VOR-IDDC                                           
209200        MOVE VOR-IDDC TO W-IDDC-B6                                        
209300        PERFORM IMS-GU-WDB601                                             
209400     END-IF                                                               
209500     IF DCS-CDC                                                           
209600       INSPECT MID-IDARTNR(AKT-INDX)                                      
209700                            REPLACING LEADING SPACE BY ZERO               
209800       MOVE MID-IDARTNR(AKT-INDX) TO W-IDARTNR                            
209900       PERFORM IMS-GHU-WDK901                                             
210000       IF SEGMENT-FOUND                                                   
210100         COMPUTE ART-KVOKS-VOR = ART-KVOKS-VOR                            
210200                            - (VOR-KVBEART-Q - VOR-KVPREAVB)              
210300         PERFORM IMS-REPL-WDK901                                          
210400       ELSE                                                               
210500         MOVE ERR-PART-MISSING TO MED-IDMFSFEL                            
210600         CALL WMEDKONV USING MED-WMEDAREA                                 
210700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
210800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
210900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
211000         MOVE NEJ TO INDATA-SW                                            
211100       END-IF                                                             
211200     ELSE                                                                 
211300       IF VOR-KDORDBEK = 92 OR 93                                         
211400         MOVE VOR-IDDC TO W-IDDC                                          
211500         PERFORM IMS-GHU-WDK711                                           
211600         IF SEGMENT-FOUND                                                 
211700           COMPUTE SLAG-KVOKS-DAG = SLAG-KVOKS-DAG                        
211800                            - (VOR-KVBEART-Q - VOR-KVPREAVB)              
211900           PERFORM IMS-REPL-WDK711                                        
212000         ELSE                                                             
212100           MOVE ERR-PART-MISSING TO MED-IDMFSFEL                          
212200           CALL WMEDKONV USING MED-WMEDAREA                               
212300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
212400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
212500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
212600           MOVE NEJ TO INDATA-SW                                          
212700         END-IF                                                           
212800       END-IF                                                             
212900     END-IF                                                               
213000*    --- SKRIV LOGGPOST PÅ WDR6                                           
213100     MOVE IDPGM             TO FIL-IDPGM                                  
213200     ACCEPT FIL-TIREGDAT    FROM DATE                                     
213300     ACCEPT FIL-TIKLOCK     FROM TIME                                     
213400     MOVE ZERO              TO FIL-IDSEKVNR                               
213500     MOVE 'W414'            TO FIL-CT-IDSYSTEM                            
213600     MOVE 'A'               TO FIL-CT-IDVTYP                              
213700     MOVE '205'             TO FIL-CT-IDPTYP                              
213800     ADD 1                  TO FIL-IDSEKVNR                               
213900     MOVE VOR-IDARTNR       TO 205-IDARTNR                                
214000     MOVE VOR-IDDC          TO 205-IDDC                                   
214100     MOVE VOR-IDDISTR       TO 205-IDDISTR                                
214200     MOVE VOR-IDKUNDNR      TO 205-IDKUNDNR                               
214300     MOVE VOR-IDKUNDRF      TO 205-IDKUNDRF                               
214400     MOVE VOR-KVPREAVB      TO 205-KVAVBART                               
214500     MOVE VOR-KVBEART-Q     TO 205-KVBEART-Q                              
214600     MOVE NEJ               TO 205-FLDIRLEV                               
214700     MOVE VOR-TIREGDAT-URSP TO 205-TIREGDAT                               
214800     MOVE VOR-TIREGTID-URSP TO 205-TIREGTID                               
214900                                                                          
215000     PERFORM IMS-ISRT-WDR601                                              
215100     IF SEGMENT-FOUND-EXISTS                                              
215200       PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                             
215300         ADD 1 TO FIL-IDSEKVNR                                            
215400         PERFORM IMS-ISRT-WDR601                                          
215500       END-PERFORM                                                        
215600     END-IF                                                               
215700     .                                                                    
215800     EJECT                                                                
215900 I-SPLIT-SCREEN SECTION.                                                  
216000     MOVE 'I-SPLIT-SCREEN  ' TO CURRENT-SECTION                           
216100                                                                          
216200     MOVE JA  TO W-FIRST-NFST                                             
216300     MOVE +1 TO INDX                                                      
216400     PERFORM UNTIL INDX > MAX-INDX                                        
216500       IF MID-CMD(INDX) = 'F' OR 'N'                                      
216600         MOVE INDX TO AKT-INDX                                            
216700         PERFORM IA-SPLIT-TO-4221                                         
216800         MOVE NEJ TO W-FIRST-NFST                                         
216900       ELSE                                                               
217000         IF MID-CMD(INDX) = 'S'                                           
217100           MOVE INDX TO AKT-INDX                                          
217200           PERFORM IB-SPLIT-TO-4231                                       
217300           MOVE NEJ TO W-FIRST-NFST                                       
217400         ELSE                                                             
217500           IF MID-CMD(INDX) = 'T'                                         
217600             MOVE INDX TO AKT-INDX                                        
217700             PERFORM IC-SPLIT-TO-4275                                     
217800             MOVE +14 TO INDX                                             
217900           END-IF                                                         
218000         END-IF                                                           
218100       END-IF                                                             
218200       ADD 1 TO INDX                                                      
218300     END-PERFORM                                                          
218400     .                                                                    
218500     EJECT                                                                
218600 IA-SPLIT-TO-4221 SECTION.                                                
218700     MOVE 'IA-SPLIT-TO-4221' TO CURRENT-SECTION                           
218800                                                                          
218900     MOVE MID-IDDISTR(AKT-INDX) TO W-IDDISTR-A601                         
219000     MOVE MID-IDKUNDNR(AKT-INDX) TO W-IDKUNDNR-A601                       
219100     INSPECT MID-IDORDNR7(AKT-INDX)                                       
219200                              REPLACING LEADING SPACE BY ZERO             
219300     MOVE MID-IDORDNR7(AKT-INDX) TO W-IDKUNDRF-A601                       
219400     MOVE SPAR-TIREGDAT-URSP-RAD(AKT-INDX)                                
219500                                 TO W-TIREGDAT-URSP-A601                  
219600     INSPECT MID-IDARTNR(AKT-INDX)                                        
219700                              REPLACING LEADING SPACE BY ZERO             
219800     MOVE MID-IDARTNR(AKT-INDX) TO W-IDARTNR                              
219900     MOVE SPAR-TIREGTID-URSP-RAD(AKT-INDX)                                
220000                                 TO W-TIREGTID-URSP-A601                  
220100     MOVE SPAR-TIREGDAT-AVV-RAD(AKT-INDX)                                 
220200                                 TO W-TIREGDAT-AVV-A601                   
220300     MOVE SPAR-TIREGTID-AVV-RAD(AKT-INDX)                                 
220400                                 TO W-TIREGTID-AVV-A601                   
220500                                                                          
220600     PERFORM IMS-GHU-WDA601                                               
220700     IF SEGMENT-MISSING                                                   
220800       IF W-FIRST-NFST = JA                                               
220900         MOVE ERR-KEYS-ARE-MISSING TO MED-IDMFSFEL                        
221000         CALL WMEDKONV USING MED-WMEDAREA                                 
221100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
221200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
221300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
221400         MOVE NEJ TO INDATA-SW                                            
221500       END-IF                                                             
221600     ELSE                                                                 
221700       IF  W-FIRST-NFST = JA                                              
221800         MOVE '1' TO VOR-KDVORATG                                         
221900         MOVE '**' TO VOR-TEVORMRK                                        
222000         MOVE MSG-SIGNON-USERID TO VOR-IDUSER                             
222100         MOVE MSGI-TILOKDAT TO VOR-TIUPPDAT                               
222200         MOVE MSGI-TILOKTID TO VOR-TIUPPTID                               
222300         PERFORM IMS-REPL-WDA601                                          
222400*      --- SKAPA MID FÖR FÖRBI (FLFORBI=J) & NORMAL ORDER                 
222500         COMPUTE MSG-KVLL = LENGTH OF 4221-MID-W4I22101 + 17              
222600         MOVE LOW-VALUE TO MSG-KDZ1                                       
222700                           MSG-KDZ2                                       
222800         MOVE 'W4T221' TO MSG-KDTRANS-1                                   
222900         MOVE '4225' TO MSG-IDTRANS-1                                     
223000         MOVE '2' TO MSG-KDMFSFOR-1                                       
223100         MOVE JA TO 4221-MID-FLVORKO                                      
223200         MOVE MID-IDORDNR7(INDX)(3:5)                                     
223201                      TO 4221-MID-BEKUNDRF                                
223210         MOVE ALL '+' TO 4221-MID-IDORDNR5                                
223300         MOVE '0' TO 4221-MID-KDORDKL                                     
223400         MOVE ALL '+' TO 4221-MID-TIRFS-DAT                               
223500         MOVE ALL '+' TO 4221-MID-TIRFS-TID                               
223600                                                                          
223700         MOVE JA TO P2P-SW                                                
223800         MOVE MID-IDDISTR(AKT-INDX) TO 4221-MID-IDDISTR                   
223900         INSPECT 4221-MID-IDDISTR REPLACING LEADING SPACE BY ZERO         
224000                                                                          
224100         MOVE MID-IDKUNDNR(AKT-INDX) TO 4221-MID-IDKUNDNR                 
224200         INSPECT 4221-MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO        
224300                                                                          
224400         IF MID-CMD(INDX) = 'F'                                           
224500           MOVE JA TO 4221-MID-FLFORBI                                    
224600         ELSE                                                             
224700           MOVE NEJ TO 4221-MID-FLFORBI                                   
224800         END-IF                                                           
224900                                                                          
225000         PERFORM IMS-INSERT-4221-MSG                                      
225100       ELSE                                                               
225200         IF  P2P                                                          
225300           MOVE '1' TO VOR-KDVORATG                                       
225400           MOVE '**' TO VOR-TEVORMRK                                      
225500           MOVE MSG-SIGNON-USERID TO VOR-IDUSER                           
225600           MOVE MSGI-TILOKDAT TO VOR-TIUPPDAT                             
225700           MOVE MSGI-TILOKTID TO VOR-TIUPPTID                             
225800           PERFORM IMS-REPL-WDA601                                        
225900         END-IF                                                           
226000       END-IF                                                             
226100     END-IF                                                               
226200     .                                                                    
226300     EJECT                                                                
226400 IB-SPLIT-TO-4231 SECTION.                                                
226500     MOVE 'IB-SPLIT-TO-4231' TO CURRENT-SECTION                           
226600                                                                          
226700     MOVE MID-IDDISTR(AKT-INDX) TO W-IDDISTR-A601                         
226800     MOVE MID-IDKUNDNR(AKT-INDX) TO W-IDKUNDNR-A601                       
226900     INSPECT MID-IDORDNR7(AKT-INDX)                                       
227000                              REPLACING LEADING SPACE BY ZERO             
227100     MOVE MID-IDORDNR7(AKT-INDX) TO W-IDKUNDRF-A601                       
227200     MOVE SPAR-TIREGDAT-URSP-RAD(AKT-INDX)                                
227300                                 TO W-TIREGDAT-URSP-A601                  
227400     INSPECT MID-IDARTNR(AKT-INDX)                                        
227500                              REPLACING LEADING SPACE BY ZERO             
227600     MOVE MID-IDARTNR(AKT-INDX) TO W-IDARTNR                              
227700     MOVE SPAR-TIREGTID-URSP-RAD(AKT-INDX)                                
227800                                 TO W-TIREGTID-URSP-A601                  
227900     MOVE SPAR-TIREGDAT-AVV-RAD(AKT-INDX)                                 
228000                                 TO W-TIREGDAT-AVV-A601                   
228100     MOVE SPAR-TIREGTID-AVV-RAD(AKT-INDX)                                 
228200                                 TO W-TIREGTID-AVV-A601                   
228300                                                                          
228400     PERFORM IMS-GHU-WDA601                                               
228500     IF SEGMENT-MISSING                                                   
228600       IF W-FIRST-NFST = JA                                               
228700         MOVE ERR-KEYS-ARE-MISSING TO MED-IDMFSFEL                        
228800         CALL WMEDKONV USING MED-WMEDAREA                                 
228900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
229000         PERFORM MFS-ROER-EJ-FAELT-UT                                     
229100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
229200         MOVE NEJ TO INDATA-SW                                            
229300       END-IF                                                             
229400     ELSE                                                                 
229500       IF  W-FIRST-NFST = JA                                              
229600         MOVE '1' TO VOR-KDVORATG                                         
229700         MOVE '**' TO VOR-TEVORMRK                                        
229800         MOVE MSG-SIGNON-USERID TO VOR-IDUSER                             
229900         MOVE MSGI-TILOKDAT TO VOR-TIUPPDAT                               
230000         MOVE MSGI-TILOKTID TO VOR-TIUPPTID                               
230100         PERFORM IMS-REPL-WDA601                                          
230200*    --- SKAPA MID FÖR SPECIALORDER                                       
230300         COMPUTE MSG-KVLL = LENGTH OF 4231-MID-W4I23101 + 17              
230400         MOVE LOW-VALUE TO MSG-KDZ1                                       
230500                           MSG-KDZ2                                       
230600         MOVE 'W4T231' TO MSG-KDTRANS-1                                   
230700         MOVE '4225' TO MSG-IDTRANS-1                                     
230800         MOVE '2' TO MSG-KDMFSFOR-1                                       
230900         MOVE JA TO 4231-MID-FLVORKO                                      
231000         MOVE ALL '+' TO 4231-MID-IDORDNR                                 
231100         MOVE '0' TO 4231-MID-KDORDKL                                     
231200                                                                          
231300         MOVE JA TO P2P-SW                                                
231400         MOVE MID-IDDISTR(AKT-INDX) TO 4231-MID-IDDISTR                   
231500         INSPECT 4231-MID-IDDISTR REPLACING LEADING SPACE BY ZERO         
231600                                                                          
231700         MOVE MID-IDKUNDNR(AKT-INDX) TO 4231-MID-IDKUNDNR                 
231800         INSPECT 4231-MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO        
231900                                                                          
232000         PERFORM IMS-INSERT-4231-MSG                                      
232100       ELSE                                                               
232200         IF  P2P                                                          
232300           MOVE '1' TO VOR-KDVORATG                                       
232400           MOVE '**' TO VOR-TEVORMRK                                      
232500           MOVE MSG-SIGNON-USERID TO VOR-IDUSER                           
232600           MOVE MSGI-TILOKDAT TO VOR-TIUPPDAT                             
232700           MOVE MSGI-TILOKTID TO VOR-TIUPPTID                             
232800           PERFORM IMS-REPL-WDA601                                        
232900         END-IF                                                           
233000       END-IF                                                             
233100     END-IF                                                               
233200     .                                                                    
233300     EJECT                                                                
233400 IC-SPLIT-TO-4275 SECTION.                                                
233410     MOVE 'IC-SPLIT-TO-4275' TO CURRENT-SECTION                           
233420                                                                          
233430     MOVE MID-IDDISTR(AKT-INDX) TO W-IDDISTR-A601                         
233440     MOVE MID-IDKUNDNR(AKT-INDX) TO W-IDKUNDNR-A601                       
233450     INSPECT MID-IDORDNR7(AKT-INDX)                                       
233460                              REPLACING LEADING SPACE BY ZERO             
233470     MOVE MID-IDORDNR7(AKT-INDX) TO W-IDKUNDRF-A601                       
233480     MOVE SPAR-TIREGDAT-URSP-RAD(AKT-INDX)                                
233490                                 TO W-TIREGDAT-URSP-A601                  
233491     INSPECT MID-IDARTNR(AKT-INDX)                                        
233492                              REPLACING LEADING SPACE BY ZERO             
233493     MOVE MID-IDARTNR(AKT-INDX) TO W-IDARTNR                              
233494     MOVE SPAR-TIREGTID-URSP-RAD(AKT-INDX)                                
233495                                 TO W-TIREGTID-URSP-A601                  
233496     MOVE SPAR-TIREGDAT-AVV-RAD(AKT-INDX)                                 
233497                                 TO W-TIREGDAT-AVV-A601                   
233498     MOVE SPAR-TIREGTID-AVV-RAD(AKT-INDX)                                 
233499                                 TO W-TIREGTID-AVV-A601                   
233500                                                                          
233501     PERFORM IMS-GHU-WDA601                                               
233502     IF SEGMENT-MISSING                                                   
233504         MOVE ERR-KEYS-ARE-MISSING TO MED-IDMFSFEL                        
233505         CALL WMEDKONV USING MED-WMEDAREA                                 
233506         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
233507         PERFORM MFS-ROER-EJ-FAELT-UT                                     
233508         PERFORM MFS-ROER-EJ-FAELT-IN                                     
233509         MOVE NEJ TO INDATA-SW                                            
233511     ELSE                                                                 
233700*    --- SKAPA MID FÖR TEXTBILD 4275                                      
233800         COMPUTE MSG-KVLL = LENGTH OF 4275-MID-W4I27501-CTX + 17          
233900         MOVE LOW-VALUE TO MSG-KDZ1                                       
234000                           MSG-KDZ2                                       
234100         MOVE 'W4T275' TO MSG-KDTRANS-1                                   
234200         MOVE '4225' TO MSG-IDTRANS-1                                     
234300         MOVE '1' TO MSG-KDMFSFOR-1                                       
234400                                                                          
234500         MOVE JA TO P2P-SW                                                
234600         MOVE MID-IDDISTR(AKT-INDX) TO 4275-MID-IDDISTR                   
234700         INSPECT 4275-MID-IDDISTR REPLACING LEADING SPACE BY ZERO         
234800                                                                          
234900         MOVE MID-IDKUNDNR(AKT-INDX) TO 4275-MID-IDKUNDNR                 
235000         INSPECT  4275-MID-IDKUNDNR                                       
235100                               REPLACING LEADING SPACE BY ZERO            
235200         MOVE MID-IDORDNR7(AKT-INDX)TO 4275-MID-IDORDNR7                  
235300         INSPECT 4275-MID-IDORDNR7 REPLACING LEADING SPACE BY ZERO        
235400                                                                          
235500         MOVE SPAR-TIREGDAT-URSP-RAD(AKT-INDX)                            
235600                                     TO 4275-MID-TIREGDAT-URSP            
235700         INSPECT 4275-MID-TIREGDAT-URSP                                   
235800                              REPLACING LEADING SPACE BY ZERO             
235900                                                                          
236000         MOVE MID-IDARTNR(AKT-INDX) TO 4275-MID-IDARTNR                   
236100         INSPECT 4275-MID-IDARTNR REPLACING LEADING SPACE BY ZERO         
236200                                                                          
236300         MOVE SPAR-TIREGTID-URSP-RAD(AKT-INDX)                            
236400                                     TO 4275-MID-TIREGTID-URSP            
236500         INSPECT 4275-MID-TIREGTID-URSP                                   
236600                              REPLACING LEADING SPACE BY ZERO             
236700                                                                          
236800         MOVE SPAR-TIREGDAT-AVV-RAD(AKT-INDX)                             
236900                                     TO 4275-MID-TIREGDAT-AVV             
237000         INSPECT 4275-MID-TIREGDAT-AVV                                    
237100                              REPLACING LEADING SPACE BY ZERO             
237200                                                                          
237300         MOVE SPAR-TIREGTID-AVV-RAD(AKT-INDX)                             
237400                                     TO 4275-MID-TIREGTID-AVV             
237500         INSPECT 4275-MID-TIREGTID-AVV                                    
237600                              REPLACING LEADING SPACE BY ZERO             
237700         MOVE ALL '+'            TO 4275-MID-FLLAEST                      
237800                                    4275-MID-TEVORINT                     
237900                                    4275-MID-TEVOREXT                     
238000                                    4275-MID-TELOSNOT                     
238100                                                                          
238200         PERFORM IMS-INSERT-4275-MSG                                      
238292     END-IF                                                               
238300     .                                                                    
238400     EJECT                                                                
238500 J-CHECK-INPUT SECTION.                                                   
238600     MOVE 'J-CHECK-INPUT   ' TO CURRENT-SECTION                           
238700                                                                          
238800     MOVE JA    TO INDATA-SW                                              
238900     MOVE JA    TO W-FIRST-NFST                                           
239000     MOVE SPACE TO W-KOLL-NFST                                            
239100     IF MID-INPUT = ALL '+'                                               
239200       MOVE ERR-PF9-AND-NO-DATA TO MED-IDMFSFEL                           
239300*      CALL WMEDKONV USING MED-WMEDAREA                                   
239400*      MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
239500       MOVE 'PF9 AND NO DATA' TO MOD-TEMFSFEL                             
239600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
239700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
239800       MOVE NEJ TO INDATA-SW                                              
239900     ELSE                                                                 
240000       MOVE +1 TO INDX                                                    
240100       PERFORM UNTIL INDX > MAX-INDX                                      
240200         IF MID-CMD(INDX) = ALL '+'                                       
240300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR(INDX)                
240400         ELSE                                                             
240500           IF MID-CMD(INDX) = 'N'                                         
240600           OR MID-CMD(INDX) = 'F'                                         
240700           OR MID-CMD(INDX) = 'S'                                         
240800           OR MID-CMD(INDX) = 'T'                                         
240900             IF  MID-IDDISTR(INDX) = ALL '+'                              
241000               MOVE NEJ                  TO INDATA-SW                     
241100               MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-ATTR(INDX)            
241200             ELSE                                                         
241300               IF  W-FIRST-NFST = JA                                      
241400                 MOVE NEJ                TO W-FIRST-NFST                  
241500                 MOVE MID-CMD(INDX)      TO W-KOLL-NFST                   
241600                 MOVE MID-IDDISTR(INDX)  TO W-KOLL-DISTR                  
241700                 MOVE MID-IDKUNDNR(INDX) TO W-KOLL-KUND                   
241800                 MOVE MFS-ALFA-FAELT-RAETT                                
241900                                         TO MOD-CMD-ATTR(INDX)            
242000               ELSE                                                       
242100                 IF  MID-CMD(INDX) = 'T'                                  
242200                   MOVE NEJ                TO INDATA-SW                   
242300                   MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR(INDX)          
242400                 ELSE                                                     
242500                   IF MID-CMD(INDX)      NOT = W-KOLL-NFST                
242600                   OR MID-IDDISTR(INDX)  NOT = W-KOLL-DISTR               
242700                   OR MID-IDKUNDNR(INDX) NOT = W-KOLL-KUND                
242800                     MOVE NEJ              TO INDATA-SW                   
242900                     MOVE MFS-ALFA-FAELT-FEL                              
243000                                           TO MOD-CMD-ATTR(INDX)          
243100                   ELSE                                                   
243200                     MOVE MFS-ALFA-FAELT-RAETT                            
243300                                           TO MOD-CMD-ATTR(INDX)          
243400                   END-IF                                                 
243500                 END-IF                                                   
243600               END-IF                                                     
243700             END-IF                                                       
243800           ELSE                                                           
243900             MOVE NEJ                TO INDATA-SW                         
244000             MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR(INDX)                
244100           END-IF                                                         
244200         END-IF                                                           
244300         ADD 1 TO INDX                                                    
244400       END-PERFORM                                                        
244500                                                                          
244600       IF INDATA-FEL                                                      
244700         MOVE ERR-WRONG-SELECTION-CODE TO MED-IDMFSFEL                    
244800         CALL WMEDKONV USING MED-WMEDAREA                                 
244900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
245000         PERFORM MFS-ROER-EJ-FAELT-UT                                     
245100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
245200       END-IF                                                             
245300     END-IF                                                               
245400     .                                                                    
245500     EJECT                                                                
245600 K-CREATE-REPORT  SECTION.                                                
245700     MOVE 'K-CREATE-REPORT ' TO CURRENT-SECTION                           
245800                                                                          
245900*                                                                         
246000*  STARTA BMP W440S4                                                      
246100*                                                                         
246200     MOVE '4225'          TO MSGSOP-IDTRANS                               
246300     MOVE MFS-KDMFSFOR    TO MSGSOP-KDMFSFOR                              
246400     MOVE 'W440S4'        TO MSGSOP-IDPROCESS                             
246500     MOVE 'O'             TO MSGSOP-KDSOPFUNK                             
246600                                                                          
246700*    STRING 'IDROLL(' MOD-IDROLL ')'                                      
246800*         DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
246900     PERFORM IMS-INSERT-ALTMSG                                            
247000     .                                                                    
247100     EJECT                                                                
247200 S01-CALL-CEEISEC SECTION.                                                
247300     MOVE 'S01-CALL-CEEISEC' TO CURRENT-SECTION                           
247400                                                                          
247500     IF  WS-YEAR > 50                                                     
247600       MOVE 1900 TO YEAR                                                  
247700     ELSE                                                                 
247800       MOVE 2000 TO YEAR                                                  
247900     END-IF                                                               
248000     ADD  WS-YEAR  TO YEAR                                                
248100     MOVE WS-MONTH TO MONTH                                               
248200     MOVE WS-DAYS TO DAYS                                                 
248300     MOVE WS-HOURS TO HOURS                                               
248400     MOVE WS-MINUTES TO MINUTES                                           
248500     MOVE 0 TO SECONDS                                                    
248600     MOVE 0 TO MILLSEC                                                    
248700                                                                          
248800     CALL CEEISEC USING YEAR, MONTH, DAYS,                                
248900     HOURS, MINUTES, SECONDS, MILLSEC, OUTSEC, FBC                        
249000                                                                          
249100     IF SEV = 0                                                           
249200       MOVE OUTSEC TO OUTSEC-SVAR                                         
249300     ELSE                                                                 
249400       STRING ' CEEISEC FAILED '                                          
249500       DELIMITED BY SIZE INTO FELTEXT                                     
249600       CALL FELLOG                                                        
249700     END-IF                                                               
249800     .                                                                    
249900     EJECT                                                                
250000 S02-FIX-A6SEQ-KEYS SECTION.                                              
250100     MOVE 'S02-FIX-A6SEQ-KY' TO CURRENT-SECTION                           
250200                                                                          
250300     IF WS-ENTRY = 'C'                                                    
250400       MOVE MSGI-IDROLL     TO W-IDROLL-CSEQ-MIN                          
250500                               W-IDROLL-CSEQ-MAX                          
250600     END-IF                                                               
250700     IF WS-ENTRY = 'D'                                                    
250800       MOVE MSGI-IDROLL     TO W-IDROLL-DSEQ-MIN                          
250900                               W-IDROLL-DSEQ-MAX                          
251000       IF MSGI-IDARTNR = SPACE OR ZERO                                    
251100          MOVE ZERO         TO W-IDARTNR-DSEQ-MIN                         
251200          MOVE 999999999    TO W-IDARTNR-DSEQ-MAX                         
251300       ELSE                                                               
251400          MOVE MSGI-IDARTNR TO W-IDARTNR-DSEQ-MIN                         
251500                               W-IDARTNR-DSEQ-MAX                         
251600       END-IF                                                             
251700     END-IF                                                               
251800     IF WS-ENTRY = 'E'                                                    
251900       MOVE MSGI-IDDISTR    TO W-IDDISTR-ESEQ-MIN                         
252000                               W-IDDISTR-ESEQ-MAX                         
252100     END-IF                                                               
252200     IF WS-ENTRY = 'F'                                                    
252300       MOVE MSGI-IDDISTR    TO W-IDDISTR-FSEQ-MIN                         
252400                               W-IDDISTR-FSEQ-MAX                         
252500       MOVE MSGI-IDKUNDNR   TO W-IDKUNDNR-FSEQ-MIN                        
252600                               W-IDKUNDNR-FSEQ-MAX                        
252700     END-IF                                                               
252800     IF WS-ENTRY = 'G'                                                    
252900       MOVE MSGI-IDDISTR    TO W-IDDISTR-GSEQ-MIN                         
253000                               W-IDDISTR-GSEQ-MAX                         
253100       MOVE MSGI-IDKUNDNR   TO W-IDKUNDNR-GSEQ-MIN                        
253200                               W-IDKUNDNR-GSEQ-MAX                        
253300       MOVE MSGI-IDARTNR    TO W-IDARTNR-GSEQ-MIN                         
253400                               W-IDARTNR-GSEQ-MAX                         
253500     END-IF                                                               
253600     IF WS-ENTRY = 'H'                                                    
253700       MOVE MSGI-IDDISTR    TO W-IDDISTR-HSEQ-MIN                         
253800                               W-IDDISTR-HSEQ-MAX                         
253900       MOVE MSGI-IDARTNR    TO W-IDARTNR-HSEQ-MIN                         
254000                               W-IDARTNR-HSEQ-MAX                         
254100     END-IF                                                               
254200     IF WS-ENTRY = 'I'                                                    
254300       MOVE MSGI-IDANSK     TO W-IDANSK-ISEQ-MIN                          
254400                               W-IDANSK-ISEQ-MAX                          
254500     END-IF                                                               
254600     IF WS-ENTRY = 'J'                                                    
254700       MOVE MSGI-IDARTNR    TO W-IDARTNR-JSEQ-MIN                         
254800                               W-IDARTNR-JSEQ-MAX                         
254900     END-IF                                                               
255000     .                                                                    
255100     EJECT                                                                
255200 S03-FIXA-SAMMA-SIDA SECTION.                                             
255300     MOVE 'S03-FIX-SAMMA-SI' TO CURRENT-SECTION                           
255400                                                                          
255500     EVALUATE SPAR-ENTRY                                                  
255600     WHEN 'C'                                                             
255700        MOVE SPAR-WDA6CSEQ-ENTER        TO W-WDA6CSEQ-MIN-X               
255800        MOVE SPAR-IDROLL-CSEQ-ENTER     TO W-IDROLL-CSEQ-MAX              
255900                                                                          
256000     WHEN 'D'                                                             
256100        MOVE SPAR-WDA6DSEQ-ENTER        TO W-WDA6DSEQ-MIN-X               
256200        MOVE SPAR-IDROLL-DSEQ-ENTER     TO W-IDROLL-DSEQ-MAX              
256300        IF MSGI-KDSORT = 'PA'                                             
256400           MOVE +999999999              TO W-IDARTNR-DSEQ-MAX             
256500        ELSE                                                              
256600           MOVE SPAR-IDARTNR-DSEQ-ENTER TO W-IDARTNR-DSEQ-MAX             
256700        END-IF                                                            
256800                                                                          
256900     WHEN 'E'                                                             
257000        MOVE SPAR-WDA6ESEQ-ENTER        TO W-WDA6ESEQ-MIN-X               
257100        MOVE SPAR-IDDISTR-ESEQ-ENTER    TO W-IDDISTR-ESEQ-MAX             
257200                                                                          
257300     WHEN 'F'                                                             
257400        MOVE SPAR-WDA6FSEQ-ENTER        TO W-WDA6FSEQ-MIN-X               
257500        MOVE SPAR-IDDISTR-FSEQ-ENTER    TO W-IDDISTR-FSEQ-MAX             
257600        MOVE SPAR-IDKUNDNR-FSEQ-ENTER   TO W-IDKUNDNR-FSEQ-MAX            
257700                                                                          
257800     WHEN 'G'                                                             
257900        MOVE SPAR-WDA6GSEQ-ENTER        TO W-WDA6GSEQ-MIN-X               
258000        MOVE SPAR-IDDISTR-GSEQ-ENTER    TO W-IDDISTR-GSEQ-MAX             
258100        MOVE SPAR-IDKUNDNR-GSEQ-ENTER   TO W-IDKUNDNR-GSEQ-MAX            
258200        MOVE SPAR-IDARTNR-GSEQ-ENTER    TO W-IDARTNR-GSEQ-MAX             
258300                                                                          
258400     WHEN 'H'                                                             
258500        MOVE SPAR-WDA6HSEQ-ENTER        TO W-WDA6HSEQ-MIN-X               
258600        MOVE SPAR-IDDISTR-HSEQ-ENTER    TO W-IDDISTR-HSEQ-MAX             
258700        MOVE SPAR-IDARTNR-HSEQ-ENTER    TO W-IDARTNR-HSEQ-MAX             
258800                                                                          
258900     WHEN 'I'                                                             
259000        MOVE SPAR-WDA6ISEQ-ENTER        TO W-WDA6ISEQ-MIN-X               
259100        MOVE SPAR-IDANSK-ISEQ-ENTER     TO W-IDANSK-ISEQ-MAX              
259200                                                                          
259300     WHEN 'J'                                                             
259400        MOVE SPAR-WDA6JSEQ-ENTER        TO W-WDA6JSEQ-MIN-X               
259500        MOVE SPAR-IDARTNR-JSEQ-ENTER    TO W-IDARTNR-JSEQ-MAX             
259600     END-EVALUATE                                                         
259700     MOVE SPAR-ENTRY TO WS-ENTRY                                          
259800     .                                                                    
259900     EJECT                                                                
260000 MFS-RENSA-FAELT-IN SECTION.                                              
260100                                                                          
260200*    --- ALLA INDATA-FÄLT                                                 
260300     MOVE +1 TO INDX                                                      
260400     PERFORM UNTIL INDX > MAX-INDX                                        
260500     MOVE MFS-RENSA-FAELT TO MOD-CMD(INDX)                                
260600                             MOD-TEVORMRK(INDX)                           
260700     ADD 1 TO INDX                                                        
260800     END-PERFORM                                                          
260900     .                                                                    
261000     EJECT                                                                
261100 MFS-RENSA-FAELT-UT SECTION.                                              
261200                                                                          
261300     MOVE +1 TO INDX                                                      
261400     PERFORM UNTIL INDX > MAX-INDX                                        
261500       MOVE MFS-RENSA-FAELT TO MOD-CMD(INDX)                              
261600                               MOD-TIREGDAT(INDX)                         
261700                               MOD-TIREGDAT-KOD(INDX)                     
261800                               MOD-TEVORMRK(INDX)                         
261900                               MOD-IDDISTR(INDX)                          
262000                               MOD-IDKUNDNR(INDX)                         
262100                               MOD-IDORDNR7(INDX)                         
262200                               MOD-IDANSK(INDX)                           
262300                               MOD-IDARTNR(INDX)                          
262400                               MOD-KVBEART(INDX)                          
262500                               MOD-DIFF(INDX)                             
262600                               MOD-KDORDBEK(INDX)                         
262700                               MOD-IDDC(INDX)                             
262800                               MOD-TEVORTXT-FL(INDX)                      
262900                               MOD-TEVORSC-FL(INDX)                       
263000                               MOD-TEVORNOT-FL(INDX)                      
263100                               MOD-TELOSNOT-FL(INDX)                      
263200       ADD 1 TO INDX                                                      
263300     END-PERFORM                                                          
263400     .                                                                    
263500     SKIP3                                                                
263600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
263700                                                                          
263800*    --- ALLA INDATA-FÄLT                                                 
263900     MOVE +1 TO INDX                                                      
264000     PERFORM UNTIL INDX > MAX-INDX                                        
264100       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD(INDX)                            
264200                                 MOD-TEVORMRK(INDX)                       
264300       ADD 1 TO INDX                                                      
264400     END-PERFORM                                                          
264500     .                                                                    
264600     EJECT                                                                
264700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
264800                                                                          
264900*    --- UTDATA-FÄLT (BLÄDDRINGSRADER)                                    
265000     MOVE +1 TO INDX                                                      
265100     PERFORM UNTIL INDX > MAX-INDX                                        
265200       MOVE MFS-ROER-EJ-FAELT TO MOD-TIREGDAT(INDX)                       
265300                                 MOD-TIREGDAT-KOD(INDX)                   
265400                                 MOD-TEVORMRK(INDX)                       
265500                                 MOD-IDDISTR(INDX)                        
265600                                 MOD-IDKUNDNR(INDX)                       
265700                                 MOD-IDORDNR7(INDX)                       
265800                                 MOD-IDANSK(INDX)                         
265900                                 MOD-IDARTNR(INDX)                        
266000                                 MOD-KVBEART(INDX)                        
266100                                 MOD-DIFF(INDX)                           
266200                                 MOD-KDORDBEK(INDX)                       
266300                                 MOD-IDDC(INDX)                           
266400                                 MOD-TEVORTXT-FL(INDX)                    
266500                                 MOD-TEVORSC-FL(INDX)                     
266600                                 MOD-TEVORNOT-FL(INDX)                    
266700                                 MOD-TELOSNOT-FL(INDX)                    
266800       ADD 1 TO INDX                                                      
266900     END-PERFORM                                                          
267000     .                                                                    
267100     SKIP3                                                                
267200 MFS-FORM-ATTR SECTION.                                                   
267300                                                                          
267400*    --- ALLA INDATA-FÄLT                                                 
267500     MOVE +1 TO INDX                                                      
267600     PERFORM UNTIL INDX > MAX-INDX                                        
267700       MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR(INDX)                      
267800                                  MOD-TIREGDAT-ATTR(INDX)                 
267900                                  MOD-TIREGDAT-KOD-ATTR(INDX)             
268000                                  MOD-TEVORMRK-ATTR(INDX)                 
268100                                  MOD-TEVORSC-FL-ATTR(INDX)               
268200                                  MOD-IDDISTR-ATTR(INDX)                  
268300                                  MOD-IDKUNDNR-ATTR(INDX)                 
268400                                  MOD-IDORDNR7-ATTR(INDX)                 
268500     ADD 1 TO INDX                                                        
268600     END-PERFORM                                                          
268700     .                                                                    
268800     EJECT                                                                
268900* --- IMS SEKTIONER                                                       
269000 IMS-GET-MSG SECTION.                                                     
269100                                                                          
269200     MOVE '  QC' TO GODK-STATUSKODER                                      
269300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
269400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
269500     PERFORM IMS-STATUSKONTROLL                                           
269600     .                                                                    
269700     EJECT                                                                
269800 IMS-INSERT-MSG SECTION.                                                  
269900                                                                          
270000     MOVE SPACE TO GODK-STATUSKODER                                       
270100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
270200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
270300     PERFORM IMS-STATUSKONTROLL                                           
270400     .                                                                    
270500     EJECT                                                                
270600 IMS-INSERT-4221-MSG SECTION.                                             
270700                                                                          
270800     MOVE SPACE TO GODK-STATUSKODER                                       
270900     CALL CBLTDLI USING ISRT 4221-PCB MSG-IO-AREA                         
271000     MOVE 4221-STATUS-CODE TO STATUS-WS                                   
271100     PERFORM IMS-STATUSKONTROLL                                           
271200     .                                                                    
271300     EJECT                                                                
271400 IMS-INSERT-4231-MSG SECTION.                                             
271500                                                                          
271600     MOVE SPACE TO GODK-STATUSKODER                                       
271700     CALL CBLTDLI USING ISRT 4231-PCB MSG-IO-AREA                         
271800     MOVE 4231-STATUS-CODE TO STATUS-WS                                   
271900     PERFORM IMS-STATUSKONTROLL                                           
272000     .                                                                    
272100     EJECT                                                                
272200 IMS-INSERT-4275-MSG SECTION.                                             
272300                                                                          
272400     MOVE SPACE TO GODK-STATUSKODER                                       
272500     CALL CBLTDLI USING ISRT 4275-PCB MSG-IO-AREA                         
272600     MOVE 4275-STATUS-CODE TO STATUS-WS                                   
272700     PERFORM IMS-STATUSKONTROLL                                           
272800     .                                                                    
272900     EJECT                                                                
273000                                                                          
273100 IMS-INSERT-ALTMSG SECTION.                                               
273200                                                                          
273300     MOVE '  ' TO GODK-STATUSKODER                                        
273400     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
273500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
273600     PERFORM IMS-STATUSKONTROLL                                           
273700     .                                                                    
273800 IMS-GU-WDA601-CSEQ SECTION.                                              
273900     MOVE 'GU-WDA601-CSEQ  ' TO CURRENT-IMS-SECTION                       
274000                                                                          
274100     MOVE SPACE               TO ALL-SSA                                  
274200     STRING 'WDA601  (WDA6CSEQ>=' W-WDA6CSEQ-MIN-X                        
274300                    '&WDA6CSEQ<=' W-WDA6CSEQ-MAX-X ')'                    
274400          DELIMITED BY SIZE INTO SSA1                                     
274500     MOVE '  GE' TO GODK-STATUSKODER                                      
274600     CALL CBLTDLI USING GU WDA6C-PCB DLI-IO-WDA601 SSA1                   
274700     MOVE WDA6C-STATUS-CODE TO STATUS-WS                                  
274800     PERFORM IMS-STATUSKONTROLL                                           
274900     .                                                                    
275000     EJECT                                                                
275100 IMS-GN-WDA601-CSEQ SECTION.                                              
275200     MOVE 'GN-WDA601-CSEQ  ' TO CURRENT-IMS-SECTION                       
275300                                                                          
275400     MOVE SPACE               TO ALL-SSA                                  
275500     STRING 'WDA601  (WDA6CSEQ>=' W-WDA6CSEQ-MIN-X                        
275600                    '&WDA6CSEQ<=' W-WDA6CSEQ-MAX-X ')'                    
275700          DELIMITED BY SIZE INTO SSA1                                     
275800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
275900     CALL CBLTDLI USING GN WDA6C-PCB DLI-IO-WDA601 SSA1                   
276000     MOVE WDA6C-STATUS-CODE TO STATUS-WS                                  
276100     PERFORM IMS-STATUSKONTROLL                                           
276200     .                                                                    
276300     EJECT                                                                
276400 IMS-GNP-WDA611-CSEQ SECTION.                                             
276500     MOVE 'GNP-WDA611-CSEQ ' TO CURRENT-IMS-SECTION                       
276600                                                                          
276700     MOVE SPACE               TO ALL-SSA                                  
276800     MOVE 'WDA611  ' TO SSA1                                              
276900     MOVE '  GE' TO GODK-STATUSKODER                                      
277000     CALL CBLTDLI USING GNP WDA6C-PCB DLI-IO-WDA611 SSA1                  
277100     MOVE WDA6C-STATUS-CODE TO STATUS-WS                                  
277200     PERFORM IMS-STATUSKONTROLL                                           
277300     .                                                                    
277400     EJECT                                                                
277500 IMS-GNP-WDA612-CSEQ SECTION.                                             
277600     MOVE 'GNP-WDA612-CSEQ ' TO CURRENT-IMS-SECTION                       
277700                                                                          
277800     MOVE SPACE               TO ALL-SSA                                  
277900     MOVE 'WDA612  ' TO SSA1                                              
278000     MOVE '  GE' TO GODK-STATUSKODER                                      
278100     CALL CBLTDLI USING GNP WDA6C-PCB DLI-IO-WDA612 SSA1                  
278200     MOVE WDA6C-STATUS-CODE TO STATUS-WS                                  
278300     PERFORM IMS-STATUSKONTROLL                                           
278400     .                                                                    
278500     EJECT                                                                
278600 IMS-GNP-WDA613-CSEQ SECTION.                                             
278700     MOVE 'GNP-WDA613-CSEQ ' TO CURRENT-IMS-SECTION                       
278800                                                                          
278900     MOVE SPACE               TO ALL-SSA                                  
279000     MOVE 'WDA613  ' TO SSA1                                              
279100     MOVE '  GE' TO GODK-STATUSKODER                                      
279200     CALL CBLTDLI USING GNP WDA6C-PCB DLI-IO-WDA613 SSA1                  
279300     MOVE WDA6C-STATUS-CODE TO STATUS-WS                                  
279400     PERFORM IMS-STATUSKONTROLL                                           
279500     .                                                                    
279600     EJECT                                                                
279700 IMS-GU-WDA601-DSEQ SECTION.                                              
279800     MOVE 'GU-WDA601-DSEQ  ' TO CURRENT-IMS-SECTION                       
279900                                                                          
280000     MOVE SPACE               TO ALL-SSA                                  
280100     STRING 'WDA601  (WDA6DSEQ>=' W-WDA6DSEQ-MIN-X                        
280200                    '&WDA6DSEQ<=' W-WDA6DSEQ-MAX-X ')'                    
280300          DELIMITED BY SIZE INTO SSA1                                     
280400     MOVE '  GE' TO GODK-STATUSKODER                                      
280500     CALL CBLTDLI USING GU WDA6D-PCB DLI-IO-WDA601 SSA1                   
280600     MOVE WDA6D-STATUS-CODE TO STATUS-WS                                  
280700     PERFORM IMS-STATUSKONTROLL                                           
280800     .                                                                    
280900     EJECT                                                                
281000 IMS-GN-WDA601-DSEQ SECTION.                                              
281100     MOVE 'GN-WDA601-DSEQ  ' TO CURRENT-IMS-SECTION                       
281200                                                                          
281300     MOVE SPACE               TO ALL-SSA                                  
281400     STRING 'WDA601  (WDA6DSEQ>=' W-WDA6DSEQ-MIN-X                        
281500                    '&WDA6DSEQ<=' W-WDA6DSEQ-MAX-X ')'                    
281600          DELIMITED BY SIZE INTO SSA1                                     
281700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
281800     CALL CBLTDLI USING GN WDA6D-PCB DLI-IO-WDA601 SSA1                   
281900     MOVE WDA6D-STATUS-CODE TO STATUS-WS                                  
282000     PERFORM IMS-STATUSKONTROLL                                           
282100     .                                                                    
282200     EJECT                                                                
282300 IMS-GNP-WDA611-DSEQ SECTION.                                             
282400     MOVE 'GNP-WDA611-DSEQ ' TO CURRENT-IMS-SECTION                       
282500                                                                          
282600     MOVE SPACE               TO ALL-SSA                                  
282700     MOVE 'WDA611  ' TO SSA1                                              
282800     MOVE '  GE' TO GODK-STATUSKODER                                      
282900     CALL CBLTDLI USING GNP WDA6D-PCB DLI-IO-WDA611 SSA1                  
283000     MOVE WDA6D-STATUS-CODE TO STATUS-WS                                  
283100     PERFORM IMS-STATUSKONTROLL                                           
283200     .                                                                    
283300     EJECT                                                                
283400 IMS-GNP-WDA612-DSEQ SECTION.                                             
283500     MOVE 'GNP-WDA612-DSEQ ' TO CURRENT-IMS-SECTION                       
283600                                                                          
283700     MOVE SPACE               TO ALL-SSA                                  
283800     MOVE 'WDA612  ' TO SSA1                                              
283900     MOVE '  GE' TO GODK-STATUSKODER                                      
284000     CALL CBLTDLI USING GNP WDA6D-PCB DLI-IO-WDA612 SSA1                  
284100     MOVE WDA6D-STATUS-CODE TO STATUS-WS                                  
284200     PERFORM IMS-STATUSKONTROLL                                           
284300     .                                                                    
284400     EJECT                                                                
284500 IMS-GNP-WDA613-DSEQ SECTION.                                             
284600     MOVE 'GNP-WDA613-DSEQ ' TO CURRENT-IMS-SECTION                       
284700                                                                          
284800     MOVE SPACE               TO ALL-SSA                                  
284900     MOVE 'WDA613  ' TO SSA1                                              
285000     MOVE '  GE' TO GODK-STATUSKODER                                      
285100     CALL CBLTDLI USING GNP WDA6D-PCB DLI-IO-WDA613 SSA1                  
285200     MOVE WDA6D-STATUS-CODE TO STATUS-WS                                  
285300     PERFORM IMS-STATUSKONTROLL                                           
285400     .                                                                    
285500     EJECT                                                                
285600 IMS-GU-WDA601-ESEQ SECTION.                                              
285700     MOVE 'GU-WDA601-ESEQ  ' TO CURRENT-IMS-SECTION                       
285800                                                                          
285900     MOVE SPACE               TO ALL-SSA                                  
286000     STRING 'WDA601  (WDA6ESEQ>=' W-WDA6ESEQ-MIN-X                        
286100                    '&WDA6ESEQ<=' W-WDA6ESEQ-MAX-X ')'                    
286200          DELIMITED BY SIZE INTO SSA1                                     
286300     MOVE '  GE' TO GODK-STATUSKODER                                      
286400     CALL CBLTDLI USING GU WDA6E-PCB DLI-IO-WDA601 SSA1                   
286500     MOVE WDA6E-STATUS-CODE TO STATUS-WS                                  
286600     PERFORM IMS-STATUSKONTROLL                                           
286700     .                                                                    
286800     EJECT                                                                
286900 IMS-GN-WDA601-ESEQ SECTION.                                              
287000     MOVE 'GN-WDA601-ESEQ  ' TO CURRENT-IMS-SECTION                       
287100                                                                          
287200     MOVE SPACE               TO ALL-SSA                                  
287300     STRING 'WDA601  (WDA6ESEQ>=' W-WDA6ESEQ-MIN-X                        
287400                    '&WDA6ESEQ<=' W-WDA6ESEQ-MAX-X ')'                    
287500          DELIMITED BY SIZE INTO SSA1                                     
287600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
287700     CALL CBLTDLI USING GN WDA6E-PCB DLI-IO-WDA601 SSA1                   
287800     MOVE WDA6E-STATUS-CODE TO STATUS-WS                                  
287900     PERFORM IMS-STATUSKONTROLL                                           
288000     .                                                                    
288100     EJECT                                                                
288200 IMS-GNP-WDA611-ESEQ SECTION.                                             
288300     MOVE 'GNP-WDA611-ESEQ ' TO CURRENT-IMS-SECTION                       
288400                                                                          
288500     MOVE SPACE               TO ALL-SSA                                  
288600     MOVE 'WDA611  ' TO SSA1                                              
288700     MOVE '  GE' TO GODK-STATUSKODER                                      
288800     CALL CBLTDLI USING GNP WDA6E-PCB DLI-IO-WDA611 SSA1                  
288900     MOVE WDA6E-STATUS-CODE TO STATUS-WS                                  
289000     PERFORM IMS-STATUSKONTROLL                                           
289100     .                                                                    
289200     EJECT                                                                
289300 IMS-GNP-WDA612-ESEQ SECTION.                                             
289400     MOVE 'GNP-WDA612-ESEQ ' TO CURRENT-IMS-SECTION                       
289500                                                                          
289600     MOVE SPACE               TO ALL-SSA                                  
289700     MOVE 'WDA612  ' TO SSA1                                              
289800     MOVE '  GE' TO GODK-STATUSKODER                                      
289900     CALL CBLTDLI USING GNP WDA6E-PCB DLI-IO-WDA612 SSA1                  
290000     MOVE WDA6E-STATUS-CODE TO STATUS-WS                                  
290100     PERFORM IMS-STATUSKONTROLL                                           
290200     .                                                                    
290300     EJECT                                                                
290400 IMS-GNP-WDA613-ESEQ SECTION.                                             
290500     MOVE 'GNP-WDA613-ESEQ ' TO CURRENT-IMS-SECTION                       
290600                                                                          
290700     MOVE SPACE               TO ALL-SSA                                  
290800     MOVE 'WDA613  ' TO SSA1                                              
290900     MOVE '  GE' TO GODK-STATUSKODER                                      
291000     CALL CBLTDLI USING GNP WDA6E-PCB DLI-IO-WDA613 SSA1                  
291100     MOVE WDA6E-STATUS-CODE TO STATUS-WS                                  
291200     PERFORM IMS-STATUSKONTROLL                                           
291300     .                                                                    
291400     EJECT                                                                
291500 IMS-GU-WDA601-FSEQ SECTION.                                              
291600     MOVE 'GU-WDA601-FSEQ  ' TO CURRENT-IMS-SECTION                       
291700                                                                          
291800     MOVE SPACE               TO ALL-SSA                                  
291900     STRING 'WDA601  (WDA6FSEQ>=' W-WDA6FSEQ-MIN-X                        
292000                    '&WDA6FSEQ<=' W-WDA6FSEQ-MAX-X ')'                    
292100          DELIMITED BY SIZE INTO SSA1                                     
292200     MOVE '  GE' TO GODK-STATUSKODER                                      
292300     CALL CBLTDLI USING GU WDA6F-PCB DLI-IO-WDA601 SSA1                   
292400     MOVE WDA6F-STATUS-CODE TO STATUS-WS                                  
292500     PERFORM IMS-STATUSKONTROLL                                           
292600     .                                                                    
292700     EJECT                                                                
292800 IMS-GN-WDA601-FSEQ SECTION.                                              
292900     MOVE 'GN-WDA601-FSEQ  ' TO CURRENT-IMS-SECTION                       
293000                                                                          
293100     MOVE SPACE               TO ALL-SSA                                  
293200     STRING 'WDA601  (WDA6FSEQ>=' W-WDA6FSEQ-MIN-X                        
293300                    '&WDA6FSEQ<=' W-WDA6FSEQ-MAX-X ')'                    
293400          DELIMITED BY SIZE INTO SSA1                                     
293500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
293600     CALL CBLTDLI USING GN WDA6F-PCB DLI-IO-WDA601 SSA1                   
293700     MOVE WDA6F-STATUS-CODE TO STATUS-WS                                  
293800     PERFORM IMS-STATUSKONTROLL                                           
293900     .                                                                    
294000     EJECT                                                                
294100 IMS-GNP-WDA611-FSEQ SECTION.                                             
294200     MOVE 'GNP-WDA611-FSEQ ' TO CURRENT-IMS-SECTION                       
294300                                                                          
294400     MOVE SPACE               TO ALL-SSA                                  
294500     MOVE 'WDA611  ' TO SSA1                                              
294600     MOVE '  GE' TO GODK-STATUSKODER                                      
294700     CALL CBLTDLI USING GNP WDA6F-PCB DLI-IO-WDA611 SSA1                  
294800     MOVE WDA6F-STATUS-CODE TO STATUS-WS                                  
294900     PERFORM IMS-STATUSKONTROLL                                           
295000     .                                                                    
295100     EJECT                                                                
295200 IMS-GNP-WDA612-FSEQ SECTION.                                             
295300     MOVE 'GNP-WDA612-FSEQ ' TO CURRENT-IMS-SECTION                       
295400                                                                          
295500     MOVE SPACE               TO ALL-SSA                                  
295600     MOVE 'WDA612  ' TO SSA1                                              
295700     MOVE '  GE' TO GODK-STATUSKODER                                      
295800     CALL CBLTDLI USING GNP WDA6F-PCB DLI-IO-WDA612 SSA1                  
295900     MOVE WDA6F-STATUS-CODE TO STATUS-WS                                  
296000     PERFORM IMS-STATUSKONTROLL                                           
296100     .                                                                    
296200     EJECT                                                                
296300 IMS-GNP-WDA613-FSEQ SECTION.                                             
296400     MOVE 'GNP-WDA613-FSEQ ' TO CURRENT-IMS-SECTION                       
296500                                                                          
296600     MOVE SPACE               TO ALL-SSA                                  
296700     MOVE 'WDA613  ' TO SSA1                                              
296800     MOVE '  GE' TO GODK-STATUSKODER                                      
296900     CALL CBLTDLI USING GNP WDA6F-PCB DLI-IO-WDA613 SSA1                  
297000     MOVE WDA6F-STATUS-CODE TO STATUS-WS                                  
297100     PERFORM IMS-STATUSKONTROLL                                           
297200     .                                                                    
297300     EJECT                                                                
297400 IMS-GU-WDA601-GSEQ SECTION.                                              
297500     MOVE 'GU-WDA601-GSEQ  ' TO CURRENT-IMS-SECTION                       
297600                                                                          
297700     MOVE SPACE               TO ALL-SSA                                  
297800     STRING 'WDA601  (WDA6GSEQ>=' W-WDA6GSEQ-MIN-X                        
297900                    '&WDA6GSEQ<=' W-WDA6GSEQ-MAX-X ')'                    
298000          DELIMITED BY SIZE INTO SSA1                                     
298100     MOVE '  GE' TO GODK-STATUSKODER                                      
298200     CALL CBLTDLI USING GU WDA6G-PCB DLI-IO-WDA601 SSA1                   
298300     MOVE WDA6G-STATUS-CODE TO STATUS-WS                                  
298400     PERFORM IMS-STATUSKONTROLL                                           
298500     .                                                                    
298600     EJECT                                                                
298700 IMS-GN-WDA601-GSEQ SECTION.                                              
298800     MOVE 'GN-WDA601-GSEQ  ' TO CURRENT-IMS-SECTION                       
298900                                                                          
299000     MOVE SPACE               TO ALL-SSA                                  
299100     STRING 'WDA601  (WDA6GSEQ>=' W-WDA6GSEQ-MIN-X                        
299200                    '&WDA6GSEQ<=' W-WDA6GSEQ-MAX-X ')'                    
299300          DELIMITED BY SIZE INTO SSA1                                     
299400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
299500     CALL CBLTDLI USING GN WDA6G-PCB DLI-IO-WDA601 SSA1                   
299600     MOVE WDA6G-STATUS-CODE TO STATUS-WS                                  
299700     PERFORM IMS-STATUSKONTROLL                                           
299800     .                                                                    
299900     EJECT                                                                
300000 IMS-GNP-WDA611-GSEQ SECTION.                                             
300100     MOVE 'GNP-WDA611-GSEQ ' TO CURRENT-IMS-SECTION                       
300200                                                                          
300300     MOVE SPACE               TO ALL-SSA                                  
300400     MOVE 'WDA611  ' TO SSA1                                              
300500     MOVE '  GE' TO GODK-STATUSKODER                                      
300600     CALL CBLTDLI USING GNP WDA6G-PCB DLI-IO-WDA611 SSA1                  
300700     MOVE WDA6G-STATUS-CODE TO STATUS-WS                                  
300800     PERFORM IMS-STATUSKONTROLL                                           
300900     .                                                                    
301000     EJECT                                                                
301100 IMS-GNP-WDA612-GSEQ SECTION.                                             
301200     MOVE 'GNP-WDA612-GSEQ ' TO CURRENT-IMS-SECTION                       
301300                                                                          
301400     MOVE SPACE               TO ALL-SSA                                  
301500     MOVE 'WDA612  ' TO SSA1                                              
301600     MOVE '  GE' TO GODK-STATUSKODER                                      
301700     CALL CBLTDLI USING GNP WDA6G-PCB DLI-IO-WDA612 SSA1                  
301800     MOVE WDA6G-STATUS-CODE TO STATUS-WS                                  
301900     PERFORM IMS-STATUSKONTROLL                                           
302000     .                                                                    
302100     EJECT                                                                
302200 IMS-GNP-WDA613-GSEQ SECTION.                                             
302300     MOVE 'GNP-WDA613-GSEQ ' TO CURRENT-IMS-SECTION                       
302400                                                                          
302500     MOVE SPACE               TO ALL-SSA                                  
302600     MOVE 'WDA613  ' TO SSA1                                              
302700     MOVE '  GE' TO GODK-STATUSKODER                                      
302800     CALL CBLTDLI USING GNP WDA6G-PCB DLI-IO-WDA613 SSA1                  
302900     MOVE WDA6G-STATUS-CODE TO STATUS-WS                                  
303000     PERFORM IMS-STATUSKONTROLL                                           
303100     .                                                                    
303200     EJECT                                                                
303300 IMS-GU-WDA601-HSEQ SECTION.                                              
303400     MOVE 'GU-WDA601-HSEQ  ' TO CURRENT-IMS-SECTION                       
303500                                                                          
303600     MOVE SPACE               TO ALL-SSA                                  
303700     STRING 'WDA601  (WDA6HSEQ>=' W-WDA6HSEQ-MIN-X                        
303800                    '&WDA6HSEQ<=' W-WDA6HSEQ-MAX-X ')'                    
303900          DELIMITED BY SIZE INTO SSA1                                     
304000     MOVE '  GE' TO GODK-STATUSKODER                                      
304100     CALL CBLTDLI USING GU WDA6H-PCB DLI-IO-WDA601 SSA1                   
304200     MOVE WDA6H-STATUS-CODE TO STATUS-WS                                  
304300     PERFORM IMS-STATUSKONTROLL                                           
304400     .                                                                    
304500     EJECT                                                                
304600 IMS-GN-WDA601-HSEQ SECTION.                                              
304700     MOVE 'GN-WDA601-HSEQ  ' TO CURRENT-IMS-SECTION                       
304800                                                                          
304900     MOVE SPACE               TO ALL-SSA                                  
305000     STRING 'WDA601  (WDA6HSEQ>=' W-WDA6HSEQ-MIN-X                        
305100                    '&WDA6HSEQ<=' W-WDA6HSEQ-MAX-X ')'                    
305200          DELIMITED BY SIZE INTO SSA1                                     
305300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
305400     CALL CBLTDLI USING GN WDA6H-PCB DLI-IO-WDA601 SSA1                   
305500     MOVE WDA6H-STATUS-CODE TO STATUS-WS                                  
305600     PERFORM IMS-STATUSKONTROLL                                           
305700     .                                                                    
305800     EJECT                                                                
305900 IMS-GNP-WDA611-HSEQ SECTION.                                             
306000     MOVE 'GNP-WDA611-HSEQ ' TO CURRENT-IMS-SECTION                       
306100                                                                          
306200     MOVE SPACE               TO ALL-SSA                                  
306300     MOVE 'WDA611  ' TO SSA1                                              
306400     MOVE '  GE' TO GODK-STATUSKODER                                      
306500     CALL CBLTDLI USING GNP WDA6H-PCB DLI-IO-WDA611 SSA1                  
306600     MOVE WDA6H-STATUS-CODE TO STATUS-WS                                  
306700     PERFORM IMS-STATUSKONTROLL                                           
306800     .                                                                    
306900     EJECT                                                                
307000 IMS-GNP-WDA612-HSEQ SECTION.                                             
307100     MOVE 'GNP-WDA612-HSEQ ' TO CURRENT-IMS-SECTION                       
307200                                                                          
307300     MOVE SPACE               TO ALL-SSA                                  
307400     MOVE 'WDA612  ' TO SSA1                                              
307500     MOVE '  GE' TO GODK-STATUSKODER                                      
307600     CALL CBLTDLI USING GNP WDA6H-PCB DLI-IO-WDA612 SSA1                  
307700     MOVE WDA6H-STATUS-CODE TO STATUS-WS                                  
307800     PERFORM IMS-STATUSKONTROLL                                           
307900     .                                                                    
308000     EJECT                                                                
308100 IMS-GNP-WDA613-HSEQ SECTION.                                             
308200     MOVE 'GNP-WDA613-HSEQ ' TO CURRENT-IMS-SECTION                       
308300                                                                          
308400     MOVE SPACE               TO ALL-SSA                                  
308500     MOVE 'WDA613  ' TO SSA1                                              
308600     MOVE '  GE' TO GODK-STATUSKODER                                      
308700     CALL CBLTDLI USING GNP WDA6H-PCB DLI-IO-WDA613 SSA1                  
308800     MOVE WDA6H-STATUS-CODE TO STATUS-WS                                  
308900     PERFORM IMS-STATUSKONTROLL                                           
309000     .                                                                    
309100     EJECT                                                                
309200 IMS-GU-WDA601-ISEQ SECTION.                                              
309300     MOVE 'GU-WDA601-ISEQ  ' TO CURRENT-IMS-SECTION                       
309400                                                                          
309500     MOVE SPACE               TO ALL-SSA                                  
309600     STRING 'WDA601  (WDA6ISEQ>=' W-WDA6ISEQ-MIN-X                        
309700                    '&WDA6ISEQ<=' W-WDA6ISEQ-MAX-X ')'                    
309800          DELIMITED BY SIZE INTO SSA1                                     
309900     MOVE '  GE' TO GODK-STATUSKODER                                      
310000     CALL CBLTDLI USING GU WDA6I-PCB DLI-IO-WDA601 SSA1                   
310100     MOVE WDA6I-STATUS-CODE TO STATUS-WS                                  
310200     PERFORM IMS-STATUSKONTROLL                                           
310300     .                                                                    
310400     EJECT                                                                
310500 IMS-GN-WDA601-ISEQ SECTION.                                              
310600     MOVE 'GN-WDA601-ISEQ  ' TO CURRENT-IMS-SECTION                       
310700                                                                          
310800     MOVE SPACE               TO ALL-SSA                                  
310900     STRING 'WDA601  (WDA6ISEQ>=' W-WDA6ISEQ-MIN-X                        
311000                    '&WDA6ISEQ<=' W-WDA6ISEQ-MAX-X ')'                    
311100          DELIMITED BY SIZE INTO SSA1                                     
311200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
311300     CALL CBLTDLI USING GN WDA6I-PCB DLI-IO-WDA601 SSA1                   
311400     MOVE WDA6I-STATUS-CODE TO STATUS-WS                                  
311500     PERFORM IMS-STATUSKONTROLL                                           
311600     .                                                                    
311700     EJECT                                                                
311800 IMS-GNP-WDA611-ISEQ SECTION.                                             
311900     MOVE 'GNP-WDA611-ISEQ ' TO CURRENT-IMS-SECTION                       
312000                                                                          
312100     MOVE SPACE               TO ALL-SSA                                  
312200     MOVE 'WDA611  ' TO SSA1                                              
312300     MOVE '  GE' TO GODK-STATUSKODER                                      
312400     CALL CBLTDLI USING GNP WDA6I-PCB DLI-IO-WDA611 SSA1                  
312500     MOVE WDA6I-STATUS-CODE TO STATUS-WS                                  
312600     PERFORM IMS-STATUSKONTROLL                                           
312700     .                                                                    
312800     EJECT                                                                
312900 IMS-GNP-WDA612-ISEQ SECTION.                                             
313000     MOVE 'GNP-WDA612-ISEQ ' TO CURRENT-IMS-SECTION                       
313100                                                                          
313200     MOVE SPACE               TO ALL-SSA                                  
313300     MOVE 'WDA612  ' TO SSA1                                              
313400     MOVE '  GE' TO GODK-STATUSKODER                                      
313500     CALL CBLTDLI USING GNP WDA6I-PCB DLI-IO-WDA612 SSA1                  
313600     MOVE WDA6I-STATUS-CODE TO STATUS-WS                                  
313700     PERFORM IMS-STATUSKONTROLL                                           
313800     .                                                                    
313900     EJECT                                                                
314000 IMS-GNP-WDA613-ISEQ SECTION.                                             
314100     MOVE 'GNP-WDA613-ISEQ ' TO CURRENT-IMS-SECTION                       
314200                                                                          
314300     MOVE SPACE               TO ALL-SSA                                  
314400     MOVE 'WDA613  ' TO SSA1                                              
314500     MOVE '  GE' TO GODK-STATUSKODER                                      
314600     CALL CBLTDLI USING GNP WDA6I-PCB DLI-IO-WDA613 SSA1                  
314700     MOVE WDA6I-STATUS-CODE TO STATUS-WS                                  
314800     PERFORM IMS-STATUSKONTROLL                                           
314900     .                                                                    
315000     EJECT                                                                
315100 IMS-GU-WDA601-JSEQ SECTION.                                              
315200     MOVE 'GU-WDA601-JSEQ  ' TO CURRENT-IMS-SECTION                       
315300                                                                          
315400     MOVE SPACE               TO ALL-SSA                                  
315500     STRING 'WDA601  (WDA6JSEQ>=' W-WDA6JSEQ-MIN-X                        
315600                    '&WDA6JSEQ<=' W-WDA6JSEQ-MAX-X ')'                    
315700          DELIMITED BY SIZE INTO SSA1                                     
315800     MOVE '  GE' TO GODK-STATUSKODER                                      
315900     CALL CBLTDLI USING GU WDA6J-PCB DLI-IO-WDA601 SSA1                   
316000     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
316100     PERFORM IMS-STATUSKONTROLL                                           
316200     .                                                                    
316300     EJECT                                                                
316400 IMS-GN-WDA601-JSEQ SECTION.                                              
316500     MOVE 'GN-WDA601-JSEQ  ' TO CURRENT-IMS-SECTION                       
316600                                                                          
316700     MOVE SPACE               TO ALL-SSA                                  
316800     STRING 'WDA601  (WDA6JSEQ>=' W-WDA6JSEQ-MIN-X                        
316900                    '&WDA6JSEQ<=' W-WDA6JSEQ-MAX-X ')'                    
317000          DELIMITED BY SIZE INTO SSA1                                     
317100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
317200     CALL CBLTDLI USING GN WDA6J-PCB DLI-IO-WDA601 SSA1                   
317300     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
317400     PERFORM IMS-STATUSKONTROLL                                           
317500     .                                                                    
317600     EJECT                                                                
317700 IMS-GNP-WDA611-JSEQ SECTION.                                             
317800     MOVE 'GNP-WDA611-JSEQ ' TO CURRENT-IMS-SECTION                       
317900                                                                          
318000     MOVE SPACE               TO ALL-SSA                                  
318100     MOVE 'WDA611  ' TO SSA1                                              
318200     MOVE '  GE' TO GODK-STATUSKODER                                      
318300     CALL CBLTDLI USING GNP WDA6J-PCB DLI-IO-WDA611 SSA1                  
318400     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
318500     PERFORM IMS-STATUSKONTROLL                                           
318600     .                                                                    
318700     EJECT                                                                
318800 IMS-GNP-WDA612-JSEQ SECTION.                                             
318900     MOVE 'GNP-WDA612-JSEQ ' TO CURRENT-IMS-SECTION                       
319000                                                                          
319100     MOVE SPACE               TO ALL-SSA                                  
319200     MOVE 'WDA612  ' TO SSA1                                              
319300     MOVE '  GE' TO GODK-STATUSKODER                                      
319400     CALL CBLTDLI USING GNP WDA6J-PCB DLI-IO-WDA612 SSA1                  
319500     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
319600     PERFORM IMS-STATUSKONTROLL                                           
319700     .                                                                    
319800     EJECT                                                                
319900 IMS-GNP-WDA613-JSEQ SECTION.                                             
320000     MOVE 'GNP-WDA613-JSEQ ' TO CURRENT-IMS-SECTION                       
320100                                                                          
320200     MOVE SPACE               TO ALL-SSA                                  
320300     MOVE 'WDA613  ' TO SSA1                                              
320400     MOVE '  GE' TO GODK-STATUSKODER                                      
320500     CALL CBLTDLI USING GNP WDA6J-PCB DLI-IO-WDA613 SSA1                  
320600     MOVE WDA6J-STATUS-CODE TO STATUS-WS                                  
320700     PERFORM IMS-STATUSKONTROLL                                           
320800     .                                                                    
320900     EJECT                                                                
321000 IMS-GHU-WDA601 SECTION.                                                  
321100     MOVE 'GHU-WDA601      ' TO CURRENT-IMS-SECTION                       
321200                                                                          
321300     MOVE SPACE               TO ALL-SSA                                  
321400     STRING 'WDA601  (WDA601KY =' W-WDA601KY-X ')'                        
321500          DELIMITED BY SIZE INTO SSA1                                     
321600     MOVE '  GE' TO GODK-STATUSKODER                                      
321700     CALL CBLTDLI USING GHU WDA6-PCB DLI-IO-WDA601 SSA1                   
321800     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
321900     PERFORM IMS-STATUSKONTROLL                                           
322000     .                                                                    
322100     EJECT                                                                
322200 IMS-REPL-WDA601 SECTION.                                                 
322300     MOVE 'REPL-WDA601     ' TO CURRENT-IMS-SECTION                       
322400                                                                          
322500     MOVE SPACE               TO ALL-SSA                                  
322600     MOVE '  ' TO GODK-STATUSKODER                                        
322700     CALL CBLTDLI USING REPL WDA6-PCB DLI-IO-WDA601                       
322800     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
322900     PERFORM IMS-STATUSKONTROLL                                           
323000     .                                                                    
323100     EJECT                                                                
323200 IMS-GU-WDK611 SECTION.                                                   
323300     MOVE 'GU-WDK611       ' TO CURRENT-IMS-SECTION                       
323400                                                                          
323500     MOVE SPACE               TO ALL-SSA                                  
323600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
323700          DELIMITED BY SIZE INTO SSA1                                     
323800     MOVE 'WDK611' TO SSA2                                                
323900     MOVE '  GE' TO GODK-STATUSKODER                                      
324000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
324100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
324200     PERFORM IMS-STATUSKONTROLL                                           
324300     .                                                                    
324400     EJECT                                                                
324500 IMS-GHU-WDK611 SECTION.                                                  
324600     MOVE 'GHU-WDK611      ' TO CURRENT-IMS-SECTION                       
324700                                                                          
324800     MOVE SPACE               TO ALL-SSA                                  
324900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
325000          DELIMITED BY SIZE INTO SSA1                                     
325100     MOVE 'WDK611'               TO SSA2                                  
325200     MOVE '  GE'                 TO GODK-STATUSKODER                      
325300     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
325400     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
325500     PERFORM IMS-STATUSKONTROLL                                           
325600     .                                                                    
325700     SKIP2                                                                
325800 IMS-REPL-WDK6 SECTION.                                                   
325900     MOVE 'REPL-WDK6       ' TO CURRENT-IMS-SECTION                       
326000                                                                          
326100     MOVE SPACE               TO ALL-SSA                                  
326200     MOVE '  '                 TO GODK-STATUSKODER                        
326300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
326400     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
326500     PERFORM IMS-STATUSKONTROLL                                           
326600     .                                                                    
326700     EJECT                                                                
326800 IMS-GHU-WDK711 SECTION.                                                  
326900     MOVE 'GHU-WDK711      ' TO CURRENT-IMS-SECTION                       
327000                                                                          
327100     MOVE SPACE               TO ALL-SSA                                  
327200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
327300          DELIMITED BY SIZE INTO SSA1                                     
327400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
327500          DELIMITED BY SIZE INTO SSA2                                     
327600     MOVE '  GE' TO GODK-STATUSKODER                                      
327700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
327800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
327900     PERFORM IMS-STATUSKONTROLL                                           
328000     .                                                                    
328100     EJECT                                                                
328200 IMS-REPL-WDK711 SECTION.                                                 
328300     MOVE 'REPL-WDK711     ' TO CURRENT-IMS-SECTION                       
328400                                                                          
328500     MOVE SPACE               TO ALL-SSA                                  
328600     MOVE '    '               TO GODK-STATUSKODER                        
328700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
328800     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
328900     PERFORM IMS-STATUSKONTROLL                                           
329000     .                                                                    
329100     EJECT                                                                
329200 IMS-GHU-WDK901 SECTION.                                                  
329300     MOVE 'GHU-WDK901      ' TO CURRENT-IMS-SECTION                       
329400                                                                          
329500     MOVE SPACE               TO ALL-SSA                                  
329600     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
329700          DELIMITED BY SIZE INTO SSA1                                     
329800     MOVE '  GE'   TO GODK-STATUSKODER                                    
329900     CALL CBLTDLI USING GHU WDK9-PCB DLI-IO-WDK901 SSA1                   
330000     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
330100     PERFORM IMS-STATUSKONTROLL                                           
330200     .                                                                    
330300     EJECT                                                                
330400 IMS-REPL-WDK901 SECTION.                                                 
330500     MOVE 'REPL-WDK901     ' TO CURRENT-IMS-SECTION                       
330600                                                                          
330700     MOVE SPACE               TO ALL-SSA                                  
330800     MOVE '  ' TO GODK-STATUSKODER                                        
330900     CALL CBLTDLI USING REPL WDK9-PCB DLI-IO-WDK901                       
331000     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
331100     PERFORM IMS-STATUSKONTROLL                                           
331200     .                                                                    
331300     EJECT                                                                
331400 IMS-GU-WDP501 SECTION.                                                   
331500     MOVE 'GU-WDP501       ' TO CURRENT-IMS-SECTION                       
331600                                                                          
331700     MOVE SPACE               TO ALL-SSA                                  
331800     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
331900          DELIMITED BY SIZE INTO SSA1                                     
332000     MOVE '  GE' TO GODK-STATUSKODER                                      
332100     CALL CBLTDLI USING GU WDP5-PCB DLI-IO-WDP501 SSA1                    
332200     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
332300     PERFORM IMS-STATUSKONTROLL                                           
332400     .                                                                    
332500     SKIP3                                                                
332600 IMS-ISRT-WDR601 SECTION.                                                 
332700     MOVE 'ISRT-WDR601     ' TO CURRENT-IMS-SECTION                       
332800                                                                          
332900     MOVE SPACE               TO ALL-SSA                                  
333000     MOVE 'WDR601' TO SSA1                                                
333100     MOVE '  II' TO GODK-STATUSKODER                                      
333200     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-WDR601 SSA1                  
333300     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
333400     PERFORM IMS-STATUSKONTROLL                                           
333500     .                                                                    
333600     EJECT                                                                
333700 IMS-GU-WDL201 SECTION.                                                   
333800     MOVE 'GU-WDL201       ' TO CURRENT-IMS-SECTION                       
333900                                                                          
334000     MOVE SPACE               TO ALL-SSA                                  
334100     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
334200          DELIMITED BY SIZE INTO SSA1                                     
334300     MOVE '  GE' TO GODK-STATUSKODER                                      
334400     CALL CBLTDLI USING GU   WDL2-PCB DLI-IO-WDL201 SSA1                  
334500     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
334600     PERFORM IMS-STATUSKONTROLL                                           
334700     .                                                                    
334800                                                                          
334900 IMS-GNP-WDL221 SECTION.                                                  
335000     MOVE 'GNP-WDL221      ' TO CURRENT-IMS-SECTION                       
335100                                                                          
335200     MOVE SPACE               TO ALL-SSA                                  
335300     STRING 'WDL211     '                                                 
335400          DELIMITED BY SIZE INTO SSA1                                     
335500     STRING 'WDL221     '                                                 
335600          DELIMITED BY SIZE INTO SSA2                                     
335700     MOVE '  GE' TO GODK-STATUSKODER                                      
335800     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
335900     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
336000     PERFORM IMS-STATUSKONTROLL                                           
336100     .                                                                    
336200 IMS-GU-WDB601    SECTION.                                                
336300     MOVE 'GU-WDB601       ' TO CURRENT-IMS-SECTION                       
336400                                                                          
336500     MOVE SPACE               TO ALL-SSA                                  
336600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
336700          DELIMITED BY SIZE INTO SSA1                                     
336800     MOVE '  GE' TO GODK-STATUSKODER                                      
336900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
337000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
337100     PERFORM IMS-STATUSKONTROLL                                           
337200     IF SEGMENT-MISSING                                                   
337300         MOVE SPACE TO DCS-KDDC                                           
337400     END-IF                                                               
337500     .                                                                    
337600 IMS-GU-WDD902  SECTION.                                                  
337700     MOVE 'GU-WDD902       ' TO CURRENT-IMS-SECTION                       
337800                                                                          
337900     MOVE SPACE                 TO ALL-SSA                                
338000     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
338100            DELIMITED BY SIZE INTO SSA1                                   
338200     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
338300            DELIMITED BY SIZE INTO SSA2                                   
338400     MOVE '  GE'                TO GODK-STATUSKODER                       
338500     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-AREA-D902 SSA1 SSA2            
338600     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
338700     PERFORM IMS-STATUSKONTROLL                                           
338800     .                                                                    
338900                                                                          
339000 IMS-GNP-WDD924  SECTION.                                                 
339100     MOVE 'GNP-WDD924      ' TO CURRENT-IMS-SECTION                       
339200                                                                          
339300     MOVE SPACE            TO ALL-SSA                                     
339400     MOVE 'WDD924 '        TO SSA1                                        
339500     MOVE '  GE'           TO GODK-STATUSKODER                            
339600     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-AREA-D924 SSA1                
339700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
339800     PERFORM IMS-STATUSKONTROLL                                           
339900     .                                                                    
340000                                                                          
340100 IMS-GNP-WDD925  SECTION.                                                 
340200     MOVE 'GNP-WDD925      ' TO CURRENT-IMS-SECTION                       
340300                                                                          
340400     MOVE SPACE            TO ALL-SSA                                     
340500     MOVE 'WDD925 '        TO SSA1                                        
340600     MOVE '  GE'           TO GODK-STATUSKODER                            
340700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-AREA-D925 SSA1                
340800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
340900     PERFORM IMS-STATUSKONTROLL                                           
341000     .                                                                    
341100                                                                          
341200 IMS-STATUSKONTROLL SECTION.                                              
341300                                                                          
341400     SET STATUS-IX TO 1                                                   
341500     SEARCH GODK-STATUS                                                   
341600       AT END                                                             
341700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
341800         DELIMITED BY SIZE INTO FELTEXT                                   
341900         CALL FELLOG                                                      
342000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
342100         CONTINUE                                                         
342200     END-SEARCH                                                           
342300     .                                                                    
342400     EJECT                                                                
