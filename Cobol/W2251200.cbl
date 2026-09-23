000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2251200.                                    
000300 AUTHOR                      IDK, GÖTEBORG.                               
000400 DATE-WRITTEN.               OKT 1978.                                    
000410 DATE-COMPILED.                                                           
000500                                                                          
000700*    FUNKTION.                                                            
000800*        SKRIVER UT LISTORNA                                              
000900*            - SERVICEGRAD PER ANSKAFFARE                                 
001000*            - SERVICEGRAD PER GRUPP                                      
001100*            - SERVICEGRAD PER SEKTION                                    
001300*        PÅ LISTAN SERVICEGRAD PER ANSKAFFARE SKRIVS INFO                 
001400*        OM C1-LAGER, C2-LAGER OCH GEMENSAM DEL BEROENDE PÅ               
001500*        KDCLPOST. INOM VARJE ANSKAFFARE SKER BRYTNING PER                
001600*        LEVERANTÖR. ENDAST ARTIKLAR MED TOTALT SUROBEL STÖRRE            
001700*        ÄN NOLL SKRIVS UT MEN ALLA BEARBETAS VID BERÄKNING               
001800*        AV SERVICEGRADEN PÅ DE OLIKA NIVÅERNA.                           
001900*        VID BRYTNING PÅ ANSKAFFARE SKRIVS TOTAL UT.                      
002000*        PÅ LISTORNA PER GRUPP OCH PER SEKTION SKRIVS                     
002100*        TOTALT ANSKAFFARE OCH TOTALT GRUPP RESP TOTALT SECTION           
002110*                                                                         
002120*        LISTA W22512-004 SERVICEGRAD - TOTALT, BORTAGEN                  
002130*        92-12-15 SK                                                      
002140*                                                                         
002200*                                                                         
002300*        MODUL W200ANSK ANROPAS FÖR ATT ERHÅLLA GRUPPER                   
002400*        OCH SEKTIONER FRÅN IDANSK.                                       
002900*    RETURKODER.                                                          
003000*        U0020      FEL I SORTERINGEN.                                    
003100     EJECT                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300 INPUT-OUTPUT SECTION.                                                    
003400 FILE-CONTROL.                                                            
003500     SKIP2                                                                
003600*--------------------------------------- LISTRECORD FÖR SERVICE-          
003700*                                        GRAD PER ANSKAFFARE              
003800     SELECT  W22513          ASSIGN      W22512D1.                        
003900*                                                                         
004000*--------------------------------------- LISTA SERVICEGRAD                
004100*                                        PER ANSKAFFARE                   
004200     SELECT  SA-LISTA        ASSIGN      W22512D2.                        
004300*                                                                         
004400*--------------------------------------- LISTA SERVICEGRAD                
004500*                                        PER GRUPP                        
004600     SELECT  SG-LISTA        ASSIGN      W22512D3.                        
004700*                                                                         
004800*--------------------------------------- LISTA SERVICEGRAD                
004900*                                        PER SEKTION                      
005000     SELECT  SS-LISTA        ASSIGN      W22512D4.                        
005100*                                                                         
005600*--------------------------------------- SORTERINGSFIL FÖR                
005700*                                        FIL W22513                       
005800     SELECT  SRT-FIL         ASSIGN      W22512DS.                        
005900     EJECT                                                                
006000 DATA DIVISION.                                                           
006100 FILE SECTION.                                                            
006200     SKIP2                                                                
006300 FD  W22513                                                               
006400     RECORDING F                                                          
006500     BLOCK 0                                                              
006600     LABEL RECORD STANDARD.                                               
006700*01  -COPY W225LI01    -L                                                 
006900     SKIP3                                                                
007000 FD  SA-LISTA                                                             
007100     RECORDING F                                                          
007200     LABEL RECORD STANDARD                                                
007300     RECORD 122.                                                          
007400 01  SA-POST.                                                             
007500     05  FILLER                  PIC X.                                   
007600     05  SA-RAD                  PIC X(121).                              
007700     SKIP3                                                                
007800 FD  SG-LISTA                                                             
007900     RECORDING F                                                          
008000     LABEL RECORD STANDARD                                                
008100     RECORD 122.                                                          
008200 01  SG-POST.                                                             
008300     05  FILLER                  PIC X.                                   
008400     05  SG-RAD                  PIC X(121).                              
008500     SKIP3                                                                
008600 FD  SS-LISTA                                                             
008700     RECORDING F                                                          
008800     LABEL RECORD STANDARD                                                
008900     RECORD 122.                                                          
009000 01  SS-POST.                                                             
009100     05  FILLER                  PIC X.                                   
009200     05  SS-RAD                  PIC X(121).                              
009300     SKIP3                                                                
010200 SD  SRT-FIL                                                              
010300     RECORDING F.                                                         
010400*01  POST   -COPY W225LI01   -PRE  SRT-                                   
010600     EJECT                                                                
010700 WORKING-STORAGE SECTION.                                                 
010800     SKIP2                                                                
010801                                                                          
010810*    -- CHECKED BY WY2000                                                 
010900 01  W.                                                                   
011000*                                                                         
011100*--------------------------------------- ALLMÄNA ARBETSFÄLT               
011200*                                                                         
011300     05  W-PROGNAMN          PIC X(6)    VALUE 'W22512'.                  
011400     05  W-LEV-ANTART        PIC S9(9)               COMP-3.              
011500     05  W-LEV-ROVAERDE      PIC S9(9)V9(2)          COMP-3.              
011600     05  W-LEV-KVRORAD       PIC S9(7)V9(2)          COMP-3.              
011700     05  W-SPAR-IDANSK       PIC 9(3).                                    
011800     05  W-SUROBEL-CDC-1-2   PIC S9(7)V9(2)          COMP-3.              
011900     05  W-SUROBEL-CDC-3-4   PIC S9(7)V9(2)          COMP-3.              
012000     05  W-SUROBEL-SDC-1-2   PIC S9(7)V9(2)          COMP-3.              
012100     05  W-SUROBEL-SDC-3-4   PIC S9(7)V9(2)          COMP-3.              
012200     05  W-ROTKR             PIC 9(9).                                    
012300     05  W-SUMMA             PIC S9(7)               COMP-3.              
012400*--------------------------------------- ARBETSFÄLT FÖR DATUM-            
012500*                                        REDIGERINGAR                     
012600     05  W-DATUM-VKA.                                                     
012700         10  W-AA-VV.                                                     
012800             15  FILLER      PIC X.                                       
012900             15  W-A-VV      PIC X(3).                                    
013000         10  FILLER          PIC X.                                       
013100     05  W-DATUM-VKA-N       REDEFINES W-DATUM-VKA                        
013200                             PIC 9(5).                                    
013300*--------------------------------------- NYTT JM-87                       
013400*                                                                         
013500     05  WS-TEST-DATUM       PIC 9(3).                                    
013600     05  FILLER   REDEFINES WS-TEST-DATUM.                                
013700         10  WS-TEST-A       PIC 9.                                       
013800         10  WS-TEST-VV      PIC 9(2).                                    
013900*                                                                         
014000*--------------------------------------- SLUT JM-87                       
014100*                                                                         
014200*--------------------------------------- LAGRAR SERVICEPROCENT            
014300*                                        PER ORDERKLASS                   
014400     05  W-SERVPROC          OCCURS 3.                                    
014500         10  W-SERVPROC-CDC  PIC S9(4)V9             COMP-3.              
014600         10  W-SERVPROC-SDC  PIC S9(4)V9             COMP-3.              
014700         10  W-SERVPROC-T    PIC S9(4)V9             COMP-3.              
014800                                                                          
014900*-------------------------------------- LAGRAR INTERVALLET FÖR            
015000*                                       AKTUELL GRP OCH SEKTION           
015100     05  W-INTERVALL.                                                     
015200         10  W-GRP-INTV.                                                  
015300             15  W-GRP-FROM  PIC X(3).                                    
015400             15  FILLER      PIC X.                                       
015500             15  W-GRP-TOM   PIC X(3).                                    
015600         10  W-SEK-INTV.                                                  
015700             15  W-SEK-FROM  PIC X(3).                                    
015800             15  FILLER      PIC X.                                       
015900             15  W-SEK-TOM   PIC X(3).                                    
016000         10  W-FUNK-INTV.                                                 
016100             15  W-FUNK-FROM PIC X(3).                                    
016200             15  FILLER      PIC X.                                       
016300             15  W-FUNK-TOM  PIC X(3).                                    
016400     SKIP2                                                                
016500 01  RKOD                    PIC S9(4)   VALUE ZERO  COMP SYNC.           
016600                                                                          
016700 01  IX.                                                                  
016800     05  IXOKL               PIC S9(9)               COMP SYNC.           
016900     05  IXNS                PIC S9(9)               COMP SYNC.           
017000     05  IXINT               PIC S9(9)               COMP SYNC.           
017100     05  IXNS2               PIC S9(9)               COMP SYNC.           
017200     SKIP2                                                                
017300 01  KONSTANTER.                                                          
017400     05  JA                  PIC X       VALUE 'J'.                       
017500     05  NEJ                 PIC X       VALUE 'N'.                       
017600     SKIP2                                                                
017700 01  SW.                                                                  
017800     05  SW-SA-TOTAL         PIC X       VALUE 'N'.                       
017900     05  SW-W22513-EOF       PIC X       VALUE 'N'.                       
018000     05  SW-CDC-ORDERKLASS-1-2 PIC X     VALUE 'N'.                       
018100     05  SW-CDC-ORDERKLASS-3-4 PIC X     VALUE 'N'.                       
018200     05  SW-SDC-ORDERKLASS-1-2 PIC X     VALUE 'N'.                       
018300     05  SW-SDC-ORDERKLASS-3-4 PIC X     VALUE 'N'.                       
018400     05  SW-SKRIV-TOT-LEVR   PIC X       VALUE 'N'.                       
018500     SKIP2                                                                
018600 01  DYNAMISKA-SUBPROGRAM.                                                
018700     05  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
018900     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
019000     05  W200ANSK            PIC X(8)    VALUE 'W200ANSK'.                
019100     05  ABEND               PIC X(8)    VALUE 'ABEND   '.                
019200     SKIP2                                                                
019300*--------------------------------------- TRANSAKTIONS-ID                  
019400 01  TID.                                                                 
019500     05  TID-IDSEKT-IDANSK.                                               
019600         10  TID-IDSEKT-IDGRUPP.                                          
019700             15  TID-IDSEKT  PIC X(3).                                    
019800             15  TID-IDGRUPP PIC X(3).                                    
019900         10  TID-IDANSK      PIC X(3).                                    
020000     05  TID-IDLEVNR         PIC X(5).                                    
020100*--------------------------------------- LIST-ID                          
020200 01  LID.                                                                 
020300     05  LID-IDSEKT-IDANSK.                                               
020400         10  LID-IDSEKT-IDGRUPP.                                          
020500             15  LID-IDSEKT  PIC X(3).                                    
020600             15  LID-IDGRUPP PIC X(3).                                    
020700         10  LID-IDANSK      PIC X(3).                                    
020800     05  LID-IDLEVNR         PIC X(5).                                    
020900     EJECT                                                                
021000 01  FILLER                  PIC X(16) VALUE ALL 'A'.                     
021100*------------------------------------- PARAMETRAR TILL DATKORT            
021200 01   DATUMKORT-ID           PIC X(6)  VALUE 'WDATUM'.                    
021300*01   -COPY WDATKORT                                                      
021500     EJECT                                                                
022000*--------------------------------------- PARAMETRAR TILL POSTSUM          
022100*01  -COPY W0005       -PRE POSTSUM-                                      
022300     EJECT                                                                
022400*--------------------------------------- PARAMETRAR TILL W200ANSK         
022500*01  -COPY W009W42  -PRE W-                                               
022700     EJECT                                                                
022800 01  FILLER                      PIC X(16)  VALUE ALL 'B'.                
022900*--------------------------------------- AREA FÖR W22513-POST             
023000*01  AREA -COPY W225LI01    -PRE I13-                                     
023200     EJECT                                                                
023300*--------------------------------------- AREA FÖR W22513-URVALS-          
023400*                                        POST                             
023500*01  AREA -COPY W225LI00    -PRE I13P00-   -RED I13-AREA                  
023700     EJECT                                                                
023800 01  FILLER                  PIC X(16)   VALUE ALL 'C'.                   
023900*--------------------------------------- GEMENSAM RUBRIK                  
024000*                                                                         
024100 01  W-RUB.                                                               
024200     05  FILLER              PIC X(17)   VALUE 'VOLVO PARTS'.             
024300     05  FILLER              PIC X(7)    VALUE 'W22512-'.                 
024400     05  W-LISTNR            PIC 9(3).                                    
024500     05  FILLER              PIC X(2)    VALUE SPACE.                     
024600     05  FILLER              PIC X(12)   VALUE 'SERVICEGRAD'.             
024700     05  W-TEXT.                                                          
024800         10  FILLER          PIC X(15).                                   
024900         10  W-RUB-IDANSK    PIC Z(2)9.                                   
025000         10  FILLER          PIC X(3).                                    
025100     05  W-TEXT2 REDEFINES W-TEXT.                                        
025200         10  W-GRUPP         PIC X(12).                                   
025300         10  W-IDANSK-FROM   PIC Z(2)9.                                   
025400         10  FILLER          PIC X.                                       
025500         10  W-STRECK        PIC X.                                       
025600         10  FILLER          PIC X.                                       
025700         10  W-IDANSK-TOM    PIC Z(2)9.                                   
025800     05  FILLER              PIC X(20).                                   
025900     05  FILLER              PIC X(6)    VALUE 'DATUM'.                   
026000     05  W-AAR               PIC X(3).                                    
026100     05  W-MAANAD            PIC X(3).                                    
026200     05  W-DAG               PIC X(15).                                   
026300     05  FILLER              PIC X(4)    VALUE 'SID'.                     
026400     05  W-SIDNR             PIC Z(2)9.                                   
026500     SKIP2                                                                
026600 01  W-RUB1.                                                              
026700     05  FILLER              PIC X(10).                                   
026800     05  W-RUB1-URVALSINFO   PIC X(100).                                  
026900     EJECT                                                                
027000*--------------------------------------- ARBETSFÄLT TILL LISTA            
027100*                                        SERVICEGRAD PER ANSKAFFAR        
027200 01  SAW.                                                                 
027300     05  SAW-RADSTYR         PIC 9(2)                COMP-3.              
027400     05  SAW-RADANT          PIC S9(3)               COMP-3.              
027500     05  SAW-SIDANT          PIC S9(5)               COMP-3.              
027600     05  SAW-RADMAX          PIC S9(3)   VALUE +40   COMP-3.              
027700     SKIP2                                                                
027800     05  SAW-RUB2.                                                        
027900         10  FILLER          PIC X(16)   VALUE '   ART.NR PT'.            
028000         10  FILLER          PIC X(15)   VALUE 'RO-   DISP-'.             
028100         10  FILLER          PIC X(12)   VALUE 'AK-   UTR-'.              
028200         10  FILLER          PIC X(6)    VALUE 'SPÄRR'.                   
028300         10  FILLER          PIC X(7)    VALUE 'SÄKERH'.                  
028400         10  FILLER          PIC X(8)    VALUE 'AVSÄND'.                  
028500         10  FILLER          PIC X(9)    VALUE 'AVSÄND'.                  
028600         10  FILLER          PIC X(7)    VALUE 'TOTALT'.                  
028700         10  FILLER          PIC X(10)   VALUE 'PROG S'.                  
028800         10  FILLER          PIC X(11)   VALUE 'EK INV RO-'.              
028900         10  FILLER          PIC X(9)    VALUE 'RAD  ANT'.                
029000         10  FILLER          PIC X(9)    VALUE 'RAD/  RO-'.               
029100         10  FILLER          PIC X(2)    VALUE ' N'.                      
029200     SKIP2                                                                
029300     05  SAW-RUB3.                                                        
029400         10  FILLER          PIC X(10)   VALUE SPACE.                     
029500         10  FILLER          PIC X(4)    VALUE 'R2'.                      
029600         10  FILLER          PIC X(8)    VALUE 'SALDO'.                   
029700         10  FILLER          PIC X(7)    VALUE 'LAGER'.                   
029800         10  FILLER          PIC X(7)    VALUE 'TILLG'.                   
029900         10  FILLER          PIC X(7)    VALUE 'SALDO'.                   
030000         10  FILLER          PIC X(7)    VALUE 'KVANT'.                   
030100         10  FILLER          PIC X(7)    VALUE 'LAGER'.                   
030200         10  FILLER          PIC X(8)    VALUE 'DATUM'.                   
030300         10  FILLER          PIC X(12)   VALUE 'KVANT'.                   
030400         10  FILLER          PIC X(16)   VALUE 'PB  DAT P'.               
030500         10  FILLER          PIC X(4)    VALUE 'DAT'.                     
030600         10  FILLER          PIC X(4)    VALUE 'DAT'.                     
030700         10  FILLER          PIC X(5)    VALUE 'DAT'.                     
030800         10  FILLER          PIC X(6)    VALUE 'RAD'.                     
030900         10  FILLER          PIC X(4)    VALUE 'VA'.                      
031000         10  FILLER          PIC X(3)    VALUE 'TKR'.                     
031100         10  FILLER          PIC X(2)    VALUE '  '.                      
031200     SKIP2                                                                
031300     05  SAW-LEV-RUB.                                                     
031400         10  FILLER          PIC X(27)   VALUE SPACE.                     
031500         10  FILLER          PIC X(6)    VALUE '****'.                    
031600         10  FILLER          PIC X(5)    VALUE 'LEV'.                     
031700         10  SAW-RUB-IDLEVNR PIC X(5).                                    
031800         10  FILLER          PIC X(4)    VALUE SPACE.                     
031900         10  FILLER          PIC X(4)    VALUE '****'.                    
032000     SKIP2                                                                
032100     05  SAW-TOT-RUB1.                                                    
032200         10  FILLER          PIC X(28)   VALUE SPACE.                     
032300         10  FILLER          PIC X(4)    VALUE 'C'.                       
032400         10  FILLER          PIC X(8)    VALUE 'SERV'.                    
032500         10  FILLER          PIC X(11)   VALUE 'RAD/'.                    
032600         10  FILLER          PIC X(16)   VALUE 'ANT'.                     
032700         10  FILLER          PIC X(9)    VALUE 'RO-'.                     
032800         10  FILLER          PIC X(8)    VALUE 'OI-'.                     
032900         10  FILLER          PIC X(6)    VALUE 'EJ LEV'.                  
033000     SKIP2                                                                
033100     05  SAW-TOT-RUB2.                                                    
033200         10  FILLER          PIC X(28)   VALUE SPACE.                     
033300         10  FILLER          PIC X(4)    VALUE 'L'.                       
033400         10  FILLER          PIC X(10)   VALUE 'PROC'.                    
033500         10  FILLER          PIC X(9)    VALUE 'VA'.                      
033600         10  FILLER          PIC X(14)   VALUE 'ART'.                     
033700         10  FILLER          PIC X(9)    VALUE 'VÄRDE'.                   
033800         10  FILLER          PIC X(10)   VALUE 'RADER'.                   
033900         10  FILLER          PIC X(6)    VALUE 'EJ  RO'.                  
034000     05  SAW-LEV-TOT.                                                     
034100         10  FILLER          PIC X(19)   VALUE SPACE.                     
034200         10  FILLER          PIC X(6)    VALUE '****'.                    
034300         10  FILLER          PIC X(5)    VALUE 'LEV'.                     
034400         10  SAW-TOT-IDLEVNR PIC X(5).                                    
034500         10  FILLER          PIC X(4)    VALUE SPACE.                     
034600         10  FILLER          PIC X(8)    VALUE 'ANT ART'.                 
034700         10  SAW-LEV-ANTART  PIC Z(3)9.                                   
034800         10  FILLER          PIC X(2)    VALUE SPACE.                     
034900         10  FILLER          PIC X(9)    VALUE 'RO-VÄRDE'.                
035000         10  SAW-LEV-ROVAERDE PIC Z(2)BZ(3)BZ(2)9.9(2).                   
035100         10  FILLER          PIC X(2)    VALUE SPACE.                     
035200         10  FILLER          PIC X(10)   VALUE 'ANT RADER '.              
035300         10  SAW-LEV-KVRORAD PIC Z(1)BZ(3)BZ(2)9(1).                      
035400         10  FILLER          PIC X(25)   VALUE '  ****'.                  
035500     SKIP2                                                                
035600     05  SAW-RAD1.                                                        
035700         10  SAW-IDARTNR     PIC Z(9).                                    
035800         10  FILLER          PIC X.                                       
035900         10  SAW-KDPRIO      PIC Z.                                       
036000         10  SAW-FLTOPP      PIC X.                                       
036100         10  FILLER          PIC X.                                       
036200         10  SAW-KVROS       PIC Z(6)-.                                   
036300         10  SAW-KVROS-AST   REDEFINES SAW-KVROS                          
036400                             PIC X(7).                                    
036500         10  FILLER          PIC X.                                       
036600         10  SAW-KVDISPL     PIC Z(6)-.                                   
036700         10  SAW-KVDISPL-AST REDEFINES SAW-KVDISPL                        
036800                             PIC X(7).                                    
036900         10  SAW-KVAKS-TILLG PIC Z(6)-.                                   
037000         10  SAW-KVTILLG-AST REDEFINES SAW-KVAKS-TILLG                    
037100                             PIC X(7).                                    
037200         10  SAW-KVUTRS      PIC Z(6)-.                                   
037300         10  SAW-KVUTRS-AST  REDEFINES SAW-KVUTRS                         
037400                             PIC X(7).                                    
037500         10  SAW-KVSPANT     PIC Z(6)-.                                   
037600         10  SAW-KVSPANT-AST REDEFINES SAW-KVSPANT                        
037700                             PIC X(7).                                    
037800         10  SAW-KVSLAGER    PIC Z(6)-.                                   
037900         10  SAW-KVSLAG-AST  REDEFINES SAW-KVSLAGER                       
038000                             PIC X(7).                                    
038100         10  SAW-TIAVIDAT    PIC Z(6).                                    
038200         10  FILLER          PIC X(2).                                    
038300         10  SAW-KVAVIS      PIC Z(6)-.                                   
038400         10  FILLER          PIC X.                                       
038500         10  SAW-KVPB        PIC Z(4)9.9-.                                
038600         10  SAW-TIPBDAT     PIC Z(4).                                    
038700         10  FILLER          PIC X.                                       
038800         10  SAW-SP          PIC 9.                                       
038900         10  FILLER          PIC X(4).                                    
039000         10  SAW-KDERS       PIC 9(2).                                    
039100         10  FILLER          PIC X.                                       
039200         10  SAW-TIINVDAT    PIC 9(3).                                    
039300         10  FILLER          PIC X.                                       
039400         10  SAW-TIRODAT     PIC 9(3).                                    
039500         10  FILLER          PIC X.                                       
039600         10  SAW-RADDAT      PIC 9(3).                                    
039700         10  FILLER          PIC X.                                       
039800         10  SAW-KVRORAD     PIC Z(4)-.                                   
039900         10  SAW-KVRORADVKA  PIC Z(4)-.                                   
040000         10  SAW-ROTKR       PIC Z(4).                                    
040100         10  FILLER          PIC X.                                       
040200         10  SAW-TESTWEEK    PIC X(1).                                    
040300     SKIP2                                                                
040400     05  SAW-RAD2.                                                        
040500         10  FILLER          PIC X(3)    VALUE SPACE.                     
040600         10  SAW-BEART-SVE   PIC X(11).                                   
040700         10  SAW-KVAVIS-LEVBESK-1                                         
040800                             PIC Z(9)-.                                   
040900         10  SAW-TIAVIDAT-LEVBESK-1                                       
041000                             PIC Z(3).                                    
041100         10  SAW-KVAVIS-LEVBESK-2                                         
041200                             PIC Z(10)-.                                  
041300         10  SAW-TIAVIDAT-LEVBESK-2                                       
041400                             PIC Z(3).                                    
041500         10  SAW-KVAVIS-LEVBESK-3                                         
041600                             PIC Z(10)-.                                  
041700         10  SAW-TIAVIDAT-LEVBESK-3                                       
041800                             PIC Z(3).                                    
041900         10  FILLER          PIC X(3)    VALUE SPACE.                     
042000         10  FILLER          PIC X(5)    VALUE 'BEST'.                    
042100         10  SAW-KVBR        PIC Z(7)-.                                   
042200         10  FILLER          PIC X(1)    VALUE SPACE.                     
042300         10  FILLER          PIC X(4)    VALUE 'LTK'.                     
042400         10  SAW-KDLTK       PIC 9.                                       
042500         10  FILLER          PIC X(2)    VALUE SPACE.                     
042600         10  FILLER          PIC X(3)    VALUE 'HF'.                      
042700         10  SAW-KDHF        PIC 9.                                       
042800         10  SAW-ASTERISK    PIC X(4).                                    
042900         10  FILLER          PIC X(3)    VALUE 'GK'.                      
043000         10  SAW-KDGK        PIC 9B.                                      
043100         10  FILLER          PIC X(2)    VALUE 'PS'.                      
043200         10  SAW-KDPRODSL    PIC Z(1)9(2).                                
043300         10  FILLER          PIC X(2)    VALUE SPACE.                     
043310         10  FILLER          PIC X(4)    VALUE 'PUV'.                     
043400         10  SAW-TIFINLV     PIC Z(4).                                    
043500         10  FILLER          PIC X(7)    VALUE '  VVKL '.                 
043600         10  SAW-KDVVKL      PIC 9(1).                                    
043800     SKIP2                                                                
043900     05  SAW-TOT-RAD.                                                     
044000         10  FILLER          PIC X(7)    VALUE SPACE.                     
044100         10  SAW-ORDERKLASS  PIC X(21).                                   
044200         10  SAW-LAGER       PIC X(3).                                    
044300         10  SAW-SERVPROC    PIC Z(2)9.9.                                 
044400         10  FILLER          PIC X(2)    VALUE SPACE.                     
044500         10  SAW-KVRORAD-VKA PIC Z(6)-.                                   
044600         10  SAW-TOT-ANTART  PIC Z(9).                                    
044700         10  FILLER          PIC X(4)    VALUE SPACE.                     
044800         10  SAW-TOT-ROVAERDE PIC ZBZ(3)BZ(2)9.99.                        
044900         10  SAW-KVINORD     PIC Z(9)-.                                   
045000         10  SAW-ANTEJLEV    PIC Z(9).                                    
045100         10  FILLER          PIC X(23)   VALUE SPACE.                     
045200     SKIP2                                                                
045300     05  SAW-SKRIV-RAD.                                                   
045400         10  FILLER          PIC X(121).                                  
045500     EJECT                                                                
045600 01  FILLER                  PIC X(16)   VALUE ALL 'D'.                   
045700*--------------------------------------- ARBETSFÄLT TILL LISTA            
045800*                                        SERVICEGRAD PER GRUPP            
045900*                                        RADERNA ANVÄNDS ÄVEN TILL        
046000*                                        S-GRAD SEKTION OCH TOTALT        
046100 01  SGW.                                                                 
046200     05  SGW-RADSTYR         PIC 9(2)                COMP-3.              
046300     05  SGW-RADANT          PIC S9(3)               COMP-3.              
046400     05  SGW-SIDANT          PIC S9(5)               COMP-3.              
046500     05  SGW-RADMAX          PIC S9(3)   VALUE +41   COMP-3.              
046600     SKIP2                                                                
046700     05  SGW-RUB1.                                                        
046800         10  FILLER          PIC X(9)    VALUE SPACE.                     
046900         10  FILLER          PIC X(15)   VALUE '  C 1  '.                 
047000         10  FILLER          PIC X(12)   VALUE '* * * *'.                 
047100         10  FILLER          PIC X(10)   VALUE '  C 1  '.                 
047200         10  FILLER          PIC X(15)   VALUE '  C 2  '.                 
047300         10  FILLER          PIC X(12)   VALUE '* * * *'.                 
047400         10  FILLER          PIC X(10)   VALUE '  C 2  '.                 
047500         10  FILLER          PIC X(15)   VALUE ' T O T '.                 
047600         10  FILLER          PIC X(16)   VALUE '* * * *'.                 
047700         10  FILLER          PIC X(7)    VALUE ' T O T '.                 
047800         SKIP2                                                            
047900     05  SGW-RUB2.                                                        
048000         10  FILLER          PIC X       VALUE SPACE.                     
048100         10  FILLER          PIC X(8)    VALUE 'AG K'.                    
048200         10  FILLER          PIC X(14)   VALUE 'SERV  RAD/'.              
048300         10  FILLER          PIC X(6)    VALUE 'ANT'.                     
048400         10  FILLER          PIC X(6)    VALUE 'RO-'.                     
048500         10  FILLER          PIC X(4)    VALUE 'OI-'.                     
048600         10  FILLER          PIC X(8)    VALUE 'EJ LEV'.                  
048700         10  FILLER          PIC X(14)   VALUE 'SERV  RAD/'.              
048800         10  FILLER          PIC X(6)    VALUE 'ANT'.                     
048900         10  FILLER          PIC X(6)    VALUE 'RO-'.                     
049000         10  FILLER          PIC X(4)    VALUE 'OI-'.                     
049100         10  FILLER          PIC X(8)    VALUE 'EJ LEV'.                  
049200         10  FILLER          PIC X(6)    VALUE 'SERV'.                    
049300         10  FILLER          PIC X(8)    VALUE 'RAD/'.                    
049400         10  FILLER          PIC X(6)    VALUE 'ANT'.                     
049500         10  FILLER          PIC X(6)    VALUE 'RO-'.                     
049600         10  FILLER          PIC X(4)    VALUE 'OI-'.                     
049700         10  FILLER          PIC X(6)    VALUE 'EJ LEV'.                  
049800     SKIP2                                                                
049900     05  SGW-RUB3.                                                        
050000         10  FILLER          PIC X(4)    VALUE SPACE.                     
050100         10  FILLER          PIC X(6)    VALUE 'L'.                       
050200         10  FILLER          PIC X(7)    VALUE 'PRO'.                     
050300         10  FILLER          PIC X(6)    VALUE 'VA'.                      
050400         10  FILLER          PIC X(6)    VALUE 'ART'.                     
050500         10  FILLER          PIC X(4)    VALUE 'TKR'.                     
050600         10  FILLER          PIC X(6)    VALUE 'RADER'.                   
050700         10  FILLER          PIC X(9)    VALUE 'EJ RO'.                   
050800         10  FILLER          PIC X(7)    VALUE 'PRO'.                     
050900         10  FILLER          PIC X(6)    VALUE 'VA'.                      
051000         10  FILLER          PIC X(6)    VALUE 'ART'.                     
051100         10  FILLER          PIC X(4)    VALUE 'TKR'.                     
051200         10  FILLER          PIC X(6)    VALUE 'RADER'.                   
051300         10  FILLER          PIC X(9)    VALUE 'EJ RO'.                   
051400         10  FILLER          PIC X(7)    VALUE 'PRO'.                     
051500         10  FILLER          PIC X(6)    VALUE 'VA'.                      
051600         10  FILLER          PIC X(6)    VALUE 'ART'.                     
051700         10  FILLER          PIC X(4)    VALUE 'TKR'.                     
051800         10  FILLER          PIC X(6)    VALUE 'RADER'.                   
051900         10  FILLER          PIC X(5)    VALUE 'EJ RO'.                   
052000     SKIP2                                                                
052100     05  SGW-RAD1.                                                        
052200         10  SGW-IDANSK      PIC Z(2)9.                                   
052300         10  SGW-IDANSK-X    REDEFINES SGW-IDANSK                         
052400                             PIC X(3).                                    
052500         10  FILLER          PIC X.                                       
052600         10  SGW-KLASS       PIC X(4).                                    
052700         10  SGW-SERVPROC-CDC PIC Z(2)9.9.                                
052800         10  FILLER          PIC X.                                       
052900         10  SGW-KVRORAD-VKA-CDC                                          
053000                             PIC Z(5)-.                                   
053100         10  SGW-ANTART-CDC  PIC Z(6).                                    
053200         10  SGW-ROTKR-CDC   PIC Z(6).                                    
053300         10  SGW-KVINORD-CDC PIC Z(6)-.                                   
053400         10  SGW-ANTEJLEV-CDC PIC Z(5).                                   
053500         10  FILLER          PIC X(2).                                    
053600         10  SGW-SERVPROC-SDC PIC Z(2)9.9.                                
053700         10  FILLER          PIC X.                                       
053800         10  SGW-KVRORAD-VKA-SDC                                          
053900                             PIC Z(5)-.                                   
054000         10  SGW-ANTART-SDC  PIC Z(6).                                    
054100         10  SGW-ROTKR-SDC   PIC Z(6).                                    
054200         10  SGW-KVINORD-SDC PIC Z(6)-.                                   
054300         10  SGW-ANTEJLEV-SDC PIC Z(5).                                   
054400         10  FILLER          PIC X(2).                                    
054500         10  SGW-SERVPROC-T  PIC Z(2)9.9.                                 
054600         10  FILLER          PIC X.                                       
054700         10  SGW-KVRORAD-VKA-T                                            
054800                             PIC Z(5)-.                                   
054900         10  SGW-ANTART-T    PIC Z(6).                                    
055000         10  SGW-ROTKR-T     PIC Z(6).                                    
055100         10  SGW-KVINORD-T   PIC Z(6)-.                                   
055200         10  SGW-ANTEJLEV-T  PIC Z(5).                                    
055300         SKIP3                                                            
055400*--------------------------------------- ARBETSFÄLT TILL LISTA            
055500*                                        SERVICEGRAD PER SEKTION          
055600 01  SSW.                                                                 
055700     05  SSW-RADSTYR         PIC 9(2)                COMP-3.              
055800     05  SSW-RADANT          PIC S9(3)               COMP-3.              
055900     05  SSW-SIDANT          PIC S9(5)               COMP-3.              
056000     05  SSW-RADMAX          PIC S9(3)   VALUE +41   COMP-3.              
056100     SKIP3                                                                
056200*--------------------------------------- ARBETSFÄLT TILL LISTA            
056300*                                        SERVECEGRAD TOTALT               
056400 01  STW.                                                                 
056500     05  STW-RADSTYR         PIC 9(2)                COMP-3.              
056600     05  STW-RADANT          PIC S9(3)               COMP-3.              
056700     05  STW-SIDANT          PIC S9(5)               COMP-3.              
056800     05  STW-RADMAX          PIC S9(3)   VALUE +40   COMP-3.              
056900     EJECT                                                                
057000 01  FILLER                  PIC X(16)   VALUE ALL 'E'.                   
057100*--------------------------------------- TABELL ÖVER SERVICEGRAD          
057200*                                        NIVÅ 1 = PER ANSKAFFARE          
057300*                                        NIVÅ 2 = PER GRUPP               
057400*                                        NIVÅ 3 = PER SEKTION             
057500*                                        NIVÅ 4-23= SPECIAL TOTAL         
057600 01  ANS-TAB.                                                             
057700     05  ANS-NIVA            OCCURS 23.                                   
057800         10  ANS-ORDKLASS    OCCURS 3.                                    
057900             15  ANS-KVAVBRAD-CDC        PIC S9(7)V9(2)  COMP-3.          
058000             15  ANS-KVFYSAVV-CDC        PIC S9(5)V9(2)  COMP-3.          
058100             15  ANS-KVINORD-CDC         PIC S9(7)       COMP-3.          
058200             15  ANS-KVRORAD-VKA-CDC     PIC S9(7)V99    COMP-3.          
058300             15  ANS-ANTART-CDC          PIC S9(7)       COMP-3.          
058400             15  ANS-SUROBEL-CDC         PIC S9(9)V9(2)  COMP-3.          
058500             15  ANS-ANTEJLEVRO-CDC      PIC S9(7)V9(2)  COMP-3.          
058600             15  ANS-KVAVBRAD-SDC        PIC S9(7)V9(2)  COMP-3.          
058700             15  ANS-KVFYSAVV-SDC        PIC S9(5)V9(2)  COMP-3.          
058800             15  ANS-KVINORD-SDC         PIC S9(7)       COMP-3.          
058900             15  ANS-KVRORAD-VKA-SDC     PIC S9(7)V99    COMP-3.          
059000             15  ANS-ANTART-SDC          PIC S9(7)       COMP-3.          
059100             15  ANS-SUROBEL-SDC         PIC S9(9)V9(2)  COMP-3.          
059200             15  ANS-ANTEJLEVRO-SDC      PIC S9(7)V9(2)  COMP-3.          
059300             15  ANS-KVAVBRAD-T          PIC S9(7)V9(2)  COMP-3.          
059400             15  ANS-KVFYSAVV-T          PIC S9(5)V9(2)  COMP-3.          
059500             15  ANS-KVINORD-T           PIC S9(7)       COMP-3.          
059600             15  ANS-KVRORAD-VKA-T       PIC S9(7)V99    COMP-3.          
059700             15  ANS-ANTART-T            PIC S9(7)       COMP-3.          
059800             15  ANS-SUROBEL-T           PIC S9(9)V9(2)  COMP-3.          
059900             15  ANS-ANTEJLEVRO-T        PIC S9(7)V9(2)  COMP-3.          
060000     SKIP3                                                                
060100*--------------------------------------- NOLLPOST TILL ANS-TAB            
060200*                                                                         
060300 01  ANS-NOLL.                                                            
060400     05  FILLER              PIC S9(9)   VALUE ZERO  COMP-3.              
060500     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
060600     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
060700     05  FILLER              PIC S9(9)   VALUE ZERO  COMP-3.              
060800     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
060900     05  FILLER              PIC S9(11)  VALUE ZERO  COMP-3.              
061000     05  FILLER              PIC S9(9)   VALUE ZERO  COMP-3.              
061100     05  FILLER              PIC S9(9)   VALUE ZERO  COMP-3.              
061200     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
061300     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
061400     05  FILLER              PIC S9(9)   VALUE ZERO  COMP-3.              
061500     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
061600     05  FILLER              PIC S9(11)  VALUE ZERO  COMP-3.              
061700     05  FILLER              PIC S9(9)   VALUE ZERO  COMP-3.              
061800     05  FILLER              PIC S9(9)   VALUE ZERO  COMP-3.              
061900     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
062000     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
062100     05  FILLER              PIC S9(9)   VALUE ZERO  COMP-3.              
062200     05  FILLER              PIC S9(7)   VALUE ZERO  COMP-3.              
062300     05  FILLER              PIC S9(11)  VALUE ZERO  COMP-3.              
062400     05  FILLER              PIC S9(9)   VALUE ZERO  COMP-3.              
062500     EJECT                                                                
065600 PROCEDURE DIVISION.                                                      
065700 MAIN SECTION.                                                            
065800     SORT SRT-FIL                                                         
065900          ASCENDING SRT-IDLISTA                                           
066000                    SRT-IDANSK                                            
066100                    SRT-IDLEVNR                                           
066200                    SRT-IDARTNR                                           
066300          USING W22513                                                    
066400          OUTPUT PROCEDURE HUVUDSTYRDEL                                   
066500     SKIP2                                                                
066600     IF  SORT-RETURN > +0                                                 
066700         MOVE +20 TO RKOD                                                 
066800         DISPLAY '*** W22512, SORT-FEL'                                   
066900         CALL ABEND USING RKOD                                            
067000     END-IF                                                               
067100     MOVE +0 TO RETURN-CODE                                               
067200     GOBACK                                                               
067300     .                                                                    
067400     EJECT                                                                
067500 HUVUDSTYRDEL SECTION.                                                    
067600     PERFORM A-INITIERING                                                 
067700     PERFORM B-HAMTA-GRP-SEK                                              
067800                                                                          
067900     IF TID NOT = HIGH-VALUE                                              
068000     PERFORM UNTIL SW-W22513-EOF = JA                                     
068100*--------------------------------------- INITIERA IDSEKTION               
068200         MOVE SSW-RADMAX     TO SSW-RADANT                                
068300         MOVE ANS-NOLL       TO ANS-ORDKLASS (3, 1)                       
068400         MOVE ANS-NOLL       TO ANS-ORDKLASS (3, 2)                       
068500         MOVE ANS-NOLL       TO ANS-ORDKLASS (3, 3)                       
068600         MOVE W-SEKT-INTERVALL TO W-SEK-INTV                              
068700         MOVE W-FUNK-INTERVALL TO W-FUNK-INTV                             
068800         MOVE TID-IDSEKT        TO LID-IDSEKT                             
068900                                                                          
069000         PERFORM UNTIL TID-IDSEKT NOT = LID-IDSEKT                        
069100*--------------------------------------- INITIERA GRUPP                   
069200             MOVE SGW-RADMAX TO SGW-RADANT                                
069300             MOVE ANS-NOLL   TO ANS-ORDKLASS (2, 1)                       
069400             MOVE ANS-NOLL   TO ANS-ORDKLASS (2, 2)                       
069500             MOVE ANS-NOLL   TO ANS-ORDKLASS (2, 3)                       
069600             MOVE W-GRUPP-INTERVALL TO W-GRP-INTV                         
069700             MOVE TID-IDGRUPP    TO LID-IDGRUPP                           
069800                                                                          
069900             PERFORM UNTIL TID-IDSEKT-IDGRUPP NOT =                       
070000                                     LID-IDSEKT-IDGRUPP                   
070100*--------------------------------------- INITIERA ANSKAFFARE              
070200                 MOVE SAW-RADMAX TO SAW-RADANT                            
070300                 MOVE I13-IDANSK TO   W-SPAR-IDANSK                       
070400                 MOVE ANS-NOLL   TO ANS-ORDKLASS (1, 1)                   
070500                 MOVE ANS-NOLL   TO ANS-ORDKLASS (1, 2)                   
070600                 MOVE ANS-NOLL   TO ANS-ORDKLASS (1, 3)                   
070700                 MOVE TID-IDANSK TO LID-IDANSK                            
070800                                                                          
070900                 PERFORM UNTIL TID-IDSEKT-IDANSK NOT =                    
071000                                        LID-IDSEKT-IDANSK                 
071100*--------------------------------------- INITIERA LEVERANTÖR              
071200                     MOVE ZERO TO W-LEV-ANTART                            
071300                                  W-LEV-ROVAERDE                          
071400                                  W-LEV-KVRORAD                           
071500                     MOVE TID    TO LID                                   
071600                                                                          
071700                     PERFORM UNTIL TID NOT = LID                          
071800                         PERFORM D-ADDERA-ANSTAB                          
071900                         PERFORM C-SUMMERA-RORAD-VECKA                    
072000                         PERFORM E-SKRIV-DETRAD                           
072100                         PERFORM S01-LAS-TRANS                            
072200                         PERFORM B-HAMTA-GRP-SEK                          
072300                     END-PERFORM                                          
072400                                                                          
072500                     PERFORM F-SKRIV-TOT-LEV                              
072600                 END-PERFORM                                              
072700                                                                          
072800                 PERFORM G-SKRIV-TOT-ANSK                                 
072900                 MOVE 1          TO IXNS                                  
073000                 MOVE 2          TO IXNS2                                 
073100                 PERFORM S13-ADDERA-NASTA-NIVA                            
073300             END-PERFORM                                                  
073400                                                                          
073500             PERFORM J-SKRIV-TOT-GRUPP                                    
073600             MOVE 2          TO IXNS                                      
073700             MOVE 3          TO IXNS2                                     
073800             PERFORM S13-ADDERA-NASTA-NIVA                                
073900         END-PERFORM                                                      
074000                                                                          
074100         PERFORM K-SKRIV-TOT-SEK                                          
074200     END-PERFORM                                                          
074400     ELSE                                                                 
074500         PERFORM M-SKRIV-TOM-LISTA                                        
074600     END-IF                                                               
074700                                                                          
074800     PERFORM Z-AVSLUTNING                                                 
074900     .                                                                    
075000     EJECT                                                                
075100 A-INITIERING SECTION.                                                    
075200******************************************************************        
075300*    ÖPPNA ALLA FILER                                            *        
075500*    HÄMTA INFO FRÅN DATUMKORT                                   *        
075600*    INITIERA VISSA FÄLT                                         *        
075700*    LÄS EN TRANS OCH KOLLA OM DET FINNS URVALS-INFO             *        
075800******************************************************************        
075900     SKIP2                                                                
076000     OPEN OUTPUT SA-LISTA                                                 
076100                  SG-LISTA                                                
076200                  SS-LISTA                                                
076400     SKIP2                                                                
077400                                                                          
077500     CALL DATKORT USING W-PROGNAMN   DATUMKORT-ID DATUMKORT               
077600                                                                          
077700*--------------------------------------- INITIERA ARBETSFÄLT              
077800     MOVE ZERO TO SAW-SIDANT                                              
077900                   SGW-SIDANT                                             
078000                   SSW-SIDANT                                             
078100                   STW-SIDANT                                             
078200     MOVE D-AAR              TO   W-AAR                                   
078300     MOVE D-MAANAD           TO   W-MAANAD                                
078400     MOVE D-DAG              TO   W-DAG                                   
078500                                                                          
078600*-------------------------------------- NYTT AV JM-87                     
078700     MOVE K-AAR              TO   WS-TEST-A                               
078800     MOVE K-VECKA            TO   WS-TEST-VV                              
078900                                                                          
079000*-------------------------------------- SLUT JM-87                        
079100                                                                          
079200     MOVE SPACE TO SAW-RAD1                                               
079300                    SGW-RAD1                                              
079400     MOVE SAW-RADMAX TO SAW-RADANT                                        
079500                        SGW-RADANT                                        
079600                        SSW-RADANT                                        
079800     MOVE 3 TO SAW-RADSTYR                                                
079900               SGW-RADSTYR                                                
080000               SSW-RADSTYR                                                
080200                                                                          
080300*---------------------------------------- NOLLSTÄLL ANS-TABELL            
080400     MOVE 1 TO IXNS                                                       
080500     PERFORM UNTIL IXNS > 23                                              
080600         MOVE ANS-NOLL TO ANS-ORDKLASS (IXNS, 1)                          
080700                          ANS-ORDKLASS (IXNS, 2)                          
080800                          ANS-ORDKLASS (IXNS, 3)                          
080900         ADD 1 TO IXNS                                                    
081000     END-PERFORM                                                          
081100*---------------------------------------- INITIERA POSTSUM                
081200     MOVE 'W22513' TO POSTSUM-FDNAMN                                      
081300     MOVE 'W22512D1' TO POSTSUM-DDNAMN2                                   
081400     MOVE SPACE   TO POSTSUM-TRANSTYP                                     
081500     MOVE W-PROGNAMN         TO POSTSUM-PROGNAMN                          
081600     MOVE ZERO TO W-SPAR-IDANSK                                           
081700     SKIP2                                                                
081800*---------------------------------------- LÄS POST OCH LÄGG IN            
081900*                                         EV URVALSINFO I RUBRIK          
082000     PERFORM S01-LAS-TRANS                                                
082100                                                                          
082200     IF  SW-W22513-EOF = NEJ                                              
082300     IF  I13-IDLISTA = ZERO                                               
082400         MOVE I13P00-URVALS-INFO TO W-RUB1-URVALSINFO                     
082500         PERFORM S01-LAS-TRANS                                            
082600     ELSE                                                                 
082700         MOVE SPACE          TO W-RUB1-URVALSINFO                         
082800     END-IF                                                               
082900     END-IF                                                               
083000     .                                                                    
083100     EJECT                                                                
083200 B-HAMTA-GRP-SEK SECTION.                                                 
083300******************************************************************        
083400*    HÄMTAR GRUPPNR OCH SEKTIONSNR, GENOM ATT SKICKA MED         *        
083500*    ANSKAFFARNR TILL SUBPROGRAMMET W00942.                      *        
083600*    FYLL I IDBEGREPP                                            *        
083700*    OM DET ÄR SLUT PÅ POSTERNA I W22513 LÄGGS HIGH-VALUE I TID. *        
083800******************************************************************        
083900     SKIP2                                                                
084000     IF  SW-W22513-EOF = NEJ                                              
084100     MOVE I13-IDANSK         TO W-IDANSK                                  
084200     CALL W200ANSK USING W-W009W42                                        
084300     MOVE I13-IDLEVNR        TO TID-IDLEVNR                               
084400     MOVE I13-IDANSK         TO TID-IDANSK                                
084500     MOVE W-IDGRUPP          TO TID-IDGRUPP                               
084600     MOVE W-IDSEKT           TO TID-IDSEKT                                
084700     ELSE                                                                 
084800         MOVE HIGH-VALUE     TO TID                                       
084900     END-IF                                                               
085000     .                                                                    
085100     EJECT                                                                
085200 C-SUMMERA-RORAD-VECKA SECTION.                                           
085300******************************************************************        
085400*    BERÄKNA RESTORDERRADER/VECKA,ANSK O LEV                     *        
085500******************************************************************        
085600     SKIP2                                                                
085700     ADD I13-KVRORAD-CDC-1-2-VECKA I13-KVRORAD-CDC-3-4-VECKA              
085800         I13-KVRORAD-SDC-1-2-VECKA I13-KVRORAD-SDC-3-4-VECKA              
085900         TO W-LEV-KVRORAD                                                 
086000     .                                                                    
086100     EJECT                                                                
086200 D-ADDERA-ANSTAB SECTION.                                                 
086300******************************************************************        
086400*    ADDERA TILL ANSTAB NIVÅ IXNS=1 (ANSKAFFARE)                 *        
086500*    INDEX 1 = ORDERKLASS 1-2                                    *        
086600*    INDEX 2 = ORDERKLASS 3-4                                    *        
086700*    INDEX 3 = ORDERKLASS 1-4 (TOTALT)                           *        
086800******************************************************************        
086900     SKIP2                                                                
087000     MOVE 1                  TO IXNS                                      
087100*                                                                         
087200*--------------------------------------- RÄKNA UT SUROBEL                 
087300*                                                                         
087400     MULTIPLY I13-KVROS-CDC-1-2 BY I13-PRARTSTD                           
087500                               GIVING W-SUROBEL-CDC-1-2                   
087600     MULTIPLY I13-KVROS-CDC-3-4 BY I13-PRARTSTD                           
087700                               GIVING W-SUROBEL-CDC-3-4                   
087800     MULTIPLY I13-KVROS-SDC-1-2 BY I13-PRARTSTD                           
087900                               GIVING W-SUROBEL-SDC-1-2                   
088000     MULTIPLY I13-KVROS-SDC-3-4 BY I13-PRARTSTD                           
088100                               GIVING W-SUROBEL-SDC-3-4                   
088200     SKIP2                                                                
088300     MOVE NEJ  TO SW-CDC-ORDERKLASS-1-2                                   
088400                  SW-CDC-ORDERKLASS-3-4                                   
088500                  SW-SDC-ORDERKLASS-1-2                                   
088600                  SW-SDC-ORDERKLASS-3-4                                   
088700     PERFORM DA-SATT-ADD-FLAGGOR                                          
088800     SKIP2                                                                
088900*---------------------------------------- LAGER C1                        
089000     ADD I13-KVAVBRAD-CDC-1-2 TO ANS-KVAVBRAD-CDC (IXNS, 1)               
089100     ADD I13-KVAVBRAD-CDC-3-4 TO ANS-KVAVBRAD-CDC (IXNS, 2)               
089200     ADD I13-KVFYSAVV-CDC-1-2 TO ANS-KVFYSAVV-CDC (IXNS, 1)               
089300     ADD I13-KVFYSAVV-CDC-3-4 TO ANS-KVFYSAVV-CDC (IXNS, 2)               
089400     ADD I13-KVINORD-CDC-1-2 TO ANS-KVINORD-CDC (IXNS, 1)                 
089500     ADD I13-KVINORD-CDC-3-4 TO ANS-KVINORD-CDC (IXNS, 2)                 
089600     ADD W-SUROBEL-CDC-1-2   TO ANS-SUROBEL-CDC (IXNS, 1)                 
089700     ADD W-SUROBEL-CDC-3-4   TO ANS-SUROBEL-CDC (IXNS, 2)                 
089800     ADD I13-KVRORAD-CDC-1-2-VECKA TO ANS-KVRORAD-VKA-CDC(IXNS, 1)        
089900     ADD I13-KVRORAD-CDC-3-4-VECKA TO ANS-KVRORAD-VKA-CDC(IXNS, 2)        
090000     SKIP2                                                                
090100*--------------------------------------- LAGER C2                         
090200*                                                                         
090300     ADD I13-KVAVBRAD-SDC-1-2 TO ANS-KVAVBRAD-SDC (IXNS, 1)               
090400     ADD I13-KVAVBRAD-SDC-3-4 TO ANS-KVAVBRAD-SDC (IXNS, 2)               
090500     ADD I13-KVFYSAVV-SDC-1-2 TO ANS-KVFYSAVV-SDC (IXNS, 1)               
090600     ADD I13-KVFYSAVV-SDC-3-4 TO ANS-KVFYSAVV-SDC (IXNS, 2)               
090700     ADD I13-KVINORD-SDC-1-2 TO ANS-KVINORD-SDC (IXNS, 1)                 
090800     ADD I13-KVINORD-SDC-3-4 TO ANS-KVINORD-SDC (IXNS, 2)                 
090900     ADD W-SUROBEL-SDC-1-2   TO ANS-SUROBEL-SDC (IXNS, 1)                 
091000     ADD W-SUROBEL-SDC-3-4   TO ANS-SUROBEL-SDC (IXNS, 2)                 
091100     ADD I13-KVRORAD-SDC-1-2-VECKA TO ANS-KVRORAD-VKA-SDC(IXNS, 1)        
091200     ADD I13-KVRORAD-SDC-3-4-VECKA TO ANS-KVRORAD-VKA-SDC(IXNS, 2)        
091300*                                                                         
091400*--------------------------------------- RÄKNA ANTALET ARTIKLAR           
091500*                                                                         
091600         IF  SW-CDC-ORDERKLASS-1-2 = JA                                   
091700             ADD 1   TO ANS-ANTART-CDC (IXNS, 1)                          
091800         END-IF                                                           
091900*                                                                         
092000         IF  SW-CDC-ORDERKLASS-3-4 = JA                                   
092100             ADD 1   TO ANS-ANTART-CDC (IXNS, 2)                          
092200         END-IF                                                           
092300*                                                                         
092400         IF  SW-SDC-ORDERKLASS-1-2 = JA                                   
092500             ADD 1   TO ANS-ANTART-SDC (IXNS, 1)                          
092600         END-IF                                                           
092700*                                                                         
092800         IF  SW-SDC-ORDERKLASS-3-4 = JA                                   
092900             ADD 1 TO ANS-ANTART-SDC (IXNS, 2)                            
093000         END-IF                                                           
093100*                                                                         
093200         IF  SW-CDC-ORDERKLASS-1-2 = JA                                   
093300         OR  SW-SDC-ORDERKLASS-1-2 = JA                                   
093400             ADD 1 TO ANS-ANTART-T (IXNS, 1)                              
093500         END-IF                                                           
093600*                                                                         
093700         IF  SW-CDC-ORDERKLASS-3-4 = JA                                   
093800         OR  SW-SDC-ORDERKLASS-3-4 = JA                                   
093900             ADD 1 TO ANS-ANTART-T (IXNS, 2)                              
094000             END-IF                                                       
094100*                                                                         
094200         IF  SW-CDC-ORDERKLASS-1-2 = JA                                   
094300         OR  SW-CDC-ORDERKLASS-3-4 = JA                                   
094400         OR  SW-SDC-ORDERKLASS-1-2 = JA                                   
094500         OR  SW-SDC-ORDERKLASS-3-4 = JA                                   
094600             ADD 1 TO  ANS-ANTART-T (IXNS, 3)                             
094700         END-IF                                                           
094800*                                                                         
094900          IF SW-CDC-ORDERKLASS-1-2 = JA                                   
095000          OR SW-CDC-ORDERKLASS-3-4 = JA                                   
095100             ADD 1 TO ANS-ANTART-CDC (IXNS, 3)                            
095200          END-IF                                                          
095300*                                                                         
095400          IF  SW-SDC-ORDERKLASS-1-2 = JA                                  
095500          OR  SW-SDC-ORDERKLASS-3-4 = JA                                  
095600              ADD 1 TO ANS-ANTART-SDC (IXNS, 3)                           
095700          END-IF                                                          
095800     SKIP3                                                                
095900*--------------------------------------- RÄKNA ANTAL ARTIKLAR             
096000*                                        SOM EJ ÄR LEVERERADE             
096100*                                        OCH EJ RESTNOTERADE              
096200*                                                                         
096300*--------------------------------------- LAGER C1                         
096400*                                        ORDERKLASS 1-2                   
096500     ADD I13-KVEJRO-CDC-1-2                                               
096600                 TO ANS-ANTEJLEVRO-CDC (IXNS, 1)                          
096700                    ANS-ANTEJLEVRO-T (IXNS, 1)                            
096800*                                                                         
096900*--------------------------------------- LAGER C1                         
097000*                                        ORDERKLASS 3-4                   
097100     ADD I13-KVEJRO-CDC-3-4                                               
097200                 TO ANS-ANTEJLEVRO-CDC (IXNS, 2)                          
097300                    ANS-ANTEJLEVRO-T (IXNS, 2)                            
097400*                                                                         
097500*--------------------------------------- LAGER C2                         
097600*                                        ORDERKLASS 1-2                   
097700     ADD I13-KVEJRO-SDC-1-2                                               
097800                 TO ANS-ANTEJLEVRO-SDC (IXNS, 1)                          
097900                    ANS-ANTEJLEVRO-T (IXNS, 1)                            
098000*                                                                         
098100*-------------------------------------- LAGER C2                          
098200*                                       ORDERKLASS 3-4                    
098300     ADD I13-KVEJRO-SDC-3-4                                               
098400                 TO ANS-ANTEJLEVRO-SDC (IXNS, 2)                          
098500                    ANS-ANTEJLEVRO-T  (IXNS, 2)                           
098600     .                                                                    
098700     EJECT                                                                
098800 DA-SATT-ADD-FLAGGOR SECTION.                                             
098900******************************************************************        
099000*    TESTAR OM KVROS                                             *        
099100*    ÄR SKILD  FRÅN 0, I SÅ FALL SÄTTS FLAGGOR TILL JA.          *        
099200******************************************************************        
099300         IF  I13-KVROS-CDC-1-2   NOT = ZERO                               
099400             MOVE JA TO SW-CDC-ORDERKLASS-1-2                             
099500         END-IF                                                           
099600*                                                                         
099700*--------------------------------------- LAGER C1                         
099800*                                        ORDERKLASS 3-4                   
099900         IF  I13-KVROS-CDC-3-4   NOT = ZERO                               
100000             MOVE JA TO SW-CDC-ORDERKLASS-3-4                             
100100         END-IF                                                           
100200*                                                                         
100300*--------------------------------------- LAGER C2                         
100400*                                        ORDERKLASS 1-2                   
100500         IF  I13-KVROS-SDC-1-2   NOT = ZERO                               
100600             MOVE JA TO SW-SDC-ORDERKLASS-1-2                             
100700         END-IF                                                           
100800*                                                                         
100900*-------------------------------------- LAGER C2                          
101000*                                       ORDERKLASS 3-4                    
101100         IF  I13-KVROS-SDC-3-4 NOT = ZERO                                 
101200             MOVE JA TO SW-SDC-ORDERKLASS-3-4                             
101300         END-IF                                                           
101400     .                                                                    
101500     EJECT                                                                
101600 E-SKRIV-DETRAD SECTION.                                                  
101700******************************************************************        
101800*    SKRIVER C1-RAD, C2-RAD OCH GEMENSAM-RAD                     *        
101900*    BEROENDE PÅ KVROS                                           *        
102000*    OM SUROBEL = 0    SKRIVS INTE NÅGRA RADER UT                *        
102100*    ADDERAR TILL LEVACKAR (ANTART, ROVÄRDE)                     *        
102200******************************************************************        
102300     SKIP2                                                                
102400     IF (I13-KVROS-CDC-1-2 + I13-KVROS-CDC-3-4 > ZERO)                    
102500     OR (I13-KVROS-SDC-1-2 + I13-KVROS-SDC-3-4 > ZERO)                    
102600                                                                          
102700         IF  SW-SKRIV-TOT-LEVR = NEJ                                      
102800             PERFORM ED-SKRIV-LEV-RUBR                                    
102900         END-IF                                                           
103000         PERFORM EA-SKRIV-CDC-RAD                                         
103100                                                                          
103200         IF  I13-KDCLPOST = 0                                             
103300         OR  I13-KDCLPOST = 2                                             
103400             PERFORM EB-SKRIV-SDC-RAD                                     
103500         END-IF                                                           
103600         PERFORM EC-SKRIV-GEMENSAM-RAD                                    
103700*                                                                         
103800*--------------------------------------- ADDERA LEVACKAR                  
103900*                                                                         
104000         ADD 1 TO W-LEV-ANTART                                            
104100         ADD     I13-SUROBEL-CDC                                          
104200             I13-SUROBEL-SDC                                              
104300                             TO W-LEV-ROVAERDE                            
104400     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700 EA-SKRIV-CDC-RAD SECTION.                                                
104800******************************************************************        
104900*    REDIGERAR INFORMATION OM C1-LAGER TILL SAW-RAD              *        
105000*    OM DET INTE FÅR PLATS 3 RADER PER ARTIKEL PÅ SIDAN          *        
105100*    SÅ BÖRJAR MAN PÅ NY SIDA                                    *        
105200******************************************************************        
105300     SKIP2                                                                
105400     MOVE I13-IDARTNR        TO SAW-IDARTNR                               
105500     MOVE I13-KDPRIO-CDC     TO SAW-KDPRIO                                
105600     MOVE ALL '*' TO SAW-KVROS-AST                                        
105700                      SAW-KVDISPL-AST                                     
105800                      SAW-KVTILLG-AST                                     
105900                      SAW-KVUTRS-AST                                      
106000                      SAW-KVSPANT-AST                                     
106100                      SAW-KVSLAG-AST                                      
106200     MOVE SPACE TO SAW-FLTOPP                                             
106300                                                                          
106400     IF  I13-FLTOPP-CDC = JA                                              
106500         MOVE '*'            TO SAW-FLTOPP                                
106600     END-IF                                                               
106700     ADD I13-KVROS-CDC-1-2 I13-KVROS-CDC-3-4 GIVING W-SUMMA               
106800                                                                          
106900     IF  W-SUMMA    < 1000000                                             
107000         MOVE W-SUMMA        TO SAW-KVROS                                 
107100     END-IF                                                               
107200                                                                          
107300     IF  I13-KVDISPL-CDC <   1000000                                      
107400         MOVE I13-KVDISPL-CDC TO SAW-KVDISPL                              
107500     END-IF                                                               
107600                                                                          
107700     IF  I13-KVAKS-TILLG-CDC <   1000000                                  
107800         MOVE I13-KVAKS-TILLG-CDC TO SAW-KVAKS-TILLG                      
107900     END-IF                                                               
108000                                                                          
108100     IF  I13-KVUTRS-CDC <   1000000                                       
108200         MOVE I13-KVUTRS-CDC TO SAW-KVUTRS                                
108300     END-IF                                                               
108400                                                                          
108500     IF  I13-KVSPANT-CDC <   1000000                                      
108600         MOVE I13-KVSPANT-CDC TO SAW-KVSPANT                              
108700     END-IF                                                               
108800                                                                          
108900     IF  I13-KVSLAGER-CDC <   1000000                                     
109000         MOVE I13-KVSLAGER-CDC TO SAW-KVSLAGER                            
109100     END-IF                                                               
109200                                                                          
109300     MOVE I13-TIAVIDAT-CDC-SEN   TO SAW-TIAVIDAT                          
109400     MOVE I13-KVAVIS-CDC     TO SAW-KVAVIS                                
109500     MOVE I13-KVPB-TOT-CDC   TO SAW-KVPB                                  
109600     MOVE I13-TIPBDAT-CDC    TO SAW-TIPBDAT                               
109700     MOVE I13-KDSPARR-CDC    TO SAW-SP                                    
109800                                                                          
109900     MOVE I13-KDERS-CDC      TO SAW-KDERS                                 
110000     MOVE I13-TIINVDAT-CDC   TO W-DATUM-VKA-N                             
110100     MOVE W-A-VV             TO SAW-TIINVDAT                              
110200     MOVE I13-TIRODAT-CDC   TO W-DATUM-VKA-N                              
110300     MOVE W-A-VV             TO SAW-TIRODAT                               
110400     MOVE I13-TIRODAT-ORDER-CDC  TO SAW-RADDAT                            
110500                                                                          
110600*                                                                         
110700*----------------------------------------------NYTT JM-87                 
110800*                                                                         
110900     MOVE SPACE           TO SAW-TESTWEEK                                 
111000                                                                          
111100     IF SAW-TIRODAT = WS-TEST-DATUM                                       
111200        IF SAW-RADDAT = WS-TEST-DATUM                                     
111300           MOVE '*'             TO SAW-TESTWEEK                           
111400        END-IF                                                            
111500     END-IF                                                               
111600                                                                          
111700*                                                                         
111800*----------------------------------------------SLUT JM-87                 
111900     MOVE I13-KVRORAD-CDC-TOT TO SAW-KVRORAD                              
112000     ADD I13-KVRORAD-CDC-1-2-VECKA I13-KVRORAD-CDC-3-4-VECKA              
112100                             GIVING SAW-KVRORADVKA                        
112200                                                                          
112300     MOVE I13-SUROBEL-CDC TO W-ROTKR                                      
112400     PERFORM S12-AVRUNDA-SUROBEL                                          
112500     MOVE W-ROTKR TO SAW-ROTKR                                            
112600*                                                                         
112700*--------------------------------------- KONTROLLERAR ATT DET             
112800*                                        FÅR PLATS 3 RADER                
112900*                                        PÅ SIDAN                         
113000     MOVE 2                  TO SAW-RADSTYR                               
113100     IF  SAW-RADANT + 2 + SAW-RADSTYR GREATER SAW-RADMAX                  
113200         MOVE SAW-RADMAX     TO SAW-RADANT                                
113300     END-IF                                                               
113400     MOVE SAW-RAD1 TO SAW-SKRIV-RAD                                       
113500     PERFORM S02-STYR-SIDA-SA                                             
113600     MOVE SPACE TO SAW-RAD1                                               
113700     .                                                                    
113800     EJECT                                                                
113900 EB-SKRIV-SDC-RAD SECTION.                                                
114000******************************************************************        
114100*    REDIGERAR INFORMATION OM C2-LAGER TILL SAW-RAD              *        
114200******************************************************************        
114300     SKIP2                                                                
114400     MOVE I13-IDARTNR        TO SAW-IDARTNR                               
114500     MOVE I13-KDPRIO-SDC     TO SAW-KDPRIO                                
114600     MOVE ALL '*' TO SAW-KVROS-AST                                        
114700                      SAW-KVDISPL-AST                                     
114800                      SAW-KVTILLG-AST                                     
114900                      SAW-KVUTRS-AST                                      
115000                      SAW-KVSPANT-AST                                     
115100                      SAW-KVSLAG-AST                                      
115200     MOVE SPACE TO SAW-FLTOPP                                             
115300                                                                          
115400     IF  I13-FLTOPP-SDC = JA                                              
115500         MOVE '*'            TO SAW-FLTOPP                                
115600     END-IF                                                               
115700     ADD I13-KVROS-SDC-1-2 I13-KVROS-SDC-3-4 GIVING W-SUMMA               
115800                                                                          
115900     IF  W-SUMMA    < 1000000                                             
116000         MOVE W-SUMMA        TO SAW-KVROS                                 
116100     END-IF                                                               
116200                                                                          
116300     IF  I13-KVDISPL-SDC < 1000000                                        
116400         MOVE I13-KVDISPL-SDC TO SAW-KVDISPL                              
116500     END-IF                                                               
116600                                                                          
116700     IF  I13-KVAKS-TILLG-SDC < 1000000                                    
116800         MOVE I13-KVAKS-TILLG-SDC TO SAW-KVAKS-TILLG                      
116900     END-IF                                                               
117000                                                                          
117100     IF  I13-KVUTRS-SDC < 1000000                                         
117200         MOVE I13-KVUTRS-SDC TO SAW-KVUTRS                                
117300     END-IF                                                               
117400                                                                          
117500     IF  I13-KVSPANT-SDC < 1000000                                        
117600         MOVE I13-KVSPANT-SDC TO SAW-KVSPANT                              
117700     END-IF                                                               
117800                                                                          
117900     IF  I13-KVSLAGER-SDC < 1000000                                       
118000         MOVE I13-KVSLAGER-SDC TO SAW-KVSLAGER                            
118100     END-IF                                                               
118200                                                                          
118300     MOVE I13-TIAVIDAT-SDC-SEN   TO SAW-TIAVIDAT                          
118400     MOVE I13-KVAVIS-SDC     TO SAW-KVAVIS                                
118500     MOVE I13-KVPB-TOT-SDC   TO SAW-KVPB                                  
118600     MOVE I13-TIPBDAT-SDC    TO SAW-TIPBDAT                               
118700     MOVE I13-KDSPARR-SDC    TO SAW-SP                                    
118800     MOVE SPACE              TO SAW-ASTERISK                              
118900                                                                          
119000     MOVE I13-KDERS-SDC      TO SAW-KDERS                                 
119100     MOVE I13-TIINVDAT-SDC   TO W-DATUM-VKA-N                             
119200     MOVE W-A-VV             TO SAW-TIINVDAT                              
119300     MOVE I13-TIRODAT-SDC   TO W-DATUM-VKA-N                              
119400     MOVE W-A-VV             TO SAW-TIRODAT                               
119500     MOVE I13-TIRODAT-ORDER-SDC  TO SAW-RADDAT                            
119600                                                                          
119700*                                                                         
119800*----------------------------------------------NYTT JM-87                 
119900*                                                                         
120000     MOVE SPACE           TO SAW-TESTWEEK                                 
120100                                                                          
120200     IF SAW-TIRODAT = WS-TEST-DATUM                                       
120300        IF SAW-RADDAT = WS-TEST-DATUM                                     
120400           MOVE '*'             TO SAW-TESTWEEK                           
120500        END-IF                                                            
120600     END-IF                                                               
120700                                                                          
120800*                                                                         
120900*----------------------------------------------SLUT JM-87                 
121000     MOVE I13-KVRORAD-SDC-TOT TO SAW-KVRORAD                              
121100     ADD I13-KVRORAD-SDC-1-2-VECKA I13-KVRORAD-SDC-3-4-VECKA              
121200                             GIVING SAW-KVRORADVKA                        
121300     MOVE I13-SUROBEL-SDC TO W-ROTKR                                      
121400     PERFORM S12-AVRUNDA-SUROBEL                                          
121500     MOVE W-ROTKR TO SAW-ROTKR                                            
121600     MOVE SAW-RAD1 TO SAW-SKRIV-RAD                                       
121700     PERFORM S02-STYR-SIDA-SA                                             
121800     MOVE SPACE TO SAW-RAD1                                               
121900     .                                                                    
122000     EJECT                                                                
122100 EC-SKRIV-GEMENSAM-RAD SECTION.                                           
122200******************************************************************        
122300*    REDIGERA INFORMATION TILL DEN GEMMENSAMMA RADEN             *        
122400******************************************************************        
122500     SKIP2                                                                
122600     MOVE I13-BEART-SVE          TO SAW-BEART-SVE                         
122700     MOVE I13-KVAVIS-LEVBESK-1   TO SAW-KVAVIS-LEVBESK-1                  
122800     MOVE I13-TIAVIDAT-LEVBESK-1 TO SAW-TIAVIDAT-LEVBESK-1                
122900     SKIP2                                                                
123000     MOVE I13-KVAVIS-LEVBESK-2   TO SAW-KVAVIS-LEVBESK-2                  
123100     MOVE I13-TIAVIDAT-LEVBESK-2 TO SAW-TIAVIDAT-LEVBESK-2                
123200     SKIP2                                                                
123300     MOVE I13-KVAVIS-LEVBESK-3   TO SAW-KVAVIS-LEVBESK-3                  
123400     MOVE I13-TIAVIDAT-LEVBESK-3 TO SAW-TIAVIDAT-LEVBESK-3                
123500     SKIP2                                                                
123600     MOVE I13-KVBR           TO SAW-KVBR                                  
123700     MOVE I13-KDLTK          TO SAW-KDLTK                                 
123800     MOVE I13-KDHF           TO SAW-KDHF                                  
123900     MOVE SPACE TO SAW-ASTERISK                                           
124000                                                                          
124100     IF  I13-KDPROD > ZERO                                                
124200         MOVE '*'            TO SAW-ASTERISK                              
124300     END-IF                                                               
124400                                                                          
124500     MOVE I13-KDGK           TO SAW-KDGK                                  
124610     MOVE I13-KDPRODSL       TO SAW-KDPRODSL                              
124700     MOVE I13-TIFINLV        TO W-DATUM-VKA-N                             
124800     MOVE W-AA-VV            TO SAW-TIFINLV                               
124900     MOVE I13-KDVVKL         TO SAW-KDVVKL                                
125000                                                                          
125100     MOVE SAW-RAD2 TO SAW-SKRIV-RAD                                       
125200     PERFORM S02-STYR-SIDA-SA                                             
125300     .                                                                    
125400     EJECT                                                                
125500 ED-SKRIV-LEV-RUBR SECTION.                                               
125600******************************************************************        
125700*    SKRIVER LEVERANTÖRSRUBRIK                                   *        
125800******************************************************************        
125900     SKIP2                                                                
126000     IF  I13-KVROS-CDC-1-2 + I13-KVROS-CDC-3-4 +                          
126100         I13-KVROS-SDC-1-2 + I13-KVROS-SDC-3-4 > ZERO                     
126200         MOVE JA TO SW-SKRIV-TOT-LEVR                                     
126300         MOVE I13-IDLEVNR TO SAW-RUB-IDLEVNR                              
126400                             SAW-TOT-IDLEVNR                              
126500         MOVE 3 TO SAW-RADSTYR                                            
126600                                                                          
126700         IF  SAW-RADANT + SAW-RADSTYR + 5 > SAW-RADMAX                    
126800             MOVE SAW-RADMAX   TO SAW-RADANT                              
126900         END-IF                                                           
127000                                                                          
127100         MOVE SAW-LEV-RUB TO SAW-SKRIV-RAD                                
127200         PERFORM S02-STYR-SIDA-SA                                         
127300     END-IF                                                               
127400     .                                                                    
127500     EJECT                                                                
127600 F-SKRIV-TOT-LEV SECTION.                                                 
127700******************************************************************        
127800*    SKRIV UT TOTAL ÖVER LEVERANTÖR PER ANSKAFFARE               *        
127900******************************************************************        
128000     SKIP2                                                                
128100     IF  SW-SKRIV-TOT-LEVR = JA                                           
128200         MOVE NEJ TO SW-SKRIV-TOT-LEVR                                    
128300         MOVE W-LEV-ANTART       TO SAW-LEV-ANTART                        
128400         MOVE W-LEV-ROVAERDE     TO SAW-LEV-ROVAERDE                      
128500         MOVE W-LEV-KVRORAD      TO SAW-LEV-KVRORAD                       
128600         MOVE 2                  TO SAW-RADSTYR                           
128700         MOVE SAW-LEV-TOT TO SAW-SKRIV-RAD                                
128800                                                                          
128900         IF  SAW-RADSTYR + SAW-RADANT > SAW-RADMAX                        
129000             MOVE 1 TO SAW-RADSTYR                                        
129100             MOVE SAW-SKRIV-RAD TO SA-RAD                                 
129200             PERFORM S08-SKRIV-SA                                         
129300             MOVE SPACE TO SAW-SKRIV-RAD                                  
129400         ELSE                                                             
129500             PERFORM S02-STYR-SIDA-SA                                     
129600         END-IF                                                           
129700     END-IF                                                               
129800     .                                                                    
129900     EJECT                                                                
130000 G-SKRIV-TOT-ANSK SECTION.                                                
130100******************************************************************        
130200*    ADDERAR TOTALT C1+C2 OCH TOTALT ORDERKLASS                  *        
130300*    TÖMMER ANS-TAB (NIVÅ 1) OCH SKRIVER UT INFORMATIONEN        *        
130400*    PÅ ANSKAFFAR-LISAN (SA), GRUPP-LISTAN (SG)                  *        
130500******************************************************************        
130600     SKIP2                                                                
130700     MOVE 1 TO IXOKL                                                      
130800               IXNS                                                       
130900     PERFORM GA-ADD-TOTALT-CDC-SDC                                        
131000     MOVE 2 TO IXOKL                                                      
131100     PERFORM GA-ADD-TOTALT-CDC-SDC                                        
131200     PERFORM GB-ADD-TOTALT-ORDERKLASS                                     
131300                                                                          
131400     MOVE JA TO SW-SA-TOTAL                                               
131500     MOVE 1 TO IXNS                                                       
131600     MOVE 3 TO SGW-RADSTYR                                                
131700                SSW-RADSTYR                                               
131800     MOVE 1                  TO IXOKL                                     
131900     MOVE SAW-RADMAX         TO SAW-RADANT                                
132000                                                                          
132100     PERFORM UNTIL IXOKL > 3                                              
132200         PERFORM S07-BERAKNA-SERVPROC                                     
132300                                                                          
132400         EVALUATE IXOKL                                                   
132500         WHEN 1                                                           
132600                 MOVE 'ORDERKLASS    1-2' TO SAW-ORDERKLASS               
132700                 MOVE LID-IDANSK          TO SGW-IDANSK                   
132800                 MOVE '1-2'               TO SGW-KLASS                    
132900                                                                          
133000         WHEN 2                                                           
133100                 MOVE 'ORDERKLASS    3-4' TO SAW-ORDERKLASS               
133200                 MOVE '3-4'               TO SGW-KLASS                    
133300                                                                          
133400         WHEN 3                                                           
133500                 MOVE 'ORDERKLASS    1-4' TO SAW-ORDERKLASS               
133600                 MOVE 'T'                 TO SGW-KLASS                    
133700                                                                          
133800         END-EVALUATE                                                     
133900     SKIP3                                                                
134000*--------------------------------------- FLYTTA VÄRDEN SOM                
134100*                                        RÖR C1-LAGER                     
134200         MOVE '1'            TO SAW-LAGER                                 
134300         MOVE W-SERVPROC-CDC (IXOKL)     TO SAW-SERVPROC                  
134400                                            SGW-SERVPROC-CDC              
134500         MOVE ANS-KVRORAD-VKA-CDC (1, IXOKL) TO SAW-KVRORAD-VKA           
134600                                             SGW-KVRORAD-VKA-CDC          
134700         MOVE ANS-ANTART-CDC (1, IXOKL)  TO SAW-TOT-ANTART                
134800                                            SGW-ANTART-CDC                
134900         MOVE ANS-SUROBEL-CDC (1, IXOKL) TO SAW-TOT-ROVAERDE              
135000                                                                          
135100         MOVE ANS-SUROBEL-CDC (1, IXOKL) TO W-ROTKR                       
135200         PERFORM S12-AVRUNDA-SUROBEL                                      
135300                                                                          
135400         MOVE W-ROTKR TO SGW-ROTKR-CDC                                    
135500         MOVE ANS-KVINORD-CDC (1, IXOKL) TO SAW-KVINORD                   
135600                                            SGW-KVINORD-CDC               
135700         MOVE ANS-ANTEJLEVRO-CDC (1, IXOKL) TO SAW-ANTEJLEV               
135800                                              SGW-ANTEJLEV-CDC            
135900         MOVE 2 TO SAW-RADSTYR                                            
136000         MOVE SAW-TOT-RAD TO SAW-SKRIV-RAD                                
136100         PERFORM S02-STYR-SIDA-SA                                         
136200         MOVE SPACE TO SAW-ORDERKLASS                                     
136300     SKIP3                                                                
136400*--------------------------------------- FLYTTA VÄRDEN SOM                
136500*                                        RÖR C2-LAGER                     
136600         MOVE '2' TO SAW-LAGER                                            
136700         MOVE W-SERVPROC-SDC (IXOKL) TO SAW-SERVPROC                      
136800                                        SGW-SERVPROC-SDC                  
136900         MOVE ANS-KVRORAD-VKA-SDC (1, IXOKL) TO SAW-KVRORAD-VKA           
137000                                           SGW-KVRORAD-VKA-SDC            
137100         MOVE ANS-ANTART-SDC (1, IXOKL)  TO SAW-TOT-ANTART                
137200                                            SGW-ANTART-SDC                
137300         MOVE ANS-SUROBEL-SDC (1, IXOKL) TO SAW-TOT-ROVAERDE              
137400                                                                          
137500         MOVE ANS-SUROBEL-SDC (1, IXOKL) TO W-ROTKR                       
137600         PERFORM S12-AVRUNDA-SUROBEL                                      
137700                                                                          
137800         MOVE W-ROTKR TO SGW-ROTKR-SDC                                    
137900         MOVE ANS-KVINORD-SDC (1, IXOKL) TO SAW-KVINORD                   
138000                                            SGW-KVINORD-SDC               
138100         MOVE ANS-ANTEJLEVRO-SDC (1, IXOKL) TO SAW-ANTEJLEV               
138200                                              SGW-ANTEJLEV-SDC            
138300         MOVE SAW-TOT-RAD TO SAW-SKRIV-RAD                                
138400         PERFORM S02-STYR-SIDA-SA                                         
138500     SKIP3                                                                
138600*--------------------------------------- FLYTTA VÄRDEN SOM                
138700*                                        RÖR TOTAL-LAGER                  
138800         MOVE 'T' TO SAW-LAGER                                            
138900         MOVE W-SERVPROC-T (IXOKL)   TO SAW-SERVPROC                      
139000                                        SGW-SERVPROC-T                    
139100         MOVE ANS-KVRORAD-VKA-T (1, IXOKL) TO SAW-KVRORAD-VKA             
139200                                          SGW-KVRORAD-VKA-T               
139300         MOVE ANS-ANTART-T (1, IXOKL)  TO SAW-TOT-ANTART                  
139400                                        SGW-ANTART-T                      
139500         MOVE ANS-SUROBEL-T (1, IXOKL) TO SAW-TOT-ROVAERDE                
139600                                                                          
139700         MOVE ANS-SUROBEL-T (1, IXOKL) TO W-ROTKR                         
139800         PERFORM S12-AVRUNDA-SUROBEL                                      
139900                                                                          
140000         MOVE W-ROTKR TO SGW-ROTKR-T                                      
140100         MOVE ANS-KVINORD-T (1, IXOKL)   TO SAW-KVINORD                   
140200                                            SGW-KVINORD-T                 
140300         MOVE ANS-ANTEJLEVRO-T (1, IXOKL) TO SAW-ANTEJLEV                 
140400                                             SGW-ANTEJLEV-T               
140500                                                                          
140600         MOVE SAW-TOT-RAD TO SAW-SKRIV-RAD                                
140700         PERFORM S02-STYR-SIDA-SA                                         
140800*--------------------------------------- SKRIV RAD PÅ GRUPP-LISTA         
140900         PERFORM S03-STYR-SIDA-SG                                         
141000         MOVE SPACE TO SGW-RAD1                                           
141100         ADD 1 TO IXOKL                                                   
141200     END-PERFORM                                                          
141300     .                                                                    
141400     EJECT                                                                
141500 GA-ADD-TOTALT-CDC-SDC SECTION.                                           
141600*****************************************************************         
141700*    ADDERA C1-VÄRDEN OCH C2-VÄRDEN TILL TOTALT DÅ IXOKL=1,2    *         
141800*****************************************************************         
141900     SKIP2                                                                
142000     ADD ANS-KVAVBRAD-CDC (IXNS, IXOKL)                                   
142100          ANS-KVAVBRAD-SDC (IXNS, IXOKL) GIVING                           
142200          ANS-KVAVBRAD-T  (IXNS, IXOKL)                                   
142300     ADD ANS-KVFYSAVV-CDC (IXNS, IXOKL)                                   
142400          ANS-KVFYSAVV-SDC (IXNS, IXOKL) GIVING                           
142500          ANS-KVFYSAVV-T  (IXNS, IXOKL)                                   
142600     ADD ANS-KVINORD-CDC (IXNS, IXOKL)                                    
142700          ANS-KVINORD-SDC (IXNS, IXOKL) GIVING                            
142800          ANS-KVINORD-T  (IXNS, IXOKL)                                    
142900     ADD ANS-SUROBEL-CDC (IXNS, IXOKL)                                    
143000          ANS-SUROBEL-SDC (IXNS, IXOKL) GIVING                            
143100          ANS-SUROBEL-T  (IXNS, IXOKL)                                    
143200     ADD ANS-KVRORAD-VKA-CDC (IXNS, IXOKL)                                
143300          ANS-KVRORAD-VKA-SDC (IXNS, IXOKL) GIVING                        
143400          ANS-KVRORAD-VKA-T  (IXNS, IXOKL)                                
143500     .                                                                    
143600     EJECT                                                                
143700 GB-ADD-TOTALT-ORDERKLASS SECTION.                                        
143800******************************************************************        
143900*    ADDERA ORDERKLASS 1 OCH ORDERKLASS 3-4                      *        
144000******************************************************************        
144100     SKIP2                                                                
144200     ADD ANS-KVAVBRAD-CDC (IXNS, 1)                                       
144300          ANS-KVAVBRAD-CDC (IXNS, 2) GIVING                               
144400          ANS-KVAVBRAD-CDC (IXNS, 3)                                      
144500     ADD ANS-KVAVBRAD-SDC (IXNS, 1)                                       
144600          ANS-KVAVBRAD-SDC (IXNS, 2) GIVING                               
144700          ANS-KVAVBRAD-SDC (IXNS, 3)                                      
144800     ADD ANS-KVAVBRAD-T (IXNS, 1)                                         
144900          ANS-KVAVBRAD-T (IXNS, 2)  GIVING                                
145000          ANS-KVAVBRAD-T (IXNS, 3)                                        
145100     SKIP2                                                                
145200     ADD ANS-KVFYSAVV-CDC (IXNS, 1)                                       
145300          ANS-KVFYSAVV-CDC (IXNS, 2) GIVING                               
145400          ANS-KVFYSAVV-CDC (IXNS, 3)                                      
145500     ADD ANS-KVFYSAVV-SDC (IXNS, 1)                                       
145600          ANS-KVFYSAVV-SDC (IXNS, 2) GIVING                               
145700          ANS-KVFYSAVV-SDC (IXNS, 3)                                      
145800     ADD ANS-KVFYSAVV-T (IXNS, 1)                                         
145900          ANS-KVFYSAVV-T (IXNS, 2)  GIVING                                
146000          ANS-KVFYSAVV-T (IXNS, 3)                                        
146100     SKIP2                                                                
146200     ADD ANS-KVINORD-CDC (IXNS, 1)                                        
146300          ANS-KVINORD-CDC (IXNS, 2) GIVING                                
146400          ANS-KVINORD-CDC (IXNS, 3)                                       
146500     ADD ANS-KVINORD-SDC (IXNS, 1)                                        
146600          ANS-KVINORD-SDC (IXNS, 2) GIVING                                
146700          ANS-KVINORD-SDC (IXNS, 3)                                       
146800     ADD ANS-KVINORD-T (IXNS, 1)                                          
146900          ANS-KVINORD-T (IXNS, 2)  GIVING                                 
147000          ANS-KVINORD-T (IXNS, 3)                                         
147100     SKIP2                                                                
147200     ADD ANS-SUROBEL-CDC (IXNS, 1)                                        
147300          ANS-SUROBEL-CDC (IXNS, 2) GIVING                                
147400          ANS-SUROBEL-CDC (IXNS, 3)                                       
147500     ADD ANS-SUROBEL-SDC (IXNS, 1)                                        
147600          ANS-SUROBEL-SDC (IXNS, 2) GIVING                                
147700          ANS-SUROBEL-SDC (IXNS, 3)                                       
147800     ADD ANS-SUROBEL-T (IXNS, 1)                                          
147900          ANS-SUROBEL-T (IXNS, 2)  GIVING                                 
148000          ANS-SUROBEL-T (IXNS, 3)                                         
148100     SKIP2                                                                
148200     ADD ANS-KVRORAD-VKA-CDC (IXNS, 1)                                    
148300          ANS-KVRORAD-VKA-CDC (IXNS, 2) GIVING                            
148400          ANS-KVRORAD-VKA-CDC (IXNS, 3)                                   
148500     ADD ANS-KVRORAD-VKA-SDC (IXNS, 1)                                    
148600          ANS-KVRORAD-VKA-SDC (IXNS, 2) GIVING                            
148700          ANS-KVRORAD-VKA-SDC (IXNS, 3)                                   
148800     ADD ANS-KVRORAD-VKA-T (IXNS, 1)                                      
148900          ANS-KVRORAD-VKA-T (IXNS, 2)  GIVING                             
149000          ANS-KVRORAD-VKA-T (IXNS, 3)                                     
149100     SKIP2                                                                
149200     ADD ANS-ANTEJLEVRO-CDC (IXNS, 1)                                     
149300          ANS-ANTEJLEVRO-CDC (IXNS, 2) GIVING                             
149400          ANS-ANTEJLEVRO-CDC (IXNS, 3)                                    
149500     ADD ANS-ANTEJLEVRO-SDC (IXNS, 1)                                     
149600          ANS-ANTEJLEVRO-SDC (IXNS, 2) GIVING                             
149700          ANS-ANTEJLEVRO-SDC (IXNS, 3)                                    
149800     ADD ANS-ANTEJLEVRO-T (IXNS, 1)                                       
149900          ANS-ANTEJLEVRO-T (IXNS, 2)  GIVING                              
150000          ANS-ANTEJLEVRO-T (IXNS, 3)                                      
150100     .                                                                    
150200     EJECT                                                                
152400 J-SKRIV-TOT-GRUPP SECTION.                                               
152500******************************************************************        
152600*    REDIGERAR GRUPP-RAD (SGW-RAD)                               *        
152700*    SKRIVER RADEN PÅ GRUPP-LISTAN, SEKTIONS-LISTAN              *        
152800*    OCH TOTAL-LISTAN                                            *        
152900******************************************************************        
153000     SKIP2                                                                
153100     MOVE 3 TO SGW-RADSTYR                                                
153200                SSW-RADSTYR                                               
153300     MOVE 2 TO IXNS                                                       
153400     MOVE 2 TO STW-RADSTYR                                                
153500     MOVE 1 TO IXOKL                                                      
153600     SKIP2                                                                
153700     PERFORM UNTIL IXOKL > 3                                              
153800         PERFORM S07-BERAKNA-SERVPROC                                     
153900                                                                          
154000         EVALUATE IXOKL                                                   
154100         WHEN 1                                                           
154200                 MOVE W-GRP-FROM TO SGW-IDANSK                            
154300                 MOVE '1-2' TO SGW-KLASS                                  
154400                                                                          
154500         WHEN 2                                                           
154600                 MOVE '  -' TO SGW-IDANSK-X                               
154700                 MOVE '3-4' TO SGW-KLASS                                  
154800                                                                          
154900         WHEN 3                                                           
155000                 MOVE W-GRP-TOM TO SGW-IDANSK-X                           
155100                 MOVE 'T' TO SGW-KLASS                                    
155200                                                                          
155300         END-EVALUATE                                                     
155400     SKIP2                                                                
155500*--------------------------------------- FLYTTA VÄRDEN TILL               
155600*                                        SGW-RAD1                         
155700         PERFORM S06-REDIGERA-SGW-RAD                                     
155800     SKIP2                                                                
155900*--------------------------------------- SKRIV RAD PÅ GRUPP-LISTA,        
156000*                                        SEKTIONS-LISTA OCH               
156100*                                        TOTAL-LISTA                      
156200     PERFORM S03-STYR-SIDA-SG                                             
156300     PERFORM S04-STYR-SIDA-SS                                             
156500     MOVE SPACE TO SGW-RAD1                                               
156600                                                                          
156700     ADD 1 TO IXOKL                                                       
156800     END-PERFORM                                                          
156900     .                                                                    
157000     EJECT                                                                
157100 K-SKRIV-TOT-SEK SECTION.                                                 
157200******************************************************************        
157300*    REDIGERAR SEKTIONS-RAD (SGW-RAD)                            *        
157400*    SKRIV RADEN PÅ SEKTIONS-LISTAN                              *        
157500******************************************************************        
157600     SKIP2                                                                
157700     MOVE 4 TO SSW-RADSTYR                                                
157800     MOVE 1 TO IXOKL                                                      
157900     MOVE 3 TO IXNS                                                       
158000     SKIP2                                                                
158100     PERFORM UNTIL IXOKL > 3                                              
158200         PERFORM S07-BERAKNA-SERVPROC                                     
158300                                                                          
158400         EVALUATE IXOKL                                                   
158500         WHEN 1                                                           
158600                 MOVE W-SEK-FROM TO SGW-IDANSK-X                          
158700                 MOVE '1-2' TO SGW-KLASS                                  
158800                                                                          
158900         WHEN 2                                                           
159000                 MOVE '  -' TO SGW-IDANSK-X                               
159100                 MOVE '3-4' TO SGW-KLASS                                  
159200                                                                          
159300         WHEN 3                                                           
159400                 MOVE W-SEK-TOM TO SGW-IDANSK-X                           
159500                 MOVE 'T' TO SGW-KLASS                                    
159600                                                                          
159700         END-EVALUATE                                                     
159800     SKIP2                                                                
159900*--------------------------------------- FLYTTA VÄRDEN TILL               
160000*                                        SGW-RAD1                         
160100     PERFORM S06-REDIGERA-SGW-RAD                                         
160200     PERFORM S04-STYR-SIDA-SS                                             
160300     MOVE SPACE TO SGW-RAD1                                               
160400     ADD 1 TO IXOKL                                                       
160500     END-PERFORM                                                          
160600     .                                                                    
160700     EJECT                                                                
165000 M-SKRIV-TOM-LISTA SECTION.                                               
165100******************************************************************        
165200*    MARKERA ATT LISTAN ÄR TOM                                   *        
165300******************************************************************        
165400     SKIP2                                                                
165500     MOVE '*** TOM LISTA, INGA TRANSAKTIONER' TO SAW-SKRIV-RAD            
165600                                                 SGW-RAD1                 
165700     PERFORM S02-STYR-SIDA-SA                                             
165800     PERFORM S03-STYR-SIDA-SG                                             
165900     PERFORM S04-STYR-SIDA-SS                                             
166100     .                                                                    
166200     EJECT                                                                
166300 Z-AVSLUTNING SECTION.                                                    
166400******************************************************************        
166600*    STÄNG ALLA FILER                                            *        
166700*    SKRIV UT POSTSUMS RÄKNEVERK                                 *        
166800******************************************************************        
167800                                                                          
167900     CLOSE SA-LISTA                                                       
168000           SG-LISTA                                                       
168100           SS-LISTA                                                       
168300                                                                          
168400     MOVE 'S' TO POSTSUM-OPKOD                                            
168500     CALL POSTSUM USING POSTSUM-PARM                                      
168600     .                                                                    
168700     EJECT                                                                
168800 S01-LAS-TRANS SECTION.                                                   
168900******************************************************************        
169000*    LÄS W22513 OCH ADDERA UPP POSTRÄKNAREN                      *        
169100******************************************************************        
169200     SKIP2                                                                
169300     RETURN SRT-FIL INTO I13-AREA                                         
169400          AT END MOVE JA TO SW-W22513-EOF                                 
169500     END-RETURN                                                           
169600                                                                          
169700     IF SW-W22513-EOF = NEJ                                               
169800         CALL POSTSUM USING POSTSUM-PARM                                  
169900     END-IF                                                               
170000     .                                                                    
170100     EJECT                                                                
170200 S02-STYR-SIDA-SA SECTION.                                                
170300******************************************************************        
170400*    STYR SIDA FÖR LISTA SERVICEGRAD PER ANSKAFFARE              *        
170500******************************************************************        
170600     SKIP2                                                                
170700     IF SAW-RADANT + SAW-RADSTYR > SAW-RADMAX                             
170800*--------------------------------------- TRYCK RUBRIKER                   
170900*                                                                         
171000         ADD +1 TO SAW-SIDANT                                             
171100         MOVE SAW-SIDANT TO W-SIDNR                                       
171200         MOVE '001' TO W-LISTNR                                           
171300         MOVE 'PER ANSKAFFARE' TO W-TEXT                                  
171400         MOVE W-SPAR-IDANSK TO W-RUB-IDANSK                               
171500         MOVE W-RUB TO SA-RAD                                             
171600         MOVE ZERO TO SAW-RADSTYR                                         
171700         PERFORM S08-SKRIV-SA                                             
171800     SKIP2                                                                
171900         MOVE W-RUB1 TO SA-RAD                                            
172000         PERFORM S08-SKRIV-SA                                             
172100     SKIP2                                                                
172200*--------------------------------------- RUBRIKER FÖR TOTALSIDAN          
172300*                                                                         
172400         IF SW-SA-TOTAL = JA                                              
172500             MOVE SAW-TOT-RUB1 TO SA-RAD                                  
172600             MOVE 2 TO SAW-RADSTYR                                        
172700             PERFORM S08-SKRIV-SA                                         
172800     SKIP2                                                                
172900             MOVE SAW-TOT-RUB2 TO SA-RAD                                  
173000             PERFORM S08-SKRIV-SA                                         
173100             MOVE NEJ TO SW-SA-TOTAL                                      
173200         ELSE                                                             
173300*--------------------------------------- RUBRIKER FÖR VANLIG SIDA         
173400*                                                                         
173500             MOVE SAW-RUB2 TO SA-RAD                                      
173600             MOVE 2 TO SAW-RADSTYR                                        
173700             PERFORM S08-SKRIV-SA                                         
173800     SKIP2                                                                
173900             MOVE SAW-RUB3 TO SA-RAD                                      
174000             PERFORM S08-SKRIV-SA                                         
174100         END-IF                                                           
174200         MOVE 2 TO SAW-RADSTYR                                            
174300     END-IF                                                               
174400     SKIP2                                                                
174500*--------------------------------------- TRYCK DETALJRAD                  
174600*                                                                         
174700         MOVE SAW-SKRIV-RAD TO SA-RAD                                     
174800         PERFORM S08-SKRIV-SA                                             
174900         MOVE SPACE TO SAW-SKRIV-RAD                                      
175000     .                                                                    
175100     EJECT                                                                
175200 S03-STYR-SIDA-SG SECTION.                                                
175300******************************************************************        
175400*    STYR SIDA FÖR LISTA SERVICEGRAD PER GRUPP                   *        
175500******************************************************************        
175600     SKIP2                                                                
175700     IF SGW-RADANT + SGW-RADSTYR > SGW-RADMAX                             
175800*--------------------------------------- TRYCK RUBRIKER                   
175900*                                                                         
176000         ADD +1 TO SGW-SIDANT                                             
176100         MOVE SGW-SIDANT TO W-SIDNR                                       
176200         MOVE '002' TO W-LISTNR                                           
176300         MOVE 'PER GRUPP' TO W-TEXT                                       
176400         MOVE W-GRP-FROM  TO W-IDANSK-FROM                                
176500         MOVE '-'         TO W-STRECK                                     
176600         MOVE W-GRP-TOM   TO W-IDANSK-TOM                                 
176700         MOVE W-RUB TO SG-RAD                                             
176800         MOVE ZERO TO SGW-RADSTYR                                         
176900         PERFORM S09-SKRIV-SG                                             
177000     SKIP2                                                                
177100         MOVE W-RUB1 TO SG-RAD                                            
177200         PERFORM S09-SKRIV-SG                                             
177300     SKIP2                                                                
177400         MOVE SGW-RUB1 TO SG-RAD                                          
177500         PERFORM S09-SKRIV-SG                                             
177600     SKIP2                                                                
177700         MOVE SGW-RUB2 TO SG-RAD                                          
177800         PERFORM S09-SKRIV-SG                                             
177900     SKIP2                                                                
178000         MOVE SGW-RUB3 TO SG-RAD                                          
178100         PERFORM S09-SKRIV-SG                                             
178200         MOVE 2 TO SGW-RADSTYR                                            
178300     END-IF                                                               
178400     SKIP2                                                                
178500*--------------------------------------- TRYCK DETALJRAD                  
178600*                                                                         
178700     MOVE SGW-RAD1 TO SG-RAD                                              
178800     PERFORM S09-SKRIV-SG                                                 
178900     .                                                                    
179000     EJECT                                                                
179100 S04-STYR-SIDA-SS SECTION.                                                
179200******************************************************************        
179300*    STYR SIDA FÖR LISTA SERVICEGRAD PER SEKTION                 *        
179400******************************************************************        
179500     SKIP2                                                                
179600     IF SSW-RADANT + SSW-RADSTYR > SSW-RADMAX                             
179700*--------------------------------------- TRYCK RUBRIKER                   
179800*                                                                         
179900         ADD +1 TO SSW-SIDANT                                             
180000         MOVE SSW-SIDANT TO W-SIDNR                                       
180100         MOVE '003' TO W-LISTNR                                           
180200         MOVE 'PER SEKTION' TO W-TEXT                                     
180300         MOVE W-FUNK-FROM   TO W-IDANSK-FROM                              
180400         MOVE '-'           TO W-STRECK                                   
180500         MOVE W-FUNK-TOM    TO W-IDANSK-TOM                               
180600         MOVE W-RUB TO SS-RAD                                             
180700         MOVE ZERO TO SSW-RADSTYR                                         
180800         PERFORM S10-SKRIV-SS                                             
180900     SKIP2                                                                
181000         MOVE W-RUB1 TO SS-RAD                                            
181100         PERFORM S10-SKRIV-SS                                             
181200                                                                          
181300         MOVE SGW-RUB1 TO SS-RAD                                          
181400         PERFORM S10-SKRIV-SS                                             
181500     SKIP2                                                                
181600         MOVE SGW-RUB2 TO SS-RAD                                          
181700         PERFORM S10-SKRIV-SS                                             
181800     SKIP2                                                                
181900         MOVE SGW-RUB3 TO SS-RAD                                          
182000         PERFORM S10-SKRIV-SS                                             
182100         MOVE 2 TO SSW-RADSTYR                                            
182200     END-IF                                                               
182300     SKIP2                                                                
182400*--------------------------------------- TRYCK DETALJRAD                  
182500*                                                                         
182600     MOVE SGW-RAD1 TO SS-RAD                                              
182700     PERFORM S10-SKRIV-SS                                                 
182800     .                                                                    
182900     EJECT                                                                
186600 S06-REDIGERA-SGW-RAD SECTION.                                            
186700******************************************************************        
186800*    FLYTTA VÄRDEN FRÅN ANS-TAB (IXNS) TILL SGW-RAD              *        
186900******************************************************************        
187000     SKIP2                                                                
187100*--------------------------------------- C1-INFORMATION                   
187200*                                                                         
187300         MOVE W-SERVPROC-CDC (IXOKL) TO SGW-SERVPROC-CDC                  
187400         MOVE ANS-KVRORAD-VKA-CDC (IXNS, IXOKL)                           
187500                                  TO SGW-KVRORAD-VKA-CDC                  
187600         MOVE ANS-ANTART-CDC (IXNS, IXOKL) TO SGW-ANTART-CDC              
187700         MOVE ANS-KVINORD-CDC (IXNS, IXOKL) TO SGW-KVINORD-CDC            
187800         MOVE ANS-ANTEJLEVRO-CDC (IXNS, IXOKL) TO SGW-ANTEJLEV-CDC        
187900         MOVE ANS-SUROBEL-CDC (IXNS, IXOKL) TO W-ROTKR                    
188000         PERFORM S12-AVRUNDA-SUROBEL                                      
188100                                                                          
188200         MOVE W-ROTKR TO SGW-ROTKR-CDC                                    
188300     SKIP2                                                                
188400*--------------------------------------- C2-INFORMATION                   
188500*                                                                         
188600         MOVE W-SERVPROC-SDC (IXOKL) TO SGW-SERVPROC-SDC                  
188700         MOVE ANS-KVRORAD-VKA-SDC (IXNS, IXOKL)                           
188800                                 TO SGW-KVRORAD-VKA-SDC                   
188900         MOVE ANS-ANTART-SDC (IXNS, IXOKL) TO SGW-ANTART-SDC              
189000         MOVE ANS-KVINORD-SDC (IXNS, IXOKL) TO SGW-KVINORD-SDC            
189100         MOVE ANS-ANTEJLEVRO-SDC (IXNS, IXOKL) TO SGW-ANTEJLEV-SDC        
189200                                                                          
189300         MOVE ANS-SUROBEL-SDC (IXNS, IXOKL) TO W-ROTKR                    
189400         PERFORM S12-AVRUNDA-SUROBEL                                      
189500         MOVE W-ROTKR TO SGW-ROTKR-SDC                                    
189600     SKIP2                                                                
189700*--------------------------------------- TOTALT C1, C2-INFORMATION        
189800*                                                                         
189900         MOVE W-SERVPROC-T (IXOKL) TO SGW-SERVPROC-T                      
190000         MOVE ANS-KVRORAD-VKA-T (IXNS, IXOKL)                             
190100                               TO SGW-KVRORAD-VKA-T                       
190200         MOVE ANS-ANTART-T (IXNS, IXOKL) TO SGW-ANTART-T                  
190300         MOVE ANS-KVINORD-T (IXNS, IXOKL) TO SGW-KVINORD-T                
190400         MOVE ANS-ANTEJLEVRO-T (IXNS, IXOKL) TO SGW-ANTEJLEV-T            
190500                                                                          
190600         MOVE ANS-SUROBEL-T (IXNS, IXOKL) TO W-ROTKR                      
190700         PERFORM S12-AVRUNDA-SUROBEL                                      
190800         MOVE W-ROTKR TO SGW-ROTKR-T                                      
190900     .                                                                    
191000     EJECT                                                                
191100 S07-BERAKNA-SERVPROC SECTION.                                            
191200******************************************************************        
191300*    BERÄKNAR SERVICEGRADEN                                      *        
191400*    PER C-LAGER OCH PER ORDERKLASS (BEROENDE PÅ IXOKL)          *        
191500******************************************************************        
191600     SKIP2                                                                
191700*--------------------------------------- LAGER C1                         
191800     IF ANS-KVINORD-CDC (IXNS, IXOKL) = ZERO                              
191900         MOVE ZERO TO W-SERVPROC-CDC (IXOKL)                              
192000     ELSE                                                                 
192100         COMPUTE W-SERVPROC-CDC (IXOKL) ROUNDED =                         
192200             ANS-KVAVBRAD-CDC (IXNS, IXOKL) * 100 /                       
192300             ANS-KVINORD-CDC (IXNS, IXOKL)                                
192400     END-IF                                                               
192500     SKIP2                                                                
192600*--------------------------------------- LAGER C2                         
192700     IF ANS-KVINORD-SDC (IXNS, IXOKL) = ZERO                              
192800         MOVE ZERO TO W-SERVPROC-SDC (IXOKL)                              
192900     ELSE                                                                 
193000         COMPUTE W-SERVPROC-SDC (IXOKL) ROUNDED =                         
193100            ANS-KVAVBRAD-SDC (IXNS, IXOKL) * 100 /                        
193200            ANS-KVINORD-SDC (IXNS, IXOKL)                                 
193300     END-IF                                                               
193400     SKIP2                                                                
193500*--------------------------------------- LAGER TOTALT                     
193600     IF ANS-KVINORD-T (IXNS, IXOKL) = ZERO                                
193700         MOVE ZERO TO W-SERVPROC-T (IXOKL)                                
193800     ELSE                                                                 
193900         COMPUTE W-SERVPROC-T (IXOKL) ROUNDED =                           
194000            ANS-KVAVBRAD-T (IXNS, IXOKL) * 100 /                          
194100            ANS-KVINORD-T (IXNS, IXOKL)                                   
194200     END-IF                                                               
194300     .                                                                    
194400     EJECT                                                                
194500 S08-SKRIV-SA SECTION.                                                    
194600******************************************************************        
194700*    SKRIV-RAD PÅ LISTA SERVICEGRAD PER ANSKAFFARE               *        
194800******************************************************************        
194900     SKIP2                                                                
195000     IF SAW-RADSTYR = ZERO                                                
195100         WRITE SA-POST AFTER PAGE                                         
195200         MOVE ZERO TO SAW-RADANT                                          
195300     ELSE                                                                 
195400         WRITE SA-POST AFTER SAW-RADSTYR                                  
195500         ADD SAW-RADSTYR TO SAW-RADANT                                    
195600     END-IF                                                               
195700                                                                          
195800     MOVE 1 TO SAW-RADSTYR                                                
195900     .                                                                    
196000     EJECT                                                                
196100 S09-SKRIV-SG SECTION.                                                    
196200******************************************************************        
196300*    SKRIV RAD PÅ LISTA SERVICEGRAD PER GRUPP                    *        
196400******************************************************************        
196500     SKIP2                                                                
196600     IF SGW-RADSTYR = ZERO                                                
196700         WRITE SG-POST AFTER PAGE                                         
196800         MOVE ZERO TO SGW-RADANT                                          
196900     ELSE                                                                 
197000         WRITE SG-POST AFTER SGW-RADSTYR                                  
197100         ADD SGW-RADSTYR TO SGW-RADANT                                    
197200     END-IF                                                               
197300                                                                          
197400     MOVE 1 TO SGW-RADSTYR                                                
197500     .                                                                    
197600     EJECT                                                                
197700 S10-SKRIV-SS SECTION.                                                    
197800******************************************************************        
197900*    SKRIV RAD PÅ LISTA SERVICEGRAD PER SEKTION                  *        
198000******************************************************************        
198100     SKIP2                                                                
198200     IF SSW-RADSTYR = ZERO                                                
198300         WRITE SS-POST AFTER PAGE                                         
198400         MOVE ZERO TO SSW-RADANT                                          
198500     ELSE                                                                 
198600         WRITE SS-POST AFTER SSW-RADSTYR                                  
198700         ADD SSW-RADSTYR TO SSW-RADANT                                    
198800     END-IF                                                               
198900                                                                          
199000     MOVE 1 TO SSW-RADSTYR                                                
199100     .                                                                    
199200     EJECT                                                                
200900 S12-AVRUNDA-SUROBEL SECTION.                                             
201000*****************************************************************         
201100*    AVRUNDA SUROBEL TILL TUSENTALS KRONOR.                     *         
201200*    VÄRDE MELLAN 0 OCH 1449.99 REDOVISAS SOM 1.                *         
201300*****************************************************************         
201400     SKIP2                                                                
201500     IF W-ROTKR > ZERO                                                    
201600     COMPUTE W-ROTKR ROUNDED = W-ROTKR / 1000                             
201700                                                                          
201800     IF  W-ROTKR = ZERO                                                   
201900         MOVE 1 TO W-ROTKR                                                
202000     END-IF                                                               
202100     END-IF                                                               
202200     .                                                                    
202300     EJECT                                                                
202400 S13-ADDERA-NASTA-NIVA SECTION.                                           
202500******************************************************************        
202600*    ADDERA TILL NÄSTA NIVÅ                                      *        
202700******************************************************************        
202800     SKIP2                                                                
202900     MOVE 1 TO IXOKL                                                      
203000     SKIP2                                                                
203100     PERFORM UNTIL IXOKL > 3                                              
203200         ADD ANS-KVAVBRAD-CDC (IXNS, IXOKL)                               
203300                             TO ANS-KVAVBRAD-CDC (IXNS2, IXOKL)           
203400         ADD ANS-KVFYSAVV-CDC (IXNS, IXOKL)                               
203500                             TO ANS-KVFYSAVV-CDC (IXNS2, IXOKL)           
203600         ADD ANS-KVINORD-CDC (IXNS, IXOKL)                                
203700                             TO ANS-KVINORD-CDC (IXNS2, IXOKL)            
203800         ADD ANS-KVRORAD-VKA-CDC (IXNS, IXOKL)                            
203900                             TO ANS-KVRORAD-VKA-CDC (IXNS2, IXOKL)        
204000         ADD ANS-ANTART-CDC (IXNS, IXOKL)                                 
204100                             TO ANS-ANTART-CDC (IXNS2, IXOKL)             
204200         ADD ANS-SUROBEL-CDC (IXNS, IXOKL)                                
204300                             TO ANS-SUROBEL-CDC (IXNS2, IXOKL)            
204400         ADD ANS-ANTEJLEVRO-CDC (IXNS, IXOKL)                             
204500                             TO ANS-ANTEJLEVRO-CDC (IXNS2, IXOKL)         
204600         ADD ANS-KVAVBRAD-SDC (IXNS, IXOKL)                               
204700                             TO ANS-KVAVBRAD-SDC (IXNS2, IXOKL)           
204800         ADD ANS-KVFYSAVV-SDC (IXNS, IXOKL)                               
204900                             TO ANS-KVFYSAVV-SDC (IXNS2, IXOKL)           
205000         ADD ANS-KVINORD-SDC (IXNS, IXOKL)                                
205100                             TO ANS-KVINORD-SDC (IXNS2, IXOKL)            
205200         ADD ANS-KVRORAD-VKA-SDC (IXNS, IXOKL)                            
205300                             TO ANS-KVRORAD-VKA-SDC (IXNS2, IXOKL)        
205400         ADD ANS-ANTART-SDC (IXNS, IXOKL)                                 
205500                             TO ANS-ANTART-SDC (IXNS2, IXOKL)             
205600         ADD ANS-SUROBEL-SDC (IXNS, IXOKL)                                
205700                             TO ANS-SUROBEL-SDC (IXNS2, IXOKL)            
205800         ADD ANS-ANTEJLEVRO-SDC (IXNS, IXOKL)                             
205900                             TO ANS-ANTEJLEVRO-SDC (IXNS2, IXOKL)         
206000         ADD ANS-KVAVBRAD-T (IXNS, IXOKL)                                 
206100                             TO ANS-KVAVBRAD-T (IXNS2, IXOKL)             
206200         ADD ANS-KVFYSAVV-T (IXNS, IXOKL)                                 
206300                             TO ANS-KVFYSAVV-T (IXNS2, IXOKL)             
206400         ADD ANS-KVINORD-T (IXNS, IXOKL)                                  
206500                             TO ANS-KVINORD-T (IXNS2, IXOKL)              
206600         ADD ANS-KVRORAD-VKA-T (IXNS, IXOKL)                              
206700                             TO ANS-KVRORAD-VKA-T (IXNS2, IXOKL)          
206800         ADD ANS-ANTART-T (IXNS, IXOKL)                                   
206900                             TO ANS-ANTART-T (IXNS2, IXOKL)               
207000         ADD ANS-SUROBEL-T (IXNS, IXOKL)                                  
207100                             TO ANS-SUROBEL-T (IXNS2, IXOKL)              
207200         ADD ANS-ANTEJLEVRO-T (IXNS, IXOKL)                               
207300                             TO ANS-ANTEJLEVRO-T (IXNS2, IXOKL)           
207400         ADD 1 TO IXOKL                                                   
207500     END-PERFORM                                                          
207600     .                                                                    
