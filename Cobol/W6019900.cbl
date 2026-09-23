000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6019900.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/09/07.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BAKGRUNDS MPP SOM SKRIVER UT INLÄGGNINGSLISTA.                   
001100*        STARTAS AV W60141.                                               
001200*                                                                         
001300*        PROGRAMMET          UPPDATERAR WLLISB (WDG8)                     
001400*        PROGRAMMET          LÄSER      W6INLA (W6D1)                     
001500*                                       WLARTD (WDD8)                     
001600*    SUB PROGRAMMET W611STYR LÄSER      W6INLA (W6G1)                     
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W6T199                                              
002000*        MID:         W6I19901                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        INLÄGGNINGSLISTA                                                 
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6019900'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004100     88  NYCKLAR-OK                          VALUE 'J'.                   
004200     88  NYCKLAR-FEL                         VALUE 'N'.                   
004300                                                                          
004400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004500     88  EGEN-MID                            VALUE '6199'.                
004600                                                                          
004700 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
004800 77  MAX-TAB-IX                  PIC S9(3)   VALUE +3 COMP-3.             
004900 77  MAX-KVRADER                 PIC S9(3)   VALUE +40 COMP-3.            
005000 77  W-SPAR-IDARTNR              PIC S9(9)   VALUE ZERO COMP-3.           
005100 77  W-KVRADER                   PIC S9(7)   VALUE ZERO COMP-3.           
005200 77  W-KVINLART-VOR              PIC S9(7)   VALUE ZERO COMP-3.           
005300 77  W-ADLAGOMR                  PIC X(4)    VALUE SPACE.                 
005400 77  W-ADBUFFOMR-ALFA            PIC X(4)    VALUE SPACE.                 
005500 77  W-IDSIDNR                   PIC S9(3)   VALUE ZERO COMP-3.           
005600 77  W-SPAR-IDILIST              PIC 9(5)    VALUE ZERO.                  
005700                                                                          
005800 01  WS-TIHHMMSSTH.                                                       
005900     03 WS-TIHH                  PIC 9(2)    VALUE ZERO.                  
006000     03 WS-TIMM                  PIC 9(2)    VALUE ZERO.                  
006100     03 FILLER                   PIC X(4)    VALUE ZERO.                  
006200                                                                          
006300*    --- TABELL FÖR BUFFERTPLATS                                          
006400 01  FILLER.                                                              
006500  03 W-BUFFERTPL-TAB     OCCURS 3 INDEXED BY TAB-IX.                      
006600     05  W-ADBUFFOMR             PIC S9(3) COMP-3 VALUE ZERO.             
006700     05  W-ADBUFFGANG            PIC S9(3) COMP-3 VALUE ZERO.             
006800     05  W-ADBUFFPL              PIC S9(5) COMP-3 VALUE ZERO.             
006900     SKIP2                                                                
007000     EJECT                                                                
007100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007200 01  GENERELLA-SUBPROGRAM.                                                
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
007600     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
007700     SKIP3                                                                
007800* VARIABLER TILL SUBPROGRAM W006PRR1                                      
007900*01  -COPY W006PRAR                                                       
008000     SKIP2                                                                
008100     EJECT                                                                
008200* VARIABLER TILL SUBPROGRAM W611STYR                                      
008300*01  -COPY W611STYR                                                       
008400     EJECT                                                                
008500 01  WS-RAPP-AREA.                                                        
008600     03  WS-RAPP-LISTID.                                                  
008700         05  FILLER              PIC X(5)    VALUE 'ILIST'.               
008800         05  WS-RAPP-IDILIST     PIC 9(5)    VALUE ZERO.                  
008900     03  WS-RAPP-LISTRAD.                                                 
009000         05  FILLER              PIC X(2)    VALUE SPACE.                 
009100         05  WS-RAPP-RAD         PIC X(130).                              
009200     03  WS-DUMMY                PIC X(1).                                
009300     03  WS-RAPP-PRINTER         PIC X(8).                                
009400     EJECT                                                                
009500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009800     SKIP3                                                                
009900*01  MID -COPY W6I19901                                                   
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010200     SKIP3                                                                
010300*01  -COPY WMSGAREA                                                       
010400     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  NYCKLAR-TILL-DLI.                                                    
011100     03  W-W6D1D1KY-MIN-X.                                                
011200        05  W-D1D1KY-IDILIST-MIN     PIC  9(5) VALUE ZERO.                
011300        05  W-D1D1KY-IDILIRAD-MIN    PIC S9(5) COMP-3 VALUE ZERO.         
011400        05  W-D1D1KY-IDRADNR-INL-MIN PIC S9(5) COMP-3 VALUE ZERO.         
011500        05  W-D1D1KY-IDDC-MIN        PIC X(2)  VALUE SPACE.               
011600        05  W-D1D1KY-IDLEVNR-MIN     PIC  X(5) VALUE SPACE.               
011700        05  W-D1D1KY-IDFS-MIN        PIC  X(8) VALUE SPACE.               
011800        05  W-D1D1KY-TIAVIDAT-MIN    PIC S9(7) COMP-3 VALUE ZERO.         
011900        05  W-D1D1KY-IDRADNR-MIN     PIC S9(5) COMP-3 VALUE ZERO.         
012000                                                                          
012100     03  W-W6D1D1KY-MAX-X.                                                
012200        05  W-D1D1KY-IDILIST-MAX     PIC  9(5) VALUE ZERO.                
012300        05  W-D1D1KY-IDILIRAD-MAX    PIC S9(5) COMP-3 VALUE 99999.        
012400        05  W-D1D1KY-IDRADNR-INL-MAX PIC S9(5) COMP-3 VALUE ZERO.         
012500        05  W-D1D1KY-IDDC-MAX        PIC X(2)  VALUE SPACE.               
012600        05  W-D1D1KY-IDLEVNR-MAX     PIC  X(5) VALUE SPACE.               
012700        05  W-D1D1KY-IDFS-MAX        PIC  X(8) VALUE SPACE.               
012800        05  W-D1D1KY-TIAVIDAT-MAX    PIC S9(7) COMP-3 VALUE ZERO.         
012900        05  W-D1D1KY-IDRADNR-MAX     PIC S9(5) COMP-3 VALUE ZERO.         
013000                                                                          
013100     03  W-W6D101KY-X.                                                    
013200         05  W-D101KY-IDDC       PIC X(2)     VALUE SPACE.                
013300         05  W-D101KY-IDLEVNR    PIC X(5)     VALUE SPACE.                
013400         05  W-D101KY-IDFS       PIC X(8)     VALUE SPACE.                
013500         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
013600                                                                          
013700     03  W-IDRADNR-INL-X.                                                 
013800        05  W-IDRADNR-INL            PIC S9(5) COMP-3 VALUE ZERO.         
013900                                                                          
014000     03  W-IDRADNR-X.                                                     
014100        05  W-IDRADNR                PIC S9(5) COMP-3 VALUE ZERO.         
014200                                                                          
014300     03  W-IDARTNR-X.                                                     
014400        05  W-IDARTNR                PIC S9(9) COMP-3 VALUE ZERO.         
014500                                                                          
014600     03  W-IDDC-X.                                                        
014700        05  W-IDDC                   PIC  X(2)        VALUE '11'.         
014800                                                                          
014900     03  W-KDSEGKEY-X.                                                    
015000        05  W-KDSEGKEY               PIC 9(1)        VALUE 1.             
015100                                                                          
015200     03  W-W6D1CSEQ-X.                                                    
015300        05  W-D1CSEQ-IDLEVNR-KOLLI   PIC  X(5) VALUE SPACE.               
015400        05  W-D1CSEQ-IDOKOLLI        PIC  9(9) VALUE ZERO.                
015500                                                                          
015600     03  W-IDLEVNR-KOLLI-X.                                               
015700        05  W-IDLEVNR-KOLLI          PIC  X(5) VALUE SPACE.               
015800                                                                          
015900     03  W-IDOKOLLI-X.                                                    
016000        05  W-IDOKOLLI               PIC  9(9) VALUE ZERO.                
016100                                                                          
016200     SKIP2                                                                
016300*    --- STATUS-KOD FRÅN IMS                                              
016400 01  STATUS-WS                   PIC XX.                                  
016500     88  SEGMENT-FINNS                       VALUE '  '.                  
016600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016900     SKIP2                                                                
017000 01  GODK-STATUSKODER.                                                    
017100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017200     SKIP3                                                                
017300 01  SSA1                        PIC X(128).                              
017400 01  SSA2                        PIC X(64).                               
017500 01  SSA3                        PIC X(64).                               
017600     EJECT                                                                
017700*    --- IMS FUNKTIONSKODER                                               
017800*01  -COPY W0003                                                          
017900     EJECT                                                                
018000*    ---  DLI INPUT-OUTPUT AREA                                           
018100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018200     SKIP3                                                                
018300 01  DLI-IO-AREA1.                                                        
018400     03  IO-AREA1                PIC X(900)  VALUE SPACE.                 
018500     SKIP3                                                                
018600     03  W6INLE01 REDEFINES IO-AREA1.                                     
018700*        05  -COPY W6D1D1                                                 
018800     EJECT                                                                
018900     03  W6INLA01 REDEFINES IO-AREA1.                                     
019000*        05  -COPY W6D101                                                 
019100     EJECT                                                                
019200     03  WLARTD11 REDEFINES IO-AREA1.                                     
019300*        05  -COPY WDD811                                                 
019400     EJECT                                                                
019500     03  WLARTC11 REDEFINES IO-AREA1.                                     
019600*        05  -COPY WDK611 -PRE ARTC-                                      
019700     EJECT                                                                
019800 01  DLI-IO-AREA2.                                                        
019900     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
020000     SKIP3                                                                
020100     03  W6INLA11 REDEFINES IO-AREA2.                                     
020200*        05  -COPY W6D111                                                 
020300     EJECT                                                                
020400 01  DLI-IO-AREA3.                                                        
020500     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
020600     SKIP3                                                                
020700     03  W6INLA11 REDEFINES IO-AREA3.                                     
020800*        05  -COPY W6D111     -PRE DIV-                                   
020900     EJECT                                                                
021000     03  W6INLA21 REDEFINES IO-AREA3.                                     
021100*        05  -COPY W6D121                                                 
021200     EJECT                                                                
021300*  PRINTRADER FÖR INLÄGGNINGSLISTA RAPPORT                                
021400                                                                          
021500 01  LIST-HRAD1.                                                          
021600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
021700     03   FILLER                  PIC X(22) VALUE                         
021800                                        'VOLVO CARS            '.         
021900     03   FILLER                  PIC X(19) VALUE SPACE.                  
022000     03   FILLER                  PIC X(6)  VALUE 'W60199'.               
022100     03   FILLER                  PIC X(11) VALUE SPACE.                  
022200     03   FILLER                  PIC X(19) VALUE                         
022300                                        'INLÄGGNINGSLISTA NR'.            
022400     03   FILLER                  PIC X(3)  VALUE SPACE.                  
022500     03   HRAD1-IDILIST           PIC Z(4)9 VALUE ZERO.                   
022600     03   FILLER                  PIC X(12) VALUE SPACE.                  
022700     03   HRAD1-DATUM             PIC X(6)  VALUE SPACE.                  
022800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
022900     03   HRAD1-TID-TT            PIC Z(2)  VALUE ZERO.                   
023000     03   FILLER                  PIC X(1)  VALUE ':'.                    
023100     03   HRAD1-TID-MM            PIC 9(2)  VALUE ZERO.                   
023200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
023300     03   FILLER                  PIC X(4)  VALUE 'SID '.                 
023400     03   HRAD1-IDSIDNR           PIC Z(2)9 VALUE ZERO.                   
023500                                                                          
023600 01  LIST-HRAD2.                                                          
023700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
023800     03   FILLER                  PIC X(7)  VALUE 'LEVNR  '.              
023900     03   HRAD2-IDLEVNR-KOLLI     PIC X(5)  VALUE ZERO.                   
024000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
024100     03   FILLER                  PIC X(8)  VALUE 'KOLLINR '.             
024200     03   HRAD2-IDOKOLLI          PIC Z(9)  VALUE ZERO.                   
024300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
024400     03   FILLER                  PIC X(5)  VALUE 'VAGN '.                
024500     03   HRAD2-IDINLVGN          PIC Z(3)  VALUE ZERO.                   
024600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
024700     03   FILLER                  PIC X(5)  VALUE 'PLAC '.                
024800     03   HRAD2-ADINLOMR          PIC X(4)  VALUE SPACE.                  
024900     03   FILLER                  PIC X(8)  VALUE SPACE.                  
025000     03   FILLER                  PIC X(11) VALUE 'LISTA FÖR: '.          
025100     03   HRAD2-ADLAGOMR          PIC X(4)  VALUE SPACE.                  
025200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
025300     03   FILLER                  PIC X(5)  VALUE 'TORG '.                
025400     03   HRAD2-ADINLOMR-TORG     PIC X(4)  VALUE SPACE.                  
025500     03   FILLER                  PIC X(36) VALUE SPACE.                  
025600                                                                          
025700 01  LIST-HRUB3.                                                          
025800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
025900     03   FILLER                  PIC X(4)  VALUE 'RAD '.                 
026000     03   FILLER                  PIC X(10) VALUE 'LAGERPLATS'.           
026100     03   FILLER                  PIC X(5)  VALUE SPACE.                  
026200     03   FILLER                  PIC X(6)  VALUE 'ARTNR '.               
026300     03   FILLER                  PIC X(10) VALUE 'BENÄMNING '.           
026400     03   FILLER                  PIC X(08) VALUE SPACE.                  
026500     03   FILLER                  PIC X(5)  VALUE 'ANTAL'.                
026600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
026700     03   FILLER                  PIC X(12) VALUE 'BUFFERTPLATS'.         
026800     03   FILLER                  PIC X(5)  VALUE SPACE.                  
026900     03   FILLER                  PIC X(4)  VALUE 'VOR '.                 
027000     03   FILLER                  PIC X(5)  VALUE 'PRIO '.                
027100     03   FILLER                  PIC X(2)  VALUE 'FB'.                   
027200     03   FILLER                  PIC X(3)  VALUE SPACE.                  
027300     03   FILLER                  PIC X(9)  VALUE '  ERSKOD '.            
027400     03   FILLER                  PIC X(3)  VALUE 'FT '.                  
027500     03   FILLER                  PIC X(6)  VALUE 'LEVNR '.               
027600     03   FILLER                  PIC X(12) VALUE 'PARTI/KOLLI '.         
027700     03   FILLER                  PIC X(10) VALUE 'NOTERINGAR'.           
027800                                                                          
027900 01  LIST-LRAD.                                                           
028000     03   FILLER                  PIC X(1).                               
028100     03   LRAD-IDILIRAD           PIC Z(2)9.                              
028200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
028300     03   LRAD-ADLAGOMR           PIC Z(1)9.                              
028400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
028500     03   LRAD-ADGANG             PIC Z(1)9.                              
028600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
028700     03   LRAD-ADPLATS            PIC Z(4)9.                              
028800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
028900     03   LRAD-IDARTNR            PIC Z(7)9.                              
029000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
029100     03   LRAD-BEART              PIC X(16).                              
029200     03   FILLER                  PIC X(1)  VALUE SPACE.                  
029300     03   LRAD-KVINLART           PIC Z(5)9.                              
029400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
029500     03   LRAD-ADBUFFOMR          PIC Z(1)9.                              
029600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
029700     03   LRAD-ADBUFFGANG         PIC Z(1)9.                              
029800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
029900     03   LRAD-ADBUFFPL           PIC Z(4)9.                              
030000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
030100     03   LRAD-KVINLART-VOR       PIC Z(6).                               
030200     03   FILLER                  PIC X(3)  VALUE SPACE.                  
030300     03   LRAD-KDPRIO             PIC X(1).                               
030400     03   FILLER                  PIC X(3)  VALUE SPACE.                  
030500     03   LRAD-ADINLOMR-FB        PIC X(4).                               
030600     03   FILLER                  PIC X(5)  VALUE SPACE.                  
030700     03   LRAD-KDERS              PIC 9(2).                               
030800     03   FILLER                  PIC X(3)  VALUE SPACE.                  
030900     03   LRAD-BEFT               PIC Z(2).                               
031000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
031100     03   LRAD-IDLEVNR-KOLLI      PIC X(5).                               
031200     03   FILLER                  PIC X(3)  VALUE SPACE.                  
031300     03   LRAD-IDLOPNRM-IDOKOLLI  PIC Z(9).                               
031400     03   FILLER                  PIC X(11) VALUE SPACE.                  
031500                                                                          
031600 LINKAGE SECTION.                                                         
031700                                                                          
031800*01  -COPY W0009   -PRE MSG-                                              
031900     EJECT                                                                
032000*01  -COPY W0009   -PRE ALT-                                              
032100     EJECT                                                                
032200*01  -COPY W0008  -PRE LISB-                                              
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032500*01  -COPY W0008  -PRE INLE-                                              
032600     05  FILLER                  PIC X.                                   
032700     EJECT                                                                
032800*01  -COPY W0008  -PRE INLA1-                                             
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100*01  -COPY W0008  -PRE INLA2-                                             
033200     05  FILLER                  PIC X.                                   
033300     EJECT                                                                
033400*01  -COPY W0008  -PRE ARTD-                                              
033500     05  FILLER                  PIC X.                                   
033600                                                                          
033700*01  -COPY W0008  -PRE ARTC-                                              
033800     05  FILLER                  PIC X.                                   
033900                                                                          
034000*    PCB'ER FÖR SUBPGM                                                    
034100                                                                          
034200 01 STYR-HANA-PCB                PIC X.                                   
034300                                                                          
034400 01 STYR-PLAA-PCB                PIC X.                                   
034500                                                                          
034600     EJECT                                                                
034700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB LISB-PCB INLE-PCB              
034800                                   INLA1-PCB INLA2-PCB ARTD-PCB           
034900                                   ARTC-PCB                               
035000                                   STYR-HANA-PCB                          
035100                                   STYR-PLAA-PCB.                         
035200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB LISB-PCB INLE-PCB              
035300                                   INLA1-PCB INLA2-PCB ARTD-PCB           
035400                                   ARTC-PCB                               
035500                                   STYR-HANA-PCB                          
035600                                   STYR-PLAA-PCB.                         
035700                                                                          
035800 MAIN   SECTION.                                                          
035810                                                                          
035900     PERFORM IMS-GET-MSG                                                  
036000     IF SEGMENT-FINNS                                                     
036100       PERFORM A-INIT                                                     
036200       PERFORM B-SKAPA-ILISTA                                             
036300     END-IF                                                               
036400                                                                          
036500     PERFORM Z-FINIT                                                      
036600     MOVE ZERO TO RETURN-CODE                                             
036700     GOBACK                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 A-INIT SECTION.                                                          
037100                                                                          
037200     IF MSG-DUBBLA-TRANSKODER                                             
037300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I19901                 
037400     ELSE                                                                 
037500       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I19901                 
037600     END-IF                                                               
037700                                                                          
037800     PERFORM AA-OPEN-PRINTER                                              
037900     .                                                                    
038000     EJECT                                                                
038100 AA-OPEN-PRINTER         SECTION.                                         
038200                                                                          
038300     MOVE MID-IDPRTLST        TO WS-RAPP-PRINTER                          
038400     MOVE +1                  TO INDX                                     
038500     MOVE MID-IDILIST(INDX)   TO WS-RAPP-IDILIST                          
038600                                 W-SPAR-IDILIST                           
038700     IF WS-RAPP-PRINTER(1:5) = '6MRT6'                                    
038800     OR WS-RAPP-PRINTER(1:5) = '6MRT7'                                    
038900       MOVE '2'               TO PRT-FORMS-OVR                            
039000     END-IF                                                               
039100     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
039200                         PRT-OPEN                                         
039300                         WS-RAPP-PRINTER                                  
039400                         ALT-PCB                                          
039500                         LISB-PCB                                         
039600                         WS-RAPP-LISTID                                   
039700                         WS-DUMMY                                         
039800                         WS-DUMMY                                         
039900     .                                                                    
040000     EJECT                                                                
040100 B-SKAPA-ILISTA    SECTION.                                               
040200                                                                          
040300     MOVE +1                   TO INDX                                    
040400     PERFORM UNTIL INDX        >  MID-KVPOST                              
040500         MOVE ZERO             TO W-IDSIDNR                               
040600                                  W-SPAR-IDARTNR                          
040700         PERFORM BA-BEHANDLA-ILISTA                                       
040800         ADD +1                TO INDX                                    
040900     END-PERFORM                                                          
041000     .                                                                    
041100     EJECT                                                                
041200 BA-BEHANDLA-ILISTA SECTION.                                              
041300                                                                          
041400     MOVE 99                   TO W-KVRADER                               
041500                                                                          
041600     MOVE LOW-VALUE            TO W-W6D1D1KY-MIN-X                        
041700     MOVE HIGH-VALUE           TO W-W6D1D1KY-MAX-X                        
041800     MOVE MID-IDILIST(INDX)    TO W-D1D1KY-IDILIST-MIN                    
041900                                  W-D1D1KY-IDILIST-MAX                    
042000     MOVE 1                    TO W-D1D1KY-IDILIRAD-MIN                   
042100     PERFORM IMS-GU-INLE-INLE01                                           
042200                                                                          
042300     IF SEGMENT-FINNS                                                     
042400        PERFORM UNTIL W-D1D1KY-IDILIRAD-MIN > MID-KVRADER(INDX)           
042500            MOVE SEQD-IDDC               TO W-D101KY-IDDC                 
042600            MOVE SEQD-IDLEVNR            TO W-D101KY-IDLEVNR              
042700            MOVE SEQD-IDFS               TO W-D101KY-IDFS                 
042800            MOVE SEQD-TIAVIDAT           TO W-D101KY-TIAVIDAT             
042900            MOVE SEQD-IDRADNR-INL        TO W-IDRADNR-INL                 
043000            MOVE SEQD-IDRADNR            TO W-IDRADNR                     
043100                                                                          
043200            PERFORM IMS-GU-INLA1-INLA01                                   
043300            PERFORM BAA-SKAPA-RAD                                         
043400            ADD +1                       TO W-D1D1KY-IDILIRAD-MIN         
043500            PERFORM IMS-GN-INLE-INLE01                                    
043600        END-PERFORM                                                       
043700     END-IF                                                               
043800     .                                                                    
043900     EJECT                                                                
044000 BAA-SKAPA-RAD       SECTION.                                             
044100                                                                          
044200     PERFORM IMS-GNP-INLA1-INLA11                                         
044300     PERFORM IMS-GNP-INLA1-INLA21                                         
044400                                                                          
044500     MOVE RAD-IDILIRAD         TO LRAD-IDILIRAD                           
044600     MOVE ART-ADLAGOMR         TO LRAD-ADLAGOMR                           
044700                                  W-ADLAGOMR                              
044800     MOVE ART-ADGANG           TO LRAD-ADGANG                             
044900     MOVE ART-ADPLATS          TO LRAD-ADPLATS                            
045000     MOVE ART-IDARTNR          TO LRAD-IDARTNR                            
045100                                  W-IDARTNR                               
045200     MOVE ART-BEART            TO LRAD-BEART                              
045300     MOVE RAD-KVINLART         TO LRAD-KVINLART                           
045400     MOVE ART-BEFT             TO LRAD-BEFT                               
045500                                                                          
045600     IF RAD-KDINLPRIO          < +31                                      
045700         MOVE 'P'              TO LRAD-KDPRIO                             
045800     END-IF                                                               
045900                                                                          
046000     IF MID-IDOKOLLI(INDX)      >  ZERO                                   
046100         MOVE ART-IDLOPNRM      TO LRAD-IDLOPNRM-IDOKOLLI                 
046200     ELSE                                                                 
046300        IF RAD-IDOKOLLI         >  ZERO                                   
046400           MOVE RAD-IDLEVNR-KOLLI TO LRAD-IDLEVNR-KOLLI                   
046500           MOVE RAD-IDOKOLLI    TO LRAD-IDLOPNRM-IDOKOLLI                 
046600        ELSE                                                              
046700           MOVE ART-IDLOPNRM    TO LRAD-IDLOPNRM-IDOKOLLI                 
046800        END-IF                                                            
046900     END-IF                                                               
047000                                                                          
047100     PERFORM BAAA-KOLLA-OM-VOR                                            
047200                                                                          
047300     PERFORM BAAB-TA-FRAM-ADINLOMR-FB                                     
047400                                                                          
047500     IF ART-IDARTNR            =  W-SPAR-IDARTNR                          
047600         IF W-KVRADER          >  MAX-KVRADER                             
047700             PERFORM S10-SKAPA-HUVUD                                      
047800             MOVE PRT-AFTER-1  TO PRT-RADSKIP                             
047900             ADD +1            TO W-KVRADER                               
048000          ELSE                                                            
048100             MOVE PRT-AFTER-2  TO PRT-RADSKIP                             
048200             ADD +2            TO W-KVRADER                               
048300         END-IF                                                           
048400         MOVE LIST-LRAD        TO WS-RAPP-RAD                             
048500         PERFORM S01-SKRIV-RAD                                            
048600      ELSE                                                                
048700         MOVE ART-IDARTNR      TO W-SPAR-IDARTNR                          
048800         PERFORM BAAC-TA-FRAM-BUFFERTPL                                   
048900                                                                          
049000         PERFORM BAAD-HAMTA-KDERS                                         
049100                                                                          
049200         SET TAB-IX                TO +1                                  
049300         MOVE W-ADBUFFOMR (TAB-IX) TO LRAD-ADBUFFOMR                      
049400         MOVE W-ADBUFFGANG(TAB-IX) TO LRAD-ADBUFFGANG                     
049500         MOVE W-ADBUFFPL (TAB-IX)  TO LRAD-ADBUFFPL                       
049600         IF W-KVRADER              >  MAX-KVRADER                         
049700             PERFORM S10-SKAPA-HUVUD                                      
049800             MOVE PRT-AFTER-1      TO PRT-RADSKIP                         
049900             ADD +1                TO W-KVRADER                           
050000          ELSE                                                            
050100             MOVE PRT-AFTER-2      TO PRT-RADSKIP                         
050200             ADD +2                TO W-KVRADER                           
050300         END-IF                                                           
050400         MOVE LIST-LRAD            TO WS-RAPP-RAD                         
050500         PERFORM S01-SKRIV-RAD                                            
050600                                                                          
050700         SET TAB-IX UP BY +1                                              
050800         PERFORM UNTIL TAB-IX           > MAX-TAB-IX OR                   
050900                 W-ADBUFFOMR(TAB-IX)    = ZERO                            
051000             MOVE SPACE                 TO LIST-LRAD                      
051100             MOVE W-ADBUFFOMR (TAB-IX)  TO LRAD-ADBUFFOMR                 
051200             MOVE W-ADBUFFGANG(TAB-IX)  TO LRAD-ADBUFFGANG                
051300             MOVE W-ADBUFFPL (TAB-IX)   TO LRAD-ADBUFFPL                  
051400             IF W-KVRADER               >  MAX-KVRADER                    
051500                 PERFORM S10-SKAPA-HUVUD                                  
051600             END-IF                                                       
051700             MOVE PRT-AFTER-1           TO PRT-RADSKIP                    
051800             ADD +1                     TO W-KVRADER                      
051900             MOVE LIST-LRAD             TO WS-RAPP-RAD                    
052000             PERFORM S01-SKRIV-RAD                                        
052100             SET TAB-IX UP BY +1                                          
052200         END-PERFORM                                                      
052300     END-IF                                                               
052400     .                                                                    
052500     EJECT                                                                
052600 BAAA-KOLLA-OM-VOR   SECTION.                                             
052700                                                                          
052800     MOVE ZERO                 TO W-KVINLART-VOR                          
052900     MOVE RAD-IDLEVNR-KOLLI    TO W-D1CSEQ-IDLEVNR-KOLLI                  
053000                                  W-IDLEVNR-KOLLI                         
053100     MOVE RAD-IDOKOLLI         TO W-D1CSEQ-IDOKOLLI                       
053200                                  W-IDOKOLLI                              
053300                                                                          
053400     IF RAD-FLDIVKLI = JA                                                 
053500        PERFORM IMS-GU-INLA2-INLA11-FIRST                                 
053600        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                   
053700                      DIV-ART-IDLOPNRM = ART-IDLOPNRM                     
053800           PERFORM IMS-GN-INLA2-INLA11                                    
053900        END-PERFORM                                                       
054000        IF SEGMENT-FINNS                                                  
054100           IF DIV-ART-IDLOPNRM = ART-IDLOPNRM                             
054200              PERFORM IMS-GNP-INLA2-INLA21-FIRST                          
054300              PERFORM UNTIL SEGMENT-SAKNAS                                
054400                IF RAD-IDOKOLLI     = MID-IDOKOLLI(INDX)                  
054500                  IF RAD-KDINLSTA   = 'VOR'                               
054600                      COMPUTE W-KVINLART-VOR =                            
054700                              W-KVINLART-VOR + RAD-KVINLART               
054800                  END-IF                                                  
054900                END-IF                                                    
055000                PERFORM IMS-GNP-INLA2-INLA21                              
055100              END-PERFORM                                                 
055200           END-IF                                                         
055300        END-IF                                                            
055400     ELSE                                                                 
055500        PERFORM IMS-GU-INLA2-INLA11-FIRST                                 
055600        IF SEGMENT-FINNS                                                  
055700              PERFORM IMS-GNP-INLA2-INLA21-FIRST                          
055800              PERFORM UNTIL SEGMENT-SAKNAS                                
055900                IF RAD-IDOKOLLI     = MID-IDOKOLLI(INDX)                  
056000                  IF RAD-KDINLSTA   = 'VOR'                               
056100                      COMPUTE W-KVINLART-VOR =                            
056200                              W-KVINLART-VOR + RAD-KVINLART               
056300                  END-IF                                                  
056400                END-IF                                                    
056500                PERFORM IMS-GNP-INLA2-INLA21                              
056600              END-PERFORM                                                 
056700        END-IF                                                            
056800     END-IF                                                               
056900                                                                          
057000     MOVE W-KVINLART-VOR       TO LRAD-KVINLART-VOR                       
057100     .                                                                    
057200     EJECT                                                                
057300 BAAB-TA-FRAM-ADINLOMR-FB   SECTION.                                      
057400                                                                          
057500     MOVE ART-IDDC             TO STYR-IDDC                               
057600     MOVE ART-IDARTNR          TO STYR-IDARTNR                            
057700     MOVE ART-IDFKNGRP         TO STYR-IDFKNGRP                           
057800     MOVE INL-IDLEVNR          TO STYR-IDLEVNR                            
057900     MOVE ART-BEFT             TO STYR-BEFT                               
058000     CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                      
058100                                       STYR-PLAA-PCB                      
058200     MOVE STYR-ADINLOMR-FB     TO LRAD-ADINLOMR-FB                        
058300     .                                                                    
058400     EJECT                                                                
058500 BAAC-TA-FRAM-BUFFERTPL     SECTION.                                      
058600                                                                          
058700     SET TAB-IX                TO +1                                      
058800     PERFORM UNTIL TAB-IX      >  MAX-TAB-IX                              
058900         MOVE ZERO             TO W-ADBUFFOMR (TAB-IX)                    
059000                                  W-ADBUFFGANG(TAB-IX)                    
059100                                  W-ADBUFFPL  (TAB-IX)                    
059200         SET TAB-IX UP BY +1                                              
059300     END-PERFORM                                                          
059400                                                                          
059500     SET TAB-IX                TO +1                                      
059600     PERFORM IMS-GU-ARTD-ARTD11                                           
059700     PERFORM UNTIL SEGMENT-SAKNAS  OR TAB-IX > MAX-TAB-IX                 
059800         MOVE SALDO-ADBUFFOMR      TO W-ADBUFFOMR-ALFA                    
059900         IF W-ADBUFFOMR-ALFA       =  W-ADLAGOMR                          
060000             CONTINUE                                                     
060100          ELSE                                                            
060200             MOVE SALDO-ADBUFFOMR  TO W-ADBUFFOMR (TAB-IX)                
060300             MOVE SALDO-ADBUFFGANG TO W-ADBUFFGANG(TAB-IX)                
060400             MOVE SALDO-ADBUFFPL   TO W-ADBUFFPL  (TAB-IX)                
060500             SET TAB-IX UP BY +1                                          
060600         END-IF                                                           
060700         PERFORM IMS-GNP-ARTD-ARTD11                                      
060800     END-PERFORM                                                          
060900     .                                                                    
061000     EJECT                                                                
061100 BAAD-HAMTA-KDERS           SECTION.                                      
061200                                                                          
061300     PERFORM IMS-GU-WLARTC11                                              
061400     IF SEGMENT-FINNS                                                     
061500        MOVE ARTC-CLAG-KDERS      TO LRAD-KDERS                           
061600     ELSE                                                                 
061700        MOVE ZERO                 TO LRAD-KDERS                           
061800     END-IF                                                               
061900     .                                                                    
062000     EJECT                                                                
062100 Z-FINIT                   SECTION.                                       
062200                                                                          
062300     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
062400                         PRT-CLOSE                                        
062500                         WS-RAPP-PRINTER                                  
062600                         ALT-PCB                                          
062700                         LISB-PCB                                         
062800                         WS-RAPP-LISTID                                   
062900                         WS-DUMMY                                         
063000                         WS-DUMMY                                         
063100     .                                                                    
063200     EJECT                                                                
063300 S01-SKRIV-RAD SECTION.                                                   
063400                                                                          
063500     IF WS-RAPP-PRINTER(1:5) = '6MRT6'                                    
063600     OR WS-RAPP-PRINTER(1:5) = '6MRT7'                                    
063700       MOVE '2'               TO PRT-FORMS-OVR                            
063800     END-IF                                                               
063900     CALL W006PRR1 USING PRT-SPOOL-OVR                                    
064000                         PRT-WRITE                                        
064100                         WS-RAPP-PRINTER                                  
064200                         ALT-PCB                                          
064300                         LISB-PCB                                         
064400                         WS-RAPP-LISTID                                   
064500                         PRT-RADSKIP                                      
064600                         WS-RAPP-LISTRAD                                  
064700                                                                          
064800     MOVE SPACE                TO WS-RAPP-LISTRAD                         
064900     .                                                                    
065000     EJECT                                                                
065100                                                                          
065200 S10-SKAPA-HUVUD     SECTION.                                             
065300                                                                          
065400     IF MID-IDILIST(INDX) NOT     =  W-SPAR-IDILIST                       
065500        MOVE MID-IDILIST(INDX)    TO W-SPAR-IDILIST                       
065600                                     WS-RAPP-IDILIST                      
065700        IF WS-RAPP-PRINTER(1:5) = '6MRT6'                                 
065800        OR WS-RAPP-PRINTER(1:5) = '6MRT7'                                 
065900          MOVE '2'            TO PRT-FORMS-OVR                            
066000        END-IF                                                            
066100        CALL W006PRR1 USING PRT-SPOOL-OVR                                 
066200                            PRT-PURGE                                     
066300                            WS-RAPP-PRINTER                               
066400                            ALT-PCB                                       
066500                            LISB-PCB                                      
066600                            WS-RAPP-LISTID                                
066700                            WS-DUMMY                                      
066800                            WS-DUMMY                                      
066900     END-IF                                                               
067000                                                                          
067100     MOVE MID-IDILIST(INDX)       TO HRAD1-IDILIST                        
067200     COMPUTE W-IDSIDNR            =  W-IDSIDNR + 1                        
067300     MOVE W-IDSIDNR               TO HRAD1-IDSIDNR                        
067400     ACCEPT HRAD1-DATUM           FROM DATE                               
067500     ACCEPT WS-TIHHMMSSTH         FROM TIME                               
067600     MOVE WS-TIHH                 TO HRAD1-TID-TT                         
067700     MOVE WS-TIMM                 TO HRAD1-TID-MM                         
067800                                                                          
067900     MOVE PRT-NYSIDA-RAD3         TO PRT-RADSKIP                          
068000     MOVE LIST-HRAD1              TO WS-RAPP-RAD                          
068100     PERFORM S01-SKRIV-RAD                                                
068200                                                                          
068300     MOVE MID-ADINLOMR(INDX)      TO HRAD2-ADINLOMR                       
068400     MOVE MID-IDLEVNR-KOLLI(INDX) TO HRAD2-IDLEVNR-KOLLI                  
068500     MOVE MID-IDOKOLLI(INDX)      TO HRAD2-IDOKOLLI                       
068600     MOVE MID-IDINLVGN(INDX)      TO HRAD2-IDINLVGN                       
068700     MOVE 'L'                     TO HRAD2-ADLAGOMR (1:1)                 
068800     MOVE MID-ADLAGOMR(INDX)      TO HRAD2-ADLAGOMR (2:2)                 
068900     MOVE MID-ADINLOMR-TORG(INDX) TO HRAD2-ADINLOMR-TORG                  
069000     MOVE PRT-AFTER-2             TO PRT-RADSKIP                          
069100     MOVE LIST-HRAD2              TO WS-RAPP-RAD                          
069200     PERFORM S01-SKRIV-RAD                                                
069300                                                                          
069400     MOVE PRT-AFTER-2             TO PRT-RADSKIP                          
069500     MOVE LIST-HRUB3              TO WS-RAPP-RAD                          
069600     PERFORM S01-SKRIV-RAD                                                
069700                                                                          
069800     MOVE +9                      TO W-KVRADER                            
069900     .                                                                    
070000     EJECT                                                                
070100* --- IMS SEKTIONER ---                                                   
070200     SKIP3                                                                
070300 IMS-GET-MSG SECTION.                                                     
070400                                                                          
070500     MOVE '  QC' TO GODK-STATUSKODER                                      
070600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
070700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070800     PERFORM IMS-STATUSKONTROLL                                           
070900     .                                                                    
071000     SKIP3                                                                
071100 IMS-GU-INLE-INLE01 SECTION.                                              
071200     STRING 'W6INLE01(W6D1D1KY>=' W-W6D1D1KY-MIN-X                        
071300                    '&W6D1D1KY<=' W-W6D1D1KY-MAX-X ')'                    
071400          DELIMITED BY SIZE INTO SSA1                                     
071500     MOVE '  GE' TO GODK-STATUSKODER                                      
071600     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA1 SSA1                     
071700     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
071800     PERFORM IMS-STATUSKONTROLL                                           
071900     .                                                                    
072000     SKIP2                                                                
072100 IMS-GN-INLE-INLE01 SECTION.                                              
072200     STRING 'W6INLE01(W6D1D1KY>=' W-W6D1D1KY-MIN-X                        
072300                    '&W6D1D1KY<=' W-W6D1D1KY-MAX-X ')'                    
072400          DELIMITED BY SIZE INTO SSA1                                     
072500     MOVE '  GE' TO GODK-STATUSKODER                                      
072600     CALL CBLTDLI USING GN INLE-PCB DLI-IO-AREA1 SSA1                     
072700     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
072800     PERFORM IMS-STATUSKONTROLL                                           
072900     .                                                                    
073000     SKIP2                                                                
073100 IMS-GU-INLA1-INLA01 SECTION.                                             
073200     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
073300          DELIMITED BY SIZE INTO SSA1                                     
073400     MOVE '    ' TO GODK-STATUSKODER                                      
073500     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA1 SSA1                    
073600     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
073700     PERFORM IMS-STATUSKONTROLL                                           
073800     .                                                                    
073900     SKIP2                                                                
074000 IMS-GNP-INLA1-INLA11 SECTION.                                            
074100     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
074200          DELIMITED BY SIZE INTO SSA1                                     
074300     MOVE '    ' TO GODK-STATUSKODER                                      
074400     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA2 SSA1                   
074500     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
074600     PERFORM IMS-STATUSKONTROLL                                           
074700     .                                                                    
074800     SKIP2                                                                
074900 IMS-GNP-INLA1-INLA21 SECTION.                                            
075000     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
075100          DELIMITED BY SIZE INTO SSA1                                     
075200     MOVE '    ' TO GODK-STATUSKODER                                      
075300     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA3 SSA1                   
075400     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
075500     PERFORM IMS-STATUSKONTROLL                                           
075600     .                                                                    
075700     EJECT                                                                
075800 IMS-GU-INLA2-INLA11-FIRST  SECTION.                                      
075900     STRING 'W6INLA11*F(W6D1CSEQ =' W-W6D1CSEQ-X ')'                      
076000          DELIMITED BY SIZE INTO SSA1                                     
076100     MOVE '  GE' TO GODK-STATUSKODER                                      
076200     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA3 SSA1                    
076300     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
076400     PERFORM IMS-STATUSKONTROLL                                           
076500     .                                                                    
076600     SKIP3                                                                
076700 IMS-GN-INLA2-INLA11 SECTION.                                             
076800     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
076900          DELIMITED BY SIZE INTO SSA1                                     
077000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
077100     CALL CBLTDLI USING GN INLA2-PCB DLI-IO-AREA3 SSA1                    
077200     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
077300     PERFORM IMS-STATUSKONTROLL                                           
077400     .                                                                    
077500     EJECT                                                                
077600 IMS-GNP-INLA2-INLA21-FIRST SECTION.                                      
077700     MOVE  'W6INLA21*F' TO SSA1                                           
077800     MOVE '  GE' TO GODK-STATUSKODER                                      
077900     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA3 SSA1                   
078000     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
078100     PERFORM IMS-STATUSKONTROLL                                           
078200     .                                                                    
078300     SKIP3                                                                
078400 IMS-GNP-INLA2-INLA21       SECTION.                                      
078500     MOVE  'W6INLA21'   TO SSA1                                           
078600     MOVE '  GE' TO GODK-STATUSKODER                                      
078700     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA3 SSA1                   
078800     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
078900     PERFORM IMS-STATUSKONTROLL                                           
079000     .                                                                    
079100     EJECT                                                                
079200 IMS-GU-ARTD-ARTD11 SECTION.                                              
079300     STRING 'WLARTD01*P(IDARTNR  =' W-IDARTNR-X ')'                       
079400          DELIMITED BY SIZE INTO SSA1                                     
079500     STRING 'WLARTD11(IDDC     =' W-IDDC-X ')'                            
079600          DELIMITED BY SIZE INTO SSA2                                     
079700     MOVE '  GE' TO GODK-STATUSKODER                                      
079800     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA1 SSA1 SSA2                
079900     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
080000     PERFORM IMS-STATUSKONTROLL                                           
080100     .                                                                    
080200     SKIP3                                                                
080300 IMS-GNP-ARTD-ARTD11 SECTION.                                             
080400     STRING 'WLARTD11(IDDC     =' W-IDDC-X ')'                            
080500          DELIMITED BY SIZE INTO SSA1                                     
080600     MOVE '  GE' TO GODK-STATUSKODER                                      
080700     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA1 SSA1                    
080800     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
080900     PERFORM IMS-STATUSKONTROLL                                           
081000     .                                                                    
081100     SKIP3                                                                
081200 IMS-GU-WLARTC11    SECTION.                                              
081300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
081400          DELIMITED BY SIZE INTO SSA1                                     
081500     MOVE 'WLARTC11 ' TO SSA2                                             
081600     MOVE '  GE' TO GODK-STATUSKODER                                      
081700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1 SSA2                
081800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
081900     PERFORM IMS-STATUSKONTROLL                                           
082000     .                                                                    
082100     SKIP3                                                                
082200 IMS-STATUSKONTROLL SECTION.                                              
082300                                                                          
082400     SET STATUS-IX TO 1                                                   
082500     SEARCH GODK-STATUS                                                   
082600       AT END                                                             
082700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
082800         DELIMITED BY SIZE INTO FELTEXT                                   
082900         CALL FELLOG                                                      
083000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
083100         CONTINUE                                                         
083200     END-SEARCH                                                           
083300     .                                                                    
