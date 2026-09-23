000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2014900.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   14/02/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        BLOCKED DELIVERY PERIODS PER SUPPLIER                            
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDG3 (2257)                                
001100*                              WDGX2258                                   
001200*                              WDGX2260                                   
001210*                                                                         
001300*                              WDF1                                       
001310*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: W2T149                                              
001600*        MID:         W2I14901                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        MOD:         W2O14901                                            
001910*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W2014900'.            
003000                                                                          
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003200 77  CURRENT-SECTION             PIC X(80) VALUE SPACE.                   
003300 77  IMS-SECTION                 PIC X(80) VALUE SPACE.                   
003400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003710 77  WS-DAAVROP-TFOM-OK          PIC X       VALUE 'N'.                   
003711 77  WS-DAAVROP-OK               PIC X       VALUE 'Y'.                   
003800                                                                          
004010 77  WS-SAVE-DAAVROP-TOM         PIC 9(06)   VALUE ZERO.                  
004020                                                                          
004100 01  W-DAT-TIAAVV                PIC 9(04) VALUE ZERO.                    
004200                                                                          
005400 01  WS-MID-IDANSK-FOM-UPD        PIC X(03).                              
005410 01  FILLER REDEFINES WS-MID-IDANSK-FOM-UPD.                              
005500     03 WS-MID-IDANSK-FOM-NUM-UPD PIC 9(03).                              
005601                                                                          
005602 01  WS-MID-IDANSK-TOM-UPD        PIC X(03).                              
005603 01  FILLER REDEFINES WS-MID-IDANSK-TOM-UPD.                              
005604     03 WS-MID-IDANSK-TOM-NUM-UPD PIC 9(03).                              
005605                                                                          
005606 01  WS-DAAVROP-FOM-UPD          PIC 9(04).                               
005607 01  FILLER REDEFINES WS-DAAVROP-FOM-UPD.                                 
005608     03 WS-DAAVROP-FOM-YY-UPD    PIC 9(02).                               
005609     03 WS-DAAVROP-FOM-WW-UPD    PIC 9(02).                               
005610                                                                          
005611 01  WS-DAAVROP-TOM-UPD          PIC 9(04).                               
005612 01  FILLER REDEFINES WS-DAAVROP-TOM-UPD.                                 
005613     03 WS-DAAVROP-TOM-YY-UPD    PIC 9(02).                               
005614     03 WS-DAAVROP-TOM-WW-UPD    PIC 9(02).                               
005615                                                                          
005616 01  WS-DAAVROP-TFOM-UPD          PIC 9(04).                              
005617 01  FILLER REDEFINES WS-DAAVROP-TFOM-UPD.                                
005620     03 WS-DAAVROP-TFOM-YY-UPD   PIC 9(02).                               
005630     03 WS-DAAVROP-TFOM-WW-UPD   PIC 9(02).                               
006200                                                                          
006210 01  WS-DAAVROP                  PIC 9(6).                                
006220 01  FILLER REDEFINES WS-DAAVROP.                                         
006230     03  WS-DAAVROP-CC           PIC 9(02).                               
006240     03  WS-DAAVROP-YYWW         PIC 9(04).                               
006250                                                                          
006251 01  WS-MID-DAAVROP-FOM-UPD          PIC 9(6).                            
006252 01  FILLER REDEFINES WS-MID-DAAVROP-FOM-UPD.                             
006253     03  WS-MID-DAAVROP-FOM-CC-UPD   PIC 9(02).                           
006254     03  WS-MID-DAAVROP-FOM-YYWW-UPD PIC 9(04).                           
006255                                                                          
006256 01  WS-MID-DAAVROP-TOM-UPD          PIC 9(6).                            
006257 01  FILLER REDEFINES WS-MID-DAAVROP-TOM-UPD.                             
006258     03  WS-MID-DAAVROP-TOM-CC-UPD   PIC 9(02).                           
006259     03  WS-MID-DAAVROP-TOM-YYWW-UPD PIC 9(04).                           
006260                                                                          
006270 01  WS-MID-DAAVROP-TFOM-UPD         PIC 9(6).                            
006280 01  FILLER REDEFINES WS-MID-DAAVROP-TFOM-UPD.                            
006290     03  WS-MID-DAAVROP-TFOM-CC-UPD  PIC 9(02).                           
006300     03  WS-MID-DAAVROP-TFOM-YYWW-UPD PIC 9(04).                          
006301                                                                          
006310*    --- PARAMETER FOR ROUTINE W221S3                                     
006400 77  IX-PARM                     PIC S9(4)  VALUE +0    COMP SYNC.        
006500 01  PARM-W221S3-TAB.                                                     
006600     03  PARM-W221S3 OCCURS 10.                                           
006700         05  PARM-IDLEVNR-SHIP        PIC X(5).                           
006900         05  PARM-DAAVROP-FOM         PIC 9(6).                           
007100         05  PARM-DAAVROP-TOM         PIC 9(6).                           
007300         05  PARM-IDANSK-FOM          PIC 9(3).                           
007500         05  PARM-IDANSK-TOM          PIC 9(3).                           
007700                                                                          
007800*    --- INDEX FOR SCROLL LINES                                           
007910 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
008000 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
008010 77  TAB-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
008020 77  MAX-TAB-IX                  PIC S9(4)  VALUE +200  COMP SYNC.        
008021 77  TAB2-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
008022 77  MAX-TAB2-IX                 PIC S9(4)  VALUE +200  COMP SYNC.        
008030                                                                          
008100*    --- INTERN SORT TABLE FOR WDGX2260                                   
008101 01  WS-TAB-LINE.                                                         
008103     05 WS-TAB-IDANSK-FOM        PIC 9(03).                               
008104     05 WS-TAB-IDANSK-TOM        PIC 9(03).                               
008105     05 WS-TAB-DAAVROP-FOM       PIC 9(06).                               
008106     05 WS-TAB-DAAVROP-TOM       PIC 9(06).                               
008107                                                                          
008108 01  TABENTRY-PARM.                                                       
008109     03  STEGLANGD               PIC S9(9) COMP  VALUE 52.                
008110     03  ANTAL                   PIC S9(9) COMP.                          
008111     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 23.                
008112                                                                          
008113 01  TAB-2260.                                                            
008114     03 TAB-WDGX2260.                                                     
008115        05  TAB-RECORD OCCURS 200.                                        
008116            07 TAB-LINE.                                                  
008117               09 TAB-IDLEVNR-SHIP          PIC X(05).                    
008118               09 TAB-IDANSK-FOM            PIC 9(03).                    
008119               09 TAB-IDANSK-TOM            PIC 9(03).                    
008120               09 TAB-DAAVROP-FOM           PIC 9(06).                    
008121               09 TAB-DAAVROP-TOM           PIC 9(06).                    
008122               09 TAB-DAAVROP-TFOM          PIC 9(06).                    
008123            07 TAB-SORT.                                                  
008124               09 TAB-IDLEVNR-SHIP-SORT     PIC X(05).                    
008125               09 TAB-IDANSK-FOM-SORT       PIC 9(03).                    
008126               09 TAB-IDANSK-TOM-SORT       PIC 9(03).                    
008127               09 TAB-DAAVROP-FOM-SORT      PIC 9(06).                    
008128               09 TAB-DAAVROP-TOM-SORT      PIC 9(06).                    
008129                                                                          
008130 01  TAB2-DAAVROP-TFOM-HEAD.                                              
008131     03 TAB2-DAAVROP-TFOM-LINE.                                           
008132        05  TAB2-RECORD OCCURS 200.                                       
008133            07 TAB2-LINE.                                                 
008134               09 TAB2-IDLEVNR-SHIP         PIC X(05).                    
008135               09 TAB2-IDANSK-FOM           PIC 9(03).                    
008136               09 TAB2-IDANSK-TOM           PIC 9(03).                    
008137               09 TAB2-DAAVROP-FOM          PIC 9(06).                    
008138               09 TAB2-DAAVROP-TOM          PIC 9(06).                    
008146                                                                          
008150*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
008200                                                                          
008300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008400     88  INDATA-OK                           VALUE 'J'.                   
008500     88  INDATA-WRONG                        VALUE 'N'.                   
008600                                                                          
008700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
008800     88  KEYS-OK                             VALUE 'J'.                   
008900     88  KEYS-WRONG                          VALUE 'N'.                   
009000                                                                          
009100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009200     88  OWN-MID                             VALUE '2149'.                
009300     88  GOOD-MID                            VALUE '2141' '2142'          
009400                                                   '2143' '2144'          
009500                                                   '2145' '2146'          
009600                                                   '2147' '2148'          
009700                                                   '2149'.                
009800     88  HELP-MID                            VALUE '0551'.                
009900     EJECT                                                                
010000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
010100 01  GENERAL-SUBPROGRAMS.                                                 
010200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010610     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
010700     EJECT                                                                
010800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
010900*01 -COPY WMEDAREA                                                        
011000     EJECT                                                                
011100*01  -COPY WDATAREA                                                       
011200     EJECT                                                                
011300     SKIP3                                                                
011400 01  MESSAGE-CODES.                                                       
011500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012110     03  ERR-LINE-EXIST          PIC X(3)    VALUE '245'.                 
012120     03  ERR-INVALID-DATE        PIC X(3)    VALUE '492'.                 
012130     03  ERR-WRONG-INTERVAL      PIC X(3)    VALUE '738'.                 
012140     03  ERR-SUPP-MISSING        PIC X(3)    VALUE '273'.                 
012200     EJECT                                                                
012300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
012400*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012600     SKIP3                                                                
012700*01 -COPY WMSGINIT                                                        
012800     EJECT                                                                
012900 01  PROG-TO-PROG-SW.                                                     
013000*    03  -COPY WMSGSOP                                                    
013100     EJECT                                                                
013200*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
013300*                                                                         
013400 01  SAVE-AREA.                                                           
013500     03  SAVE-IDTRANS             PIC X(4)    VALUE '2149'.               
013600                                                                          
013700     03  SAVE-IDLEVNR-SHIP-ENTER  PIC X(5).                               
013800     03  SAVE-DAAVROP-FOM-ENTER   PIC 9(6).                               
013900     03  SAVE-DAAVROP-TOM-ENTER   PIC 9(6).                               
014000     03  SAVE-IDANSK-FOM-ENTER    PIC 9(3).                               
014100     03  SAVE-IDANSK-TOM-ENTER    PIC 9(3).                               
014200                                                                          
014300     03  SAVE-IDLEVNR-SHIP-NEXT   PIC X(5).                               
014400     03  SAVE-DAAVROP-FOM-NEXT    PIC 9(6).                               
014500     03  SAVE-DAAVROP-TOM-NEXT    PIC 9(6).                               
014600     03  SAVE-IDANSK-FOM-NEXT     PIC 9(3).                               
014700     03  SAVE-IDANSK-TOM-NEXT     PIC 9(3).                               
014800     EJECT                                                                
014900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
015000*                                                                         
015100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015200     SKIP3                                                                
015300*01  MID -COPY W2I14901                                                   
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015600     SKIP3                                                                
015700*01  -COPY WMSGAREA                                                       
015800     EJECT                                                                
015900     03  MOD REDEFINES MSG-AREA.                                          
016000*      05  -COPY W2O14901                                                 
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016300     SKIP3                                                                
016400*01  -COPY WMFSAREA                                                       
016500     EJECT                                                                
016600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
016700*                                                                         
016800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016900     SKIP3                                                                
017000 01  KEYS-FOR-DLI.                                                        
017100*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
017200     03  W-WDGXKEY-2257-X.                                                
017300         05  W-IDHTYP-2257       PIC X(4)    VALUE '2257'.                
017400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
017500                                                                          
017810     03  W-WDGX2258-X.                                                    
017820         05  W-IDLEVNR-SHIP      PIC X(5)    VALUE SPACE.                 
017830                                                                          
017840     03  W-IDLEVNR-X.                                                     
017850         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
017860                                                                          
017900     03  W-DAAVROP-X.                                                     
018000         05 W-DAAVROP              PIC 9(6)  VALUE ZERO.                  
018100                                                                          
018110     03  W-DAAVROP-FOM-X.                                                 
018120         05 W-DAAVROP-FOM          PIC 9(6)  VALUE ZERO.                  
018130                                                                          
018200     03  W-DAAVROP-TOM-X.                                                 
018300         05 W-DAAVROP-TOM          PIC 9(6)  VALUE ZERO.                  
018400                                                                          
018500     03  W-IDANSK-X.                                                      
018600         05 W-IDANSK               PIC S9(3) VALUE ZERO COMP-3.           
018700                                                                          
018710     03  W-IDANSK-FOM-X.                                                  
018720         05 W-IDANSK-FOM           PIC S9(3) VALUE ZERO COMP-3.           
018730                                                                          
018800     03  W-IDANSK-TOM-X.                                                  
018900         05 W-IDANSK-TOM           PIC S9(3) VALUE ZERO COMP-3.           
019000                                                                          
019100     SKIP2                                                                
019200*    --- STATUS CODES FROM IMS                                            
019300 01  STATUS-WS                   PIC XX.                                  
019400     88  SEGMENT-FOUND                       VALUE '  '.                  
019500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
019600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
019700     SKIP2                                                                
019800 01  GOOD-STATUSCODES.                                                    
019900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020000     SKIP3                                                                
020100 01  SSA1                        PIC X(224).                              
020200 01  SSA2                        PIC X(224).                              
020300 01  SSA3                        PIC X(224).                              
020400     EJECT                                                                
020500*    --- IMS FUNCTION CODES                                               
020600*01  -COPY W0003                                                          
020700     EJECT                                                                
020800*    ---  DLI INPUT-OUTPUT AREA                                           
020900                                                                          
021000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
021100 01  DLI-IO-WDF101.                                                       
021200*    03  -COPY WDF101                                                     
021300                                                                          
021310 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
021320 01  DLI-IO-WDGX01.                                                       
021330*    03  -COPY WDGX01                                                     
021340                                                                          
021400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2258'.                    
021500 01  DLI-IO-WDGX2258.                                                     
021600*    03  -COPY WDGX2258                                                   
021700                                                                          
021800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2260'.                    
021900 01  DLI-IO-WDGX2260.                                                     
022000*    03  -COPY WDGX2260                                                   
022100     EJECT                                                                
022200 LINKAGE SECTION.                                                         
022300*01  -COPY W0009   -PRE MSG-                                              
022400                                                                          
022500*01  -COPY W0009   -PRE ALT-                                              
022600                                                                          
022700*01  -COPY W0009   -PRE USEA-                                             
022800                                                                          
022900*01  -COPY W0008   -PRE WDG3-                                             
023000     05  FILLER                  PIC X.                                   
023100                                                                          
023110*01  -COPY W0008   -PRE WDF1-                                             
023120     05  FILLER                  PIC X.                                   
023130                                                                          
023200     EJECT                                                                
023300 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
023310                           WDG3-PCB WDF1-PCB.                             
023400 MAIN SECTION.                                                            
023500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
023510                           WDG3-PCB WDF1-PCB.                             
023600                                                                          
023700     PERFORM IMS-GET-MSG                                                  
023800     IF SEGMENT-FOUND                                                     
023900       PERFORM A-INIT                                                     
024000       PERFORM B-CHECK-KEYS                                               
024100       IF KEYS-OK                                                         
024200         IF MFS-UPDATE                                                    
024300           PERFORM G-CHECK-INPUT                                          
024400           IF INDATA-OK                                                   
024500             PERFORM H-UPDATE                                             
024600           END-IF                                                         
024700         ELSE                                                             
024800           IF MFS-FIRST                                                   
024900             PERFORM C-FIRST-PAGE                                         
025000           ELSE                                                           
025100             IF MFS-NEXT                                                  
025200               PERFORM D-NEXT-PAGE                                        
025300             ELSE                                                         
025400               PERFORM E-SAME-PAGE                                        
025500             END-IF                                                       
025600           END-IF                                                         
025700         END-IF                                                           
025800         PERFORM F-READ-SHOW-INFO                                         
025900       END-IF                                                             
026100*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
026200*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
026300       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O14901 + 4                      
026400       PERFORM IMS-INSERT-MSG                                             
026500     END-IF                                                               
026600                                                                          
026700     MOVE ZERO TO RETURN-CODE                                             
026800     GOBACK                                                               
026900     .                                                                    
027000     EJECT                                                                
027100 A-INIT SECTION.                                                          
027200     MOVE 'A-INIT SECTION              '  TO CURRENT-SECTION              
027300                                                                          
027400     IF MSG-DOUBLE-TRANSACTIONS                                           
027500       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I14901                 
027600       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
027700       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
027800     ELSE                                                                 
027900       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W2I14901                 
028000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
028100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
028200     END-IF                                                               
028300                                                                          
028400     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
028500     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
028600     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
028700                                                                          
028800     MOVE LOW-VALUE                       TO MSG-AREA                     
028900     MOVE 'W2O149N1'                      TO MFS-IDMOD                    
029000     MOVE '2149'                          TO MOD-IDTRANS                  
029100     MOVE MFS-ERASE-FIELD                 TO MOD-TEMFSFEL                 
029110                                             MOD-TEMFSINF                 
029200                                                                          
029300     IF OWN-MID OR HELP-MID                                               
029400       CONTINUE                                                           
029500     ELSE                                                                 
029600       MOVE SPACE TO MFS-KDTRTYP                                          
029700       MOVE '7' TO MFS-IDPFK                                              
029800     END-IF                                                               
029900                                                                          
030000     PERFORM AA-DAGENS-DATUM                                              
030100     .                                                                    
030200     EJECT                                                                
030300 AA-DAGENS-DATUM SECTION.                                                 
030400     MOVE 'AA-DAGENS-DATUM '   TO CURRENT-SECTION                         
030500                                                                          
030600     MOVE 'IDAG  '                 TO DAT-KDDATFORM                       
030700                                                                          
030800     CALL WDATKONV USING DAT-KDDATFORM                                    
030900                         DAT-I-TIDATUM                                    
031000                         DAT-O-TIDATUM                                    
031100                         DAT-KDSVAR                                       
031101                                                                          
031110     MOVE DAT-TIAAVV-GRP           TO W-DAT-TIAAVV                        
031200     .                                                                    
031300     EJECT                                                                
031400 B-CHECK-KEYS SECTION.                                                    
031500     MOVE 'B-CHECK-KEYS                '  TO CURRENT-SECTION              
031600                                                                          
031700     MOVE ALL '+'                  TO MSGI-WMSGINIT                       
031800     MOVE '001'                    TO MSGI-KDCALL                         
031900     MOVE MSG-LTERM-NAME           TO MSGI-IDLTERM-USER                   
032000     MOVE MSG-SIGNON-USERID        TO MSGI-IDUSER                         
032100     MOVE '2149'                   TO MSGI-IDTRANS                        
032200     IF GOOD-MID                                                          
032300        MOVE MID-IDLEVNR-SHIP-IN   TO MSGI-IDLEVNR                        
032400     END-IF                                                               
032500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
032600     MOVE MSGI-SPAR-AREA           TO SAVE-AREA                           
032700                                                                          
032800*    - LANGUAGE TO BE USED BY MEDKONV                                     
032900     MOVE MSGI-IDLAND-SPR          TO MED-IDSKYLT                         
033000                                                                          
033100     MOVE YES                      TO KEYS-SW                             
033200                                                                          
033300                                                                          
033400*    -- CHECK OF IDLEVNR-SHIP                                             
033500     MOVE MFS-ERASE-FIELD          TO MOD-IDLEVNR-SHIP-IN                 
033600                                                                          
033700     IF MID-IDLEVNR-SHIP-IN NOT = ALL '+'                                 
033800       MOVE '7'                    TO MFS-IDPFK                           
033900       MOVE SPACE                  TO MFS-KDTRTYP                         
034000     END-IF                                                               
034100     MOVE MSGI-IDLEVNR             TO W-IDLEVNR-SHIP                      
034200                                                                          
034300     IF GOOD-MID OR KEYS-OK                                               
034400       MOVE MSGI-IDLEVNR           TO MOD-IDLEVNR-SHIP-UT                 
034500     ELSE                                                                 
034600       MOVE MFS-ERASE-FIELD        TO MOD-IDLEVNR-SHIP-UT                 
034700     END-IF                                                               
034800                                                                          
034900     IF KEYS-WRONG                                                        
035000       MOVE ERR-WRONG-KEY          TO MED-IDMFSFEL                        
035100       CALL WMEDKONV USING MED-WMEDAREA                                   
035200       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
035300       PERFORM MFS-ERASE-FIELD-IN                                         
035400       PERFORM MFS-ERASE-FIELD-OUT                                        
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 C-FIRST-PAGE SECTION.                                                    
035900     MOVE 'C-FIRST-PAGE                '  TO CURRENT-SECTION              
036000                                                                          
036100     MOVE INF-FIRST-PAGE           TO MED-IDMFSINF                        
036200     CALL WMEDKONV              USING MED-WMEDAREA                        
036300     MOVE MED-MFSINF               TO MOD-TEMFSFEL                        
036400                                                                          
036500     PERFORM MFS-ERASE-FIELD-IN                                           
036600     .                                                                    
036700     EJECT                                                                
036800 D-NEXT-PAGE SECTION.                                                     
036900     MOVE 'D-NEXT-PAGE                 '  TO CURRENT-SECTION              
037000                                                                          
037100     IF SAVE-IDTRANS = '2149'                                             
037200       MOVE SAVE-IDLEVNR-SHIP-NEXT TO W-IDLEVNR-SHIP                      
037300       MOVE SAVE-DAAVROP-FOM-NEXT  TO W-DAAVROP-FOM                       
037400       MOVE SAVE-DAAVROP-TOM-NEXT  TO W-DAAVROP-TOM                       
037500       MOVE SAVE-IDANSK-FOM-NEXT   TO W-IDANSK-FOM                        
037600       MOVE SAVE-IDANSK-TOM-NEXT   TO W-IDANSK-TOM                        
037700     ELSE                                                                 
037800       PERFORM MFS-ERASE-FIELD-IN                                         
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 E-SAME-PAGE SECTION.                                                     
038300     MOVE 'E-SAME-PAGE                 '  TO CURRENT-SECTION              
038400                                                                          
038500     IF SAVE-IDTRANS = '2149' OR '0551'                                   
038600       MOVE SAVE-IDLEVNR-SHIP-ENTER TO W-IDLEVNR-SHIP                     
038700       MOVE SAVE-DAAVROP-FOM-ENTER  TO W-DAAVROP-FOM                      
038800       MOVE SAVE-DAAVROP-TOM-ENTER  TO W-DAAVROP-TOM                      
038900       MOVE SAVE-IDANSK-FOM-ENTER   TO W-IDANSK-FOM                       
039000       MOVE SAVE-IDANSK-TOM-ENTER   TO W-IDANSK-TOM                       
039010                                                                          
039100       IF MID-IDLEVNR-SHIP-IN   = ALL '+'                                 
039101      AND MID-KDCMD-UPD (01)    = ALL '+'                                 
039110      AND MID-KDCMD-UPD (02)    = ALL '+'                                 
039120      AND MID-KDCMD-UPD (03)    = ALL '+'                                 
039130      AND MID-KDCMD-UPD (04)    = ALL '+'                                 
039140      AND MID-KDCMD-UPD (05)    = ALL '+'                                 
039150      AND MID-KDCMD-UPD (06)    = ALL '+'                                 
039160      AND MID-KDCMD-UPD (07)    = ALL '+'                                 
039170      AND MID-KDCMD-UPD (08)    = ALL '+'                                 
039180      AND MID-KDCMD-UPD (09)    = ALL '+'                                 
039190      AND MID-KDCMD-UPD (10)    = ALL '+'                                 
039191      AND MID-UPD-AREA          = ALL '+'                                 
039200         PERFORM MFS-ERASE-FIELD-IN                                       
039300       ELSE                                                               
039400         MOVE INF-PRESS-PF11        TO MED-IDMFSINF                       
039500         CALL WMEDKONV           USING MED-WMEDAREA                       
039600         MOVE MED-MFSINF            TO MOD-TEMFSFEL                       
039700         PERFORM EA-MID-INDATA-FOR-MOD                                    
039800       END-IF                                                             
039900     ELSE                                                                 
040000       PERFORM MFS-ERASE-FIELD-IN                                         
040100     END-IF                                                               
040200     .                                                                    
040300     EJECT                                                                
040400 EA-MID-INDATA-FOR-MOD SECTION.                                           
040500     MOVE 'EA-MID-INDATA-FOR-MOD       '  TO CURRENT-SECTION              
040600                                                                          
040700* * * * * FOR EVERY MID-FIELD                                             
040800* * * * * IF MID-FIELD NOT = ALL '+' MOVE MID-FIELD TO MOD-INPUT-F        
040900* * * * *        MOVE MFS-ADD-READ-FIELD TO MOD-INDATA-ATTR               
041000* * * * * ELSE ERASE MOD-INPUT-FIELD                                      
041100                                                                          
041200     MOVE +1  TO INDX                                                     
041300     PERFORM UNTIL INDX > MAX-INDX                                        
041400       IF MID-KDCMD-UPD (INDX) NOT = ALL '+'                              
041500        MOVE MID-KDCMD-UPD    (INDX)  TO MOD-KDCMD-UPD     (INDX)         
041600        MOVE MFS-ADD-READ-FIELD       TO MOD-KDCMD-UPD-ATTR(INDX)         
041700        MOVE MID-IDLEVNR-SHIP (INDX)  TO MOD-IDLEVNR-SHIP  (INDX)         
041800        MOVE MID-IDANSK-FOM   (INDX)  TO MOD-IDANSK-FOM    (INDX)         
041900        MOVE MID-IDANSK-TOM   (INDX)  TO MOD-IDANSK-TOM    (INDX)         
042000        MOVE MID-DAAVROP-FOM  (INDX)  TO MOD-DAAVROP-FOM   (INDX)         
042100        MOVE MID-DAAVROP-TOM  (INDX)  TO MOD-DAAVROP-TOM   (INDX)         
042200       ELSE                                                               
042300        MOVE MFS-ERASE-FIELD          TO MOD-KDCMD-UPD     (INDX)         
042400                                         MOD-IDLEVNR-SHIP  (INDX)         
042500                                         MOD-IDANSK-FOM    (INDX)         
042600                                         MOD-IDANSK-TOM    (INDX)         
042700                                         MOD-DAAVROP-FOM   (INDX)         
042800                                         MOD-DAAVROP-TOM   (INDX)         
042900       END-IF                                                             
043000       ADD  +1                        TO INDX                             
043100     END-PERFORM                                                          
043200                                                                          
043300     IF MID-IDLEVNR-SHIP-UPD NOT = ALL '+'                                
043400        MOVE MID-IDLEVNR-SHIP-UPD TO MOD-IDLEVNR-SHIP-UPD                 
043500        MOVE MFS-ADD-READ-FIELD   TO MOD-IDLEVNR-SHIP-UPD-ATTR            
043600     ELSE                                                                 
043700        MOVE MFS-ERASE-FIELD      TO MOD-IDLEVNR-SHIP-UPD                 
043800     END-IF                                                               
043900                                                                          
044000     IF MID-IDANSK-FOM-UPD NOT = ALL '+'                                  
044100        MOVE MID-IDANSK-FOM-UPD   TO MOD-IDANSK-FOM-UPD                   
044200        MOVE MFS-ADD-READ-FIELD   TO MOD-IDANSK-FOM-UPD-ATTR              
044300     ELSE                                                                 
044400        MOVE MFS-ERASE-FIELD      TO MOD-IDANSK-FOM-UPD                   
044500     END-IF                                                               
044600                                                                          
044700     IF MID-IDANSK-TOM-UPD NOT = ALL '+'                                  
044800        MOVE MID-IDANSK-TOM-UPD   TO MOD-IDANSK-TOM-UPD                   
044900        MOVE MFS-ADD-READ-FIELD   TO MOD-IDANSK-TOM-UPD-ATTR              
045000     ELSE                                                                 
045100        MOVE MFS-ERASE-FIELD      TO MOD-IDANSK-TOM-UPD                   
045200     END-IF                                                               
045300                                                                          
045400     IF MID-DAAVROP-FOM-UPD NOT = ALL '+'                                 
045500        MOVE MID-DAAVROP-FOM-UPD  TO MOD-DAAVROP-FOM-UPD                  
045600        MOVE MFS-ADD-READ-FIELD   TO MOD-DAAVROP-FOM-UPD-ATTR             
045700     ELSE                                                                 
045800        MOVE MFS-ERASE-FIELD      TO MOD-DAAVROP-FOM-UPD                  
045900     END-IF                                                               
046000                                                                          
046100     IF MID-DAAVROP-TOM-UPD NOT = ALL '+'                                 
046200        MOVE MID-DAAVROP-TOM-UPD  TO MOD-DAAVROP-TOM-UPD                  
046300        MOVE MFS-ADD-READ-FIELD   TO MOD-DAAVROP-TOM-UPD-ATTR             
046400     ELSE                                                                 
046500        MOVE MFS-ERASE-FIELD      TO MOD-DAAVROP-TOM-UPD                  
046600     END-IF                                                               
046700                                                                          
046800     IF MID-DAAVROP-TFOM-UPD NOT = ALL '+'                                
046900        MOVE MID-DAAVROP-TFOM-UPD TO MOD-DAAVROP-TFOM-UPD                 
047000        MOVE MFS-ADD-READ-FIELD   TO MOD-DAAVROP-TFOM-UPD-ATTR            
047100     ELSE                                                                 
047200        MOVE MFS-ERASE-FIELD      TO MOD-DAAVROP-TFOM-UPD                 
047300     END-IF                                                               
047400     .                                                                    
047500     EJECT                                                                
047600 F-READ-SHOW-INFO SECTION.                                                
047700     MOVE 'F-READ-SHOW-INFO            '  TO CURRENT-SECTION              
047800                                                                          
047900     MOVE +1                           TO INDX                            
048000     PERFORM IMS-GU-WDGX2257                                              
048100                                                                          
048200     PERFORM IMS-GNP-WDGX2258                                             
048300     PERFORM UNTIL SEGMENT-MISSING OR INDX > MAX-INDX                     
048400                                                                          
048500        MOVE 2258-IDLEVNR-SHIP         TO W-IDLEVNR-SHIP                  
048600        PERFORM IMS-GNP-WDGX2258-WDGX2260                                 
048700        PERFORM UNTIL SEGMENT-MISSING OR INDX > MAX-INDX                  
048900           MOVE 2258-IDLEVNR-SHIP      TO MOD-IDLEVNR-SHIP (INDX)         
049000           MOVE 2260-DAAVROP-FOM       TO WS-DAAVROP                      
049100           MOVE WS-DAAVROP-YYWW        TO MOD-DAAVROP-FOM  (INDX)         
049200           MOVE 2260-DAAVROP-TOM       TO WS-DAAVROP                      
049300           MOVE WS-DAAVROP-YYWW        TO MOD-DAAVROP-TOM  (INDX)         
049400           MOVE 2260-IDANSK-FOM        TO MOD-IDANSK-FOM   (INDX)         
049500           MOVE 2260-IDANSK-TOM        TO MOD-IDANSK-TOM   (INDX)         
049600           MOVE 2260-DAAVROP-TFOM      TO WS-DAAVROP                      
049700           MOVE WS-DAAVROP-YYWW        TO MOD-DAAVROP-TFOM (INDX)         
049800                                                                          
049910           IF INDX = +1                                                   
049920             MOVE MOD-IDLEVNR-SHIP(01) TO SAVE-IDLEVNR-SHIP-ENTER         
049921                                          SAVE-IDLEVNR-SHIP-NEXT          
049930             MOVE 20                   TO WS-DAAVROP-CC                   
049940             MOVE MOD-DAAVROP-FOM (01) TO WS-DAAVROP-YYWW                 
049950             MOVE WS-DAAVROP           TO SAVE-DAAVROP-FOM-ENTER          
049951                                          SAVE-DAAVROP-FOM-NEXT           
049960             MOVE MOD-DAAVROP-TOM (01) TO WS-DAAVROP-YYWW                 
049970             MOVE WS-DAAVROP           TO SAVE-DAAVROP-TOM-ENTER          
049971                                          SAVE-DAAVROP-TOM-NEXT           
049980             MOVE MOD-IDANSK-FOM  (01) TO SAVE-IDANSK-FOM-ENTER           
049981                                          SAVE-IDANSK-FOM-NEXT            
049990             MOVE MOD-IDANSK-TOM  (01) TO SAVE-IDANSK-TOM-ENTER           
049991                                          SAVE-IDANSK-TOM-NEXT            
049997           END-IF                                                         
049998                                                                          
049999           PERFORM IMS-GNP-WDGX2258-WDGX2260                              
050110                                                                          
050200           ADD +1                  TO INDX                                
050300        END-PERFORM                                                       
052600                                                                          
052610        IF SEGMENT-FOUND                                                  
052620          IF INDX > MAX-INDX                                              
052630            MOVE 2258-IDLEVNR-SHIP      TO SAVE-IDLEVNR-SHIP-NEXT         
052640            MOVE 2260-DAAVROP-FOM       TO SAVE-DAAVROP-FOM-NEXT          
052650            MOVE 2260-DAAVROP-TOM       TO SAVE-DAAVROP-TOM-NEXT          
052660            MOVE 2260-IDANSK-FOM        TO SAVE-IDANSK-FOM-NEXT           
052670            MOVE 2260-IDANSK-TOM        TO SAVE-IDANSK-TOM-NEXT           
052680            MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                   
052690            CALL WMEDKONV            USING MED-WMEDAREA                   
052691            MOVE MED-TEMFSINF           TO MOD-TEMFSINF                   
052698          END-IF                                                          
052699        ELSE                                                              
052702          PERFORM IMS-GNP-WDGX2258                                        
052703          IF SEGMENT-FOUND                                                
052704            MOVE 2258-IDLEVNR-SHIP      TO W-IDLEVNR-SHIP                 
052705            MOVE ZERO                   TO W-DAAVROP-FOM                  
052706                                           W-DAAVROP-TOM                  
052707                                           W-IDANSK-FOM                   
052708                                           W-IDANSK-TOM                   
052709            IF INDX > MAX-INDX                                            
052710              PERFORM IMS-GNP-WDGX2258-WDGX2260                           
052711              MOVE 2258-IDLEVNR-SHIP    TO SAVE-IDLEVNR-SHIP-NEXT         
052712              MOVE 2260-DAAVROP-FOM     TO SAVE-DAAVROP-FOM-NEXT          
052713              MOVE 2260-DAAVROP-TOM     TO SAVE-DAAVROP-TOM-NEXT          
052714              MOVE 2260-IDANSK-FOM      TO SAVE-IDANSK-FOM-NEXT           
052715              MOVE 2260-IDANSK-TOM      TO SAVE-IDANSK-TOM-NEXT           
052716              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
052717              CALL WMEDKONV          USING MED-WMEDAREA                   
052718              MOVE MED-TEMFSINF         TO MOD-TEMFSINF                   
052719            END-IF                                                        
052720          END-IF                                                          
052730        END-IF                                                            
052800                                                                          
052900     END-PERFORM                                                          
053127                                                                          
053130     PERFORM UNTIL INDX > MAX-INDX                                        
053200        MOVE MFS-CLOSE-FIELD       TO MOD-KDCMD-UPD-ATTR(INDX)            
053220        MOVE MFS-ERASE-FIELD       TO MOD-KDCMD-UPD     (INDX)            
053230                                      MOD-IDLEVNR-SHIP  (INDX)            
053300                                      MOD-DAAVROP-FOM   (INDX)            
053400                                      MOD-DAAVROP-TOM   (INDX)            
053500                                      MOD-IDANSK-FOM    (INDX)            
053600                                      MOD-IDANSK-TOM    (INDX)            
053700                                      MOD-DAAVROP-TFOM  (INDX)            
053800        ADD 1                      TO INDX                                
053900     END-PERFORM                                                          
054000                                                                          
054100                                                                          
054200     MOVE '002'                    TO MSGI-KDCALL                         
054300     MOVE '2149'                   TO SAVE-IDTRANS                        
054400     MOVE SAVE-AREA                TO MSGI-SPAR-AREA                      
054500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
054600     .                                                                    
054700     EJECT                                                                
054800 G-CHECK-INPUT SECTION.                                                   
054900     MOVE 'G-CHECK-INPUT               '  TO CURRENT-SECTION              
055000                                                                          
055100     MOVE YES  TO INDATA-SW                                               
055200                                                                          
055210      IF MID-IDLEVNR-SHIP-IN   = ALL '+'                                  
055220     AND MID-KDCMD-UPD (01)    = ALL '+'                                  
055230     AND MID-KDCMD-UPD (02)    = ALL '+'                                  
055240     AND MID-KDCMD-UPD (03)    = ALL '+'                                  
055250     AND MID-KDCMD-UPD (04)    = ALL '+'                                  
055260     AND MID-KDCMD-UPD (05)    = ALL '+'                                  
055270     AND MID-KDCMD-UPD (06)    = ALL '+'                                  
055280     AND MID-KDCMD-UPD (07)    = ALL '+'                                  
055290     AND MID-KDCMD-UPD (08)    = ALL '+'                                  
055291     AND MID-KDCMD-UPD (09)    = ALL '+'                                  
055292     AND MID-KDCMD-UPD (10)    = ALL '+'                                  
055293     AND MID-UPD-AREA          = ALL '+'                                  
055400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
055500       CALL WMEDKONV          USING MED-WMEDAREA                          
055600       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
055700       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
055800       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
055900       MOVE NOO                  TO INDATA-SW                             
056200     ELSE                                                                 
056300       MOVE +1  TO INDX                                                   
056400       PERFORM UNTIL INDX > MAX-INDX                                      
056500        IF MID-KDCMD-UPD (INDX) NOT = ALL '+'                             
056600          IF MID-KDCMD-UPD (INDX) = 'B' OR 'D'                            
056700            MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDCMD-UPD-ATTR(INDX)        
056800          ELSE                                                            
056900            MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDCMD-UPD-ATTR(INDX)        
057000            MOVE NOO                   TO INDATA-SW                       
057200          END-IF                                                          
057300        ELSE                                                              
057301          MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDCMD-UPD-ATTR(INDX)        
057310        END-IF                                                            
057400        ADD +1                          TO INDX                           
057500       END-PERFORM                                                        
057600                                                                          
057700       IF MID-UPD-AREA = ALL '+'                                          
058300          CONTINUE                                                        
058400       ELSE                                                               
058500         IF MID-IDLEVNR-SHIP-UPD = ALL '+'                                
058600           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDLEVNR-SHIP-UPD-ATTR        
058700           MOVE NOO                   TO INDATA-SW                        
058900         ELSE                                                             
058910           MOVE MID-IDLEVNR-SHIP-UPD  TO W-IDLEVNR                        
058920           PERFORM IMS-GU-WDF101                                          
058930           IF SEGMENT-MISSING                                             
058931             MOVE MFS-ALPHA-FIELD-WRONG                                   
058932                                      TO MOD-IDLEVNR-SHIP-UPD-ATTR        
058933             MOVE ERR-SUPP-MISSING    TO MED-IDMFSFEL                     
058934             MOVE NOO                 TO INDATA-SW                        
058940           ELSE                                                           
059000              MOVE MFS-ALPHA-FIELD-OK TO MOD-IDLEVNR-SHIP-UPD-ATTR        
059010           END-IF                                                         
059100         END-IF                                                           
059200                                                                          
059300         IF MID-DAAVROP-FOM-UPD = ALL '+'                                 
059400           MOVE MFS-NUM-FIELD-WRONG   TO MOD-DAAVROP-FOM-UPD-ATTR         
059500           MOVE NOO                   TO INDATA-SW                        
059700         ELSE                                                             
059800           IF MID-DAAVROP-FOM-UPD NOT NUMERIC                             
059900             MOVE MFS-NUM-FIELD-WRONG TO MOD-DAAVROP-FOM-UPD-ATTR         
059910             MOVE ERR-INVALID-DATE    TO MED-IDMFSFEL                     
060000             MOVE NOO                 TO INDATA-SW                        
060200           ELSE                                                           
060300             MOVE 'AAVV'              TO DAT-KDDATFORM                    
060400             MOVE MID-DAAVROP-FOM-UPD TO DAT-I-TIDATUM                    
060500             CALL WDATKONV         USING DAT-KDDATFORM                    
060600                                         DAT-I-TIDATUM                    
060700                                         DAT-O-TIDATUM                    
060800                                         DAT-KDSVAR                       
060900             IF DAT-KDSVAR-OK                                             
061000               MOVE MFS-NUM-FIELD-OK  TO MOD-DAAVROP-FOM-UPD-ATTR         
061100             ELSE                                                         
061200             MOVE MFS-NUM-FIELD-WRONG TO MOD-DAAVROP-FOM-UPD-ATTR         
061201             MOVE ERR-INVALID-DATE    TO MED-IDMFSFEL                     
061210             MOVE NOO                 TO INDATA-SW                        
062482             END-IF                                                       
062483           END-IF                                                         
062484         END-IF                                                           
062485                                                                          
062486         IF MID-DAAVROP-TOM-UPD = ALL '+'                                 
062487           MOVE MFS-NUM-FIELD-WRONG   TO MOD-DAAVROP-TOM-UPD-ATTR         
062488           MOVE NOO                   TO INDATA-SW                        
062490         ELSE                                                             
062491           IF MID-DAAVROP-TOM-UPD NOT NUMERIC                             
062492             MOVE MFS-NUM-FIELD-WRONG TO MOD-DAAVROP-TOM-UPD-ATTR         
062493             MOVE ERR-INVALID-DATE    TO MED-IDMFSFEL                     
062494             MOVE NOO                 TO INDATA-SW                        
062600           ELSE                                                           
062700             MOVE 'AAVV'              TO DAT-KDDATFORM                    
062800             MOVE MID-DAAVROP-TOM-UPD TO DAT-I-TIDATUM                    
062900             CALL WDATKONV         USING DAT-KDDATFORM                    
063000                                         DAT-I-TIDATUM                    
063100                                         DAT-O-TIDATUM                    
063200                                         DAT-KDSVAR                       
063300             IF DAT-KDSVAR-OK                                             
063400               MOVE MFS-NUM-FIELD-OK  TO MOD-DAAVROP-TOM-UPD-ATTR         
063510             ELSE                                                         
063600               MOVE MFS-NUM-FIELD-WRONG TO                                
063601                                         MOD-DAAVROP-TOM-UPD-ATTR         
063602               MOVE ERR-INVALID-DATE    TO MED-IDMFSFEL                   
063610               MOVE NOO                 TO INDATA-SW                      
063700             END-IF                                                       
063800           END-IF                                                         
063900         END-IF                                                           
064000                                                                          
064100         IF INDATA-OK                                                     
064110           MOVE MID-DAAVROP-FOM-UPD   TO WS-DAAVROP-FOM-UPD               
064120           MOVE MID-DAAVROP-TOM-UPD   TO WS-DAAVROP-TOM-UPD               
064200           IF WS-DAAVROP-FOM-UPD > WS-DAAVROP-TOM-UPD                     
064300             MOVE MFS-NUM-FIELD-WRONG TO MOD-DAAVROP-FOM-UPD-ATTR         
064400             MOVE MFS-NUM-FIELD-WRONG TO MOD-DAAVROP-TOM-UPD-ATTR         
064410             MOVE ERR-WRONG-INTERVAL  TO MED-IDMFSFEL                     
064500             MOVE NOO                 TO INDATA-SW                        
064700           ELSE                                                           
065000             IF WS-DAAVROP-FOM-YY-UPD = WS-DAAVROP-TOM-YY-UPD             
065100                COMPUTE WS-DAAVROP =                                      
065200                        WS-DAAVROP-TOM-UPD - WS-DAAVROP-FOM-UPD           
065400             ELSE                                                         
065500                COMPUTE WS-DAAVROP = 52 - WS-DAAVROP-FOM-WW-UPD           
065600                                        + WS-DAAVROP-TOM-WW-UPD           
065800             END-IF                                                       
065900            IF WS-DAAVROP > +20                                           
066100             MOVE MFS-NUM-FIELD-WRONG TO MOD-DAAVROP-FOM-UPD-ATTR         
066200             MOVE MFS-NUM-FIELD-WRONG TO MOD-DAAVROP-TOM-UPD-ATTR         
066210             MOVE ERR-WRONG-INTERVAL  TO MED-IDMFSFEL                     
066300             MOVE NOO                 TO INDATA-SW                        
066400            END-IF                                                        
066500           END-IF                                                         
066600         END-IF                                                           
066700                                                                          
066800         IF MID-IDANSK-FOM-UPD = ALL '+'                                  
066900           MOVE MFS-NUM-FIELD-WRONG   TO MOD-IDANSK-FOM-UPD-ATTR          
067000           MOVE NOO                   TO INDATA-SW                        
067200         ELSE                                                             
067300           IF MID-IDANSK-FOM-UPD NOT NUMERIC                              
067400             MOVE MFS-NUM-FIELD-WRONG TO MOD-IDANSK-FOM-UPD-ATTR          
067600             MOVE ERR-INVALID-DATE    TO MED-IDMFSFEL                     
067610             MOVE NOO                 TO INDATA-SW                        
067700           ELSE                                                           
067800             MOVE MFS-NUM-FIELD-OK    TO MOD-IDANSK-FOM-UPD-ATTR          
067900           END-IF                                                         
068000         END-IF                                                           
068100                                                                          
068200         IF MID-IDANSK-TOM-UPD = ALL '+'                                  
068300           MOVE MFS-NUM-FIELD-WRONG   TO MOD-IDANSK-TOM-UPD-ATTR          
068400           MOVE NOO                   TO INDATA-SW                        
068600         ELSE                                                             
068700           IF MID-IDANSK-TOM-UPD NOT NUMERIC                              
068800             MOVE MFS-NUM-FIELD-WRONG TO MOD-IDANSK-TOM-UPD-ATTR          
068810             MOVE ERR-INVALID-DATE    TO MED-IDMFSFEL                     
068900             MOVE NOO                 TO INDATA-SW                        
069100           ELSE                                                           
069200             MOVE MFS-NUM-FIELD-OK    TO MOD-IDANSK-TOM-UPD-ATTR          
069300           END-IF                                                         
069400         END-IF                                                           
069500                                                                          
069600         IF INDATA-OK                                                     
069800           IF MID-IDANSK-FOM-UPD > MID-IDANSK-TOM-UPD                     
069900             MOVE MFS-NUM-FIELD-WRONG TO MOD-IDANSK-FOM-UPD-ATTR          
070000             MOVE MFS-NUM-FIELD-WRONG TO MOD-IDANSK-TOM-UPD-ATTR          
070200             MOVE ERR-WRONG-INTERVAL  TO MED-IDMFSFEL                     
070210             MOVE NOO                 TO INDATA-SW                        
070300           END-IF                                                         
070400         END-IF                                                           
070500                                                                          
071000        IF MID-DAAVROP-TFOM-UPD = ALL '+'                                 
071100          MOVE MFS-NUM-FIELD-WRONG    TO MOD-DAAVROP-TFOM-UPD-ATTR        
071200          MOVE NOO                    TO INDATA-SW                        
071400        ELSE                                                              
071500          IF MID-DAAVROP-TFOM-UPD NOT NUMERIC                             
071600            MOVE MFS-NUM-FIELD-WRONG  TO MOD-DAAVROP-TFOM-UPD-ATTR        
071610            MOVE ERR-INVALID-DATE     TO MED-IDMFSFEL                     
071700            MOVE NOO                  TO INDATA-SW                        
071900          ELSE                                                            
072000            MOVE 'AAVV'               TO DAT-KDDATFORM                    
072100            MOVE MID-DAAVROP-TFOM-UPD TO DAT-I-TIDATUM                    
072200            CALL WDATKONV USING DAT-KDDATFORM                             
072300                                DAT-I-TIDATUM                             
072400                                DAT-O-TIDATUM                             
072500                                DAT-KDSVAR                                
072600            IF DAT-KDSVAR-OK                                              
072700              MOVE MID-DAAVROP-TFOM-UPD TO WS-DAAVROP-TFOM-UPD            
072710              MOVE MID-DAAVROP-FOM-UPD  TO WS-DAAVROP-FOM-UPD             
072800              IF WS-DAAVROP-TFOM-UPD  > W-DAT-TIAAVV                      
072900             AND MID-DAAVROP-TFOM-UPD < MID-DAAVROP-FOM-UPD               
073010                MOVE MFS-NUM-FIELD-OK   TO                                
073020                                        MOD-DAAVROP-TFOM-UPD-ATTR         
073030                                                                          
073060                IF WS-DAAVROP-TFOM-YY-UPD = WS-DAAVROP-FOM-YY-UPD         
073070                   COMPUTE WS-DAAVROP =                                   
073080                       WS-DAAVROP-FOM-UPD - WS-DAAVROP-TFOM-UPD           
073091                ELSE                                                      
073094                 COMPUTE WS-DAAVROP = 52 - WS-DAAVROP-TFOM-WW-UPD         
073095                                         + WS-DAAVROP-FOM-WW-UPD          
073096                END-IF                                                    
073098                IF WS-DAAVROP > +30                                       
073099                  MOVE MFS-NUM-FIELD-WRONG TO                             
073100                                         MOD-DAAVROP-TFOM-UPD-ATTR        
073101                  MOVE MFS-NUM-FIELD-WRONG TO                             
073102                                         MOD-DAAVROP-FOM-UPD-ATTR         
073103                  MOVE ERR-WRONG-INTERVAL  TO MED-IDMFSFEL                
073105                  MOVE NOO                 TO INDATA-SW                   
073106                END-IF                                                    
073110              ELSE                                                        
073200                 MOVE MFS-NUM-FIELD-WRONG  TO                             
073210                                         MOD-DAAVROP-TFOM-UPD-ATTR        
073220                 MOVE ERR-WRONG-INTERVAL   TO MED-IDMFSFEL                
073300                 MOVE NOO                  TO INDATA-SW                   
073400              END-IF                                                      
074000            ELSE                                                          
074001              MOVE MFS-NUM-FIELD-WRONG     TO                             
074002                                         MOD-DAAVROP-TFOM-UPD-ATTR        
074004              MOVE ERR-INVALID-DATE        TO MED-IDMFSFEL                
074005              MOVE NOO                     TO INDATA-SW                   
074010            END-IF                                                        
074100          END-IF                                                          
074200        END-IF                                                            
074201                                                                          
074210        IF INDATA-OK                                                      
074220           PERFORM GA-CHECK-PROC-CALLOFF-INTERVAL                         
074231        END-IF                                                            
074240                                                                          
074300       END-IF                                                             
074440                                                                          
074500       IF INDATA-WRONG                                                    
074510         IF MED-IDMFSFEL = SPACE                                          
074600            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
074620         END-IF                                                           
074700         CALL WMEDKONV            USING MED-WMEDAREA                      
074800         MOVE MED-MFSFEL             TO MOD-TEMFSFEL                      
074900         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
075000         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
076410       END-IF                                                             
076500     END-IF                                                               
076600     .                                                                    
076700     EJECT                                                                
076710 GA-CHECK-PROC-CALLOFF-INTERVAL SECTION.                                  
076900     MOVE 'GA-CHECK-PROC-CALLOFF-INTERVAL' TO CURRENT-SECTION             
077000                                                                          
077002     MOVE YES                       TO WS-DAAVROP-OK                      
077004                                                                          
077005     PERFORM GAA-WDGX2260-TAB                                             
077006                                                                          
077012     MOVE 20                       TO WS-MID-DAAVROP-FOM-CC-UPD           
077013     MOVE MID-DAAVROP-FOM-UPD      TO WS-MID-DAAVROP-FOM-YYWW-UPD         
077014                                                                          
077015     MOVE 20                       TO WS-MID-DAAVROP-TOM-CC-UPD           
077016     MOVE MID-DAAVROP-TOM-UPD      TO WS-MID-DAAVROP-TOM-YYWW-UPD         
077017                                                                          
077018     MOVE 20                       TO WS-MID-DAAVROP-TFOM-CC-UPD          
077019     MOVE MID-DAAVROP-TFOM-UPD     TO WS-MID-DAAVROP-TFOM-YYWW-UPD        
077020                                                                          
077021     MOVE MID-IDANSK-FOM-UPD       TO WS-MID-IDANSK-FOM-NUM-UPD           
077022     MOVE MID-IDANSK-TOM-UPD       TO WS-MID-IDANSK-TOM-NUM-UPD           
077023                                                                          
077024     MOVE +1                        TO TAB-IX                             
077025     MOVE +0                        TO TAB2-IX                            
077026     PERFORM UNTIL TAB-IX > MAX-TAB-IX                                    
077027                OR WS-DAAVROP-OK = NOO                                    
077042                                                                          
077063      IF (TAB-IDANSK-FOM(TAB-IX) >= WS-MID-IDANSK-FOM-UPD AND             
077064          TAB-IDANSK-FOM(TAB-IX) <= WS-MID-IDANSK-TOM-UPD)                
077065      OR (TAB-IDANSK-TOM(TAB-IX) >= WS-MID-IDANSK-FOM-UPD AND             
077066          TAB-IDANSK-TOM(TAB-IX) <= WS-MID-IDANSK-TOM-UPD)                
077067      OR (TAB-IDANSK-FOM(TAB-IX) >= WS-MID-IDANSK-FOM-UPD AND             
077068          TAB-IDANSK-TOM(TAB-IX) <= WS-MID-IDANSK-TOM-UPD)                
077071      OR (TAB-IDANSK-FOM(TAB-IX) <= WS-MID-IDANSK-FOM-UPD AND             
077072          TAB-IDANSK-TOM(TAB-IX) >= WS-MID-IDANSK-FOM-UPD)                
077073      OR (TAB-IDANSK-FOM(TAB-IX) <= WS-MID-IDANSK-TOM-UPD AND             
077074          TAB-IDANSK-TOM(TAB-IX) >= WS-MID-IDANSK-TOM-UPD)                
077075      OR (TAB-IDANSK-FOM(TAB-IX) <  WS-MID-IDANSK-FOM-UPD AND             
077076          TAB-IDANSK-TOM(TAB-IX) >  WS-MID-IDANSK-TOM-UPD)                
077077                                                                          
077078        ADD +1                        TO TAB2-IX                          
077079        MOVE TAB-IDLEVNR-SHIP(TAB-IX) TO                                  
077080                                      TAB2-IDLEVNR-SHIP (TAB2-IX)         
077081        MOVE TAB-IDANSK-FOM  (TAB-IX) TO TAB2-IDANSK-FOM(TAB2-IX)         
077082        MOVE TAB-IDANSK-TOM  (TAB-IX) TO TAB2-IDANSK-TOM(TAB2-IX)         
077083        MOVE TAB-DAAVROP-FOM (TAB-IX) TO                                  
077084                                      TAB2-DAAVROP-FOM  (TAB2-IX)         
077085        MOVE TAB-DAAVROP-TOM (TAB-IX) TO                                  
077086                                      TAB2-DAAVROP-TOM  (TAB2-IX)         
077089                                                                          
077090        IF (TAB-DAAVROP-FOM(TAB-IX) >= WS-MID-DAAVROP-FOM-UPD AND         
077091            TAB-DAAVROP-FOM(TAB-IX) <= WS-MID-DAAVROP-TOM-UPD)            
077092        OR (TAB-DAAVROP-TOM(TAB-IX) >= WS-MID-DAAVROP-FOM-UPD AND         
077093            TAB-DAAVROP-TOM(TAB-IX) <= WS-MID-DAAVROP-TOM-UPD)            
077094        OR (TAB-DAAVROP-FOM(TAB-IX) >= WS-MID-DAAVROP-FOM-UPD AND         
077095            TAB-DAAVROP-TOM(TAB-IX) <= WS-MID-DAAVROP-TOM-UPD)            
077096        OR (TAB-DAAVROP-FOM(TAB-IX) <= WS-MID-DAAVROP-FOM-UPD AND         
077097            TAB-DAAVROP-TOM(TAB-IX) >= WS-MID-DAAVROP-FOM-UPD)            
077098        OR (TAB-DAAVROP-FOM(TAB-IX) <= WS-MID-DAAVROP-TOM-UPD AND         
077099            TAB-DAAVROP-TOM(TAB-IX) >= WS-MID-DAAVROP-TOM-UPD)            
077100        OR (TAB-DAAVROP-FOM(TAB-IX) <  WS-MID-DAAVROP-FOM-UPD AND         
077101            TAB-DAAVROP-TOM(TAB-IX) >  WS-MID-DAAVROP-TOM-UPD)            
077110             MOVE NOO               TO WS-DAAVROP-OK                      
077138        END-IF                                                            
077160      END-IF                                                              
077187                                                                          
077188      ADD +1                        TO TAB-IX                             
077189                                                                          
077190     END-PERFORM                                                          
077191                                                                          
077198     IF WS-DAAVROP-OK = NOO                                               
077199        MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDANSK-FOM-UPD-ATTR            
077200                                       MOD-IDANSK-TOM-UPD-ATTR            
077201        MOVE MFS-NUM-FIELD-WRONG    TO MOD-DAAVROP-FOM-UPD-ATTR           
077202                                       MOD-DAAVROP-TOM-UPD-ATTR           
077203        MOVE ERR-LINE-EXIST         TO MED-IDMFSFEL                       
077204        MOVE NOO                    TO INDATA-SW                          
077205     ELSE                                                                 
077206        PERFORM GAB-CHECK-DAAVROP-TFOM                                    
077207        IF WS-DAAVROP-TFOM-OK = NOO                                       
077208           MOVE MFS-NUM-FIELD-WRONG TO MOD-DAAVROP-TFOM-UPD-ATTR          
077209           MOVE ERR-LINE-EXIST      TO MED-IDMFSFEL                       
077210          MOVE NOO                  TO INDATA-SW                          
077211        END-IF                                                            
077213     END-IF                                                               
077214     .                                                                    
077220     EJECT                                                                
077300 GAA-WDGX2260-TAB SECTION.                                                
077310     MOVE 'GAA-WDGX2260-TAB         ' TO CURRENT-SECTION                  
077311                                                                          
077312     MOVE +1                          TO TAB-IX                           
077321     PERFORM UNTIL TAB-IX > MAX-TAB-IX                                    
077322       MOVE SPACE             TO TAB-IDLEVNR-SHIP     (TAB-IX)            
077323       MOVE ZERO              TO TAB-IDANSK-FOM       (TAB-IX)            
077324                                 TAB-IDANSK-TOM       (TAB-IX)            
077325                                 TAB-DAAVROP-FOM      (TAB-IX)            
077326                                 TAB-DAAVROP-TOM      (TAB-IX)            
077327                                 TAB-DAAVROP-TFOM     (TAB-IX)            
077328                                                                          
077329       MOVE SPACE             TO TAB-IDLEVNR-SHIP-SORT(TAB-IX)            
077330       MOVE ZERO              TO TAB-IDANSK-FOM-SORT  (TAB-IX)            
077331                                 TAB-IDANSK-TOM-SORT  (TAB-IX)            
077332                                 TAB-DAAVROP-FOM-SORT (TAB-IX)            
077333                                 TAB-DAAVROP-TOM-SORT (TAB-IX)            
077335       ADD +1                 TO TAB-IX                                   
077336     END-PERFORM                                                          
077337                                                                          
077338     MOVE +1                          TO TAB-IX                           
077339                                                                          
077340     MOVE MID-IDLEVNR-SHIP-UPD        TO W-IDLEVNR-SHIP                   
077341     PERFORM IMS-GU-WDGX2258                                              
077342     IF SEGMENT-FOUND                                                     
077350        PERFORM IMS-GNP-WDGX2260                                          
077351        PERFORM UNTIL SEGMENT-MISSING                                     
077352                   OR TAB-IX > MAX-TAB-IX                                 
077361          MOVE 2258-IDLEVNR-SHIP TO TAB-IDLEVNR-SHIP     (TAB-IX)         
077362          MOVE 2260-IDANSK-FOM   TO TAB-IDANSK-FOM       (TAB-IX)         
077363          MOVE 2260-IDANSK-TOM   TO TAB-IDANSK-TOM       (TAB-IX)         
077365          MOVE 2260-DAAVROP-FOM  TO TAB-DAAVROP-FOM      (TAB-IX)         
077366          MOVE 2260-DAAVROP-TOM  TO TAB-DAAVROP-TOM      (TAB-IX)         
077367          MOVE 2260-DAAVROP-TFOM TO TAB-DAAVROP-TFOM     (TAB-IX)         
077372                                                                          
077373          MOVE 2258-IDLEVNR-SHIP TO TAB-IDLEVNR-SHIP-SORT(TAB-IX)         
077374          MOVE 2260-IDANSK-FOM   TO TAB-IDANSK-FOM-SORT  (TAB-IX)         
077375          MOVE 2260-IDANSK-TOM   TO TAB-IDANSK-TOM-SORT  (TAB-IX)         
077376          MOVE 2260-DAAVROP-FOM  TO TAB-DAAVROP-FOM-SORT (TAB-IX)         
077377          MOVE 2260-DAAVROP-TOM  TO TAB-DAAVROP-TOM-SORT (TAB-IX)         
077378          PERFORM IMS-GNP-WDGX2260                                        
077379          ADD +1                 TO TAB-IX                                
077380        END-PERFORM                                                       
077390     END-IF                                                               
077391                                                                          
077392     MOVE TAB-IX                 TO MAX-TAB-IX                            
077393                                                                          
077410     COMPUTE ANTAL = TAB-IX - 1                                           
077412                                                                          
077414     CALL WINTSOR USING TAB-WDGX2260 STEGLANGD ANTAL                      
077415                  TAB-SORT (1) NYCKELLANGD                                
077416                                                                          
077417     MOVE +1                          TO TAB2-IX                          
077418     PERFORM UNTIL TAB2-IX > MAX-TAB2-IX                                  
077419       MOVE SPACE             TO TAB2-IDLEVNR-SHIP     (TAB2-IX)          
077420       MOVE ZERO              TO TAB2-IDANSK-FOM       (TAB2-IX)          
077421                                 TAB2-IDANSK-TOM       (TAB2-IX)          
077422                                 TAB2-DAAVROP-FOM      (TAB2-IX)          
077423                                 TAB2-DAAVROP-TOM      (TAB2-IX)          
077431       ADD +1                 TO TAB2-IX                                  
077432     END-PERFORM                                                          
077440     .                                                                    
077500     EJECT                                                                
077600 GAB-CHECK-DAAVROP-TFOM SECTION.                                          
077700     MOVE 'GAB-CHECK-DAAVROP-TFOM       ' TO CURRENT-SECTION              
080809                                                                          
080810     MOVE YES                         TO WS-DAAVROP-TFOM-OK               
080811                                                                          
080812     MOVE TAB2-IX                     TO MAX-TAB2-IX                      
080813     MOVE +1                          TO TAB2-IX                          
080814                                                                          
080815     PERFORM UNTIL TAB2-IX > MAX-TAB2-IX                                  
080816                                                                          
080847       IF WS-MID-DAAVROP-FOM-UPD > TAB2-DAAVROP-FOM (TAB2-IX)             
080851          IF WS-MID-DAAVROP-TFOM-UPD > TAB2-DAAVROP-TOM(TAB2-IX)          
080857             CONTINUE                                                     
080900          ELSE                                                            
080901             MOVE NOO                 TO WS-DAAVROP-TFOM-OK               
080903          END-IF                                                          
080904       ELSE                                                               
080905          MOVE MAX-TAB2-IX            TO TAB2-IX                          
080906       END-IF                                                             
080907                                                                          
080908       ADD +1                         TO TAB2-IX                          
080909                                                                          
080911     END-PERFORM                                                          
080913                                                                          
080920     .                                                                    
081000     EJECT                                                                
081315 H-UPDATE SECTION.                                                        
081320     MOVE 'H-UPDATE                     ' TO CURRENT-SECTION              
081400                                                                          
081700     MOVE +0                         TO IX-PARM                           
081800     MOVE +1                         TO INDX                              
081900     PERFORM UNTIL INDX > MAX-INDX                                        
082000       IF MID-KDCMD-UPD (INDX) NOT = ALL '+'                              
082100         MOVE MID-IDLEVNR-SHIP(INDX) TO W-IDLEVNR-SHIP                    
082200         MOVE 20                     TO WS-DAAVROP-CC                     
082300         MOVE MID-DAAVROP-FOM (INDX) TO WS-DAAVROP-YYWW                   
082400         MOVE WS-DAAVROP             TO W-DAAVROP-FOM                     
082500         MOVE MID-DAAVROP-TOM (INDX) TO WS-DAAVROP-YYWW                   
082600         MOVE WS-DAAVROP             TO W-DAAVROP-TOM                     
082700         MOVE MID-IDANSK-FOM  (INDX) TO W-IDANSK-FOM                      
082800         MOVE MID-IDANSK-TOM  (INDX) TO W-IDANSK-TOM                      
082900         PERFORM IMS-GHU-WDGX2260                                         
083000         IF SEGMENT-FOUND                                                 
083100            PERFORM IMS-DLET-WDGX2260                                     
083200                                                                          
083300            ADD  +1                  TO IX-PARM                           
083400            MOVE W-IDLEVNR-SHIP      TO PARM-IDLEVNR-SHIP(IX-PARM)        
083500            MOVE 2260-DAAVROP-FOM    TO PARM-DAAVROP-FOM (IX-PARM)        
083600            MOVE 2260-DAAVROP-TOM    TO PARM-DAAVROP-TOM (IX-PARM)        
083700            MOVE 2260-IDANSK-FOM     TO PARM-IDANSK-FOM  (IX-PARM)        
083800            MOVE 2260-IDANSK-TOM     TO PARM-IDANSK-TOM  (IX-PARM)        
084000                                                                          
084002            PERFORM IMS-GU-WDGX2258                                       
084003            PERFORM IMS-GNP-WDGX2260                                      
084004            IF SEGMENT-MISSING                                            
084005               PERFORM IMS-GHU-WDGX2258                                   
084006               PERFORM IMS-DLET-WDGX2258                                  
084007            END-IF                                                        
084100         END-IF                                                           
084200       END-IF                                                             
084300       ADD +1                        TO INDX                              
084400     END-PERFORM                                                          
084500                                                                          
085900     IF MID-IDLEVNR-SHIP-UPD NOT = ALL '+'                                
086000        MOVE MID-IDLEVNR-SHIP-UPD    TO W-IDLEVNR-SHIP                    
086100        PERFORM IMS-GHU-WDGX2258                                          
086200        IF SEGMENT-MISSING                                                
086300           MOVE MID-IDLEVNR-SHIP-UPD  TO 2258-IDLEVNR-SHIP                
086400           PERFORM IMS-ISRT-WDGX2258                                      
086500        END-IF                                                            
086600                                                                          
086700        MOVE 20                       TO WS-DAAVROP-CC                    
086800        MOVE MID-DAAVROP-FOM-UPD      TO WS-DAAVROP-YYWW                  
086900        MOVE WS-DAAVROP               TO 2260-DAAVROP-FOM                 
086910                                         W-DAAVROP-FOM                    
087000        MOVE MID-DAAVROP-TOM-UPD      TO WS-DAAVROP-YYWW                  
087100        MOVE WS-DAAVROP               TO 2260-DAAVROP-TOM                 
087110                                         W-DAAVROP-TOM                    
087200        MOVE MID-IDANSK-FOM-UPD       TO 2260-IDANSK-FOM                  
087210                                         W-IDANSK-FOM                     
087300        MOVE MID-IDANSK-TOM-UPD       TO 2260-IDANSK-TOM                  
087310                                         W-IDANSK-TOM                     
087400        MOVE MID-DAAVROP-TFOM-UPD     TO WS-DAAVROP-YYWW                  
087500        MOVE WS-DAAVROP               TO 2260-DAAVROP-TFOM                
087600                                                                          
087800        PERFORM IMS-ISRT-WDGX2260                                         
087900        IF SEGMENT-FOUND-EXISTS                                           
088000           CONTINUE                                                       
088100        ELSE                                                              
088200           ADD  +1                 TO IX-PARM                             
088300           MOVE 2258-IDLEVNR-SHIP  TO PARM-IDLEVNR-SHIP(IX-PARM)          
088400           MOVE 2260-DAAVROP-FOM   TO PARM-DAAVROP-FOM (IX-PARM)          
088500           MOVE 2260-DAAVROP-TOM   TO PARM-DAAVROP-TOM (IX-PARM)          
088600           MOVE 2260-IDANSK-FOM    TO PARM-IDANSK-FOM  (IX-PARM)          
088700           MOVE 2260-IDANSK-TOM    TO PARM-IDANSK-TOM  (IX-PARM)          
089000        END-IF                                                            
089010     END-IF                                                               
089100                                                                          
089110     IF IX-PARM > +0                                                      
089120        PERFORM HA-START-ROUTINE-W221S3                                   
089130     END-IF                                                               
089140                                                                          
089200     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
089300     CALL WMEDKONV USING MED-WMEDAREA                                     
089400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
089500     PERFORM MFS-FORM-ATTR                                                
089600     PERFORM MFS-ERASE-FIELD-IN                                           
089700* * * MFS-DO-NOT-TOUCH-FIELD TO LOCKED VALUES                             
089800     .                                                                    
089900     EJECT                                                                
090000 HA-START-ROUTINE-W221S3 SECTION.                                         
090100     MOVE 'HA-START-ROUTINE-W221S3      ' TO CURRENT-SECTION              
090200                                                                          
090300     PERFORM UNTIL IX-PARM = +10                                          
090400        ADD  +1                 TO IX-PARM                                
090500        MOVE SPACE              TO PARM-IDLEVNR-SHIP(IX-PARM)             
090600        MOVE ZERO               TO PARM-DAAVROP-FOM (IX-PARM)             
090700        MOVE ZERO               TO PARM-DAAVROP-TOM (IX-PARM)             
090800        MOVE ZERO               TO PARM-IDANSK-FOM  (IX-PARM)             
090900        MOVE ZERO               TO PARM-IDANSK-TOM  (IX-PARM)             
091000     END-PERFORM                                                          
091100                                                                          
091200     MOVE '2149'   TO MSGSOP-IDTRANS                                      
091300     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
091400     MOVE 'W221S3' TO MSGSOP-IDPROCESS                                    
091500     MOVE 'A'      TO MSGSOP-KDSOPFUNK                                    
091600                                                                          
091700     STRING 'AVROP01(' PARM-W221S3(01)                                    
091800            ')AVROP02(' PARM-W221S3(02)                                   
091900            ')AVROP03(' PARM-W221S3(03)                                   
092000            ')AVROP04(' PARM-W221S3(04)                                   
092100            ')AVROP05(' PARM-W221S3(05)                                   
092200            ')AVROP06(' PARM-W221S3(06)                                   
092300            ')AVROP07(' PARM-W221S3(07)                                   
092400            ')AVROP08(' PARM-W221S3(08)                                   
092500            ')AVROP09(' PARM-W221S3(08)                                   
092600            ')AVROP10(' PARM-W221S3(10) ')'                               
092700             DELIMITED BY SIZE INTO MSGSOP-TESYMBV                        
092800                                                                          
092900     PERFORM IMS-INSERT-ALTMSG                                            
093200     .                                                                    
093300     EJECT                                                                
093400 MFS-ERASE-FIELD-OUT SECTION.                                             
093500                                                                          
093600*    --- ALLA UTDATA-FÄLT                                                 
093700*    --- INCL. SCROLL KEYS                                                
093800     MOVE MFS-ERASE-FIELD          TO MOD-IDLEVNR-SHIP-UPD                
093900                                      MOD-DAAVROP-FOM-UPD                 
094000                                      MOD-DAAVROP-TOM-UPD                 
094100                                      MOD-IDANSK-FOM-UPD                  
094200                                      MOD-IDANSK-TOM-UPD                  
094300                                      MOD-DAAVROP-TFOM-UPD                
094400                                                                          
094500*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
094600     MOVE +1                       TO INDX                                
094700     PERFORM UNTIL INDX > MAX-INDX                                        
094800       MOVE MFS-ERASE-FIELD        TO MOD-KDCMD-UPD   (INDX)              
094900                                      MOD-IDANSK-FOM  (INDX)              
095000                                      MOD-IDANSK-TOM  (INDX)              
095100                                      MOD-DAAVROP-TOM (INDX)              
095200                                      MOD-DAAVROP-FOM (INDX)              
095300       ADD +1                      TO INDX                                
095400     END-PERFORM                                                          
095500     .                                                                    
095600     SKIP3                                                                
095700 MFS-ERASE-FIELD-IN SECTION.                                              
095800                                                                          
095900*    --- ALLA INDATA-FÄLT                                                 
096000     MOVE MFS-ERASE-FIELD          TO MOD-IDLEVNR-SHIP-UPD                
096100                                      MOD-DAAVROP-FOM-UPD                 
096200                                      MOD-DAAVROP-TOM-UPD                 
096300                                      MOD-IDANSK-FOM-UPD                  
096400                                      MOD-IDANSK-TOM-UPD                  
096500                                      MOD-DAAVROP-TFOM-UPD                
096600                                                                          
096700     MOVE +1 TO INDX                                                      
096800     PERFORM UNTIL INDX > MAX-INDX                                        
096900       MOVE MFS-ERASE-FIELD        TO MOD-KDCMD-UPD   (INDX)              
096910                                      MOD-IDANSK-FOM  (INDX)              
096920                                      MOD-IDANSK-TOM  (INDX)              
096930                                      MOD-DAAVROP-TOM (INDX)              
096940                                      MOD-DAAVROP-FOM (INDX)              
097000       ADD +1                      TO INDX                                
097100     END-PERFORM                                                          
097200     .                                                                    
097300     EJECT                                                                
097400 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
097500                                                                          
097600*    --- ALLA UTDATA-FÄLT                                                 
097700*    --- INCL SCROLL KEYS AND LINEDATA                                    
097800     MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDLEVNR-SHIP-UPD                
097900                                      MOD-DAAVROP-FOM-UPD                 
098000                                      MOD-DAAVROP-TOM-UPD                 
098100                                      MOD-IDANSK-FOM-UPD                  
098200                                      MOD-IDANSK-TOM-UPD                  
098300                                      MOD-DAAVROP-TFOM-UPD                
098400                                                                          
098500*    --- OUTDATA FIELD ON SCROLL KEYS                                     
098600     MOVE +1 TO INDX                                                      
098700     PERFORM UNTIL INDX > MAX-INDX                                        
098800       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMD-UPD   (INDX)              
098900                                      MOD-IDANSK-FOM  (INDX)              
099000                                      MOD-IDANSK-TOM  (INDX)              
099100                                      MOD-DAAVROP-TOM (INDX)              
099200                                      MOD-DAAVROP-FOM (INDX)              
099300       ADD +1 TO INDX                                                     
099400     END-PERFORM                                                          
099500     .                                                                    
099600                                                                          
099700 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
099800                                                                          
099900*    --- ALLA INDATA-FÄLT                                                 
100000     MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDLEVNR-SHIP-UPD                
100100                                      MOD-DAAVROP-FOM-UPD                 
100200                                      MOD-DAAVROP-TOM-UPD                 
100300                                      MOD-IDANSK-FOM-UPD                  
100400                                      MOD-IDANSK-TOM-UPD                  
100500                                      MOD-DAAVROP-TFOM-UPD                
100600     MOVE +1 TO INDX                                                      
100700     PERFORM UNTIL INDX > MAX-INDX                                        
100800      MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-KDCMD-UPD   (INDX)             
100810                                       MOD-IDANSK-FOM  (INDX)             
100820                                       MOD-IDANSK-TOM  (INDX)             
100830                                       MOD-DAAVROP-TOM (INDX)             
100840                                       MOD-DAAVROP-FOM (INDX)             
100900      ADD +1                        TO INDX                               
101000     END-PERFORM                                                          
101100     .                                                                    
101200     EJECT                                                                
101300 MFS-FORM-ATTR SECTION.                                                   
101400                                                                          
101500*    --- ALL INDATA-FIELDS                                                
101600     MOVE MFS-FORMAT-DEFAULT-ATTR   TO MOD-IDLEVNR-SHIP-UPD-ATTR          
101700                                       MOD-DAAVROP-FOM-UPD-ATTR           
101800                                       MOD-DAAVROP-TOM-UPD-ATTR           
101900                                       MOD-IDANSK-FOM-UPD-ATTR            
102000                                       MOD-IDANSK-TOM-UPD-ATTR            
102100                                       MOD-DAAVROP-TFOM-UPD-ATTR          
102200     MOVE +1 TO INDX                                                      
102300     PERFORM UNTIL INDX > MAX-INDX                                        
102400      MOVE MFS-FORMAT-DEFAULT-ATTR   TO MOD-KDCMD-UPD-ATTR (INDX)         
102500      ADD +1                         TO INDX                              
102600     END-PERFORM                                                          
102700     .                                                                    
102800     SKIP2                                                                
102900 MFS-READ-IN-AGAIN SECTION.                                               
103000                                                                          
103100*    --- ALL INDATA-FIELDS                                                
103200     MOVE MFS-ADD-READ-FIELD         TO MOD-IDLEVNR-SHIP-UPD-ATTR         
103300                                        MOD-DAAVROP-FOM-UPD-ATTR          
103400                                        MOD-DAAVROP-TOM-UPD-ATTR          
103500                                        MOD-IDANSK-FOM-UPD-ATTR           
103600                                        MOD-IDANSK-TOM-UPD-ATTR           
103700                                        MOD-DAAVROP-TFOM-UPD-ATTR         
103800                                                                          
103900     MOVE +1                         TO INDX                              
104000     PERFORM UNTIL INDX > MAX-INDX                                        
104100      MOVE MFS-ADD-READ-FIELD        TO MOD-KDCMD-UPD-ATTR (INDX)         
104200      ADD +1                         TO INDX                              
104300     END-PERFORM                                                          
104400     .                                                                    
104500     EJECT                                                                
104600* --- IMS SECTIONS ---                                                    
104700     SKIP3                                                                
104800 IMS-GET-MSG SECTION.                                                     
104900                                                                          
105000     MOVE '  QC' TO GOOD-STATUSCODES                                      
105100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
105200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
105300     PERFORM IMS-STATUSCHECK                                              
105400     .                                                                    
105500     SKIP3                                                                
105600 IMS-INSERT-MSG SECTION.                                                  
105700                                                                          
105800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
105900     MOVE SPACE TO GOOD-STATUSCODES                                       
106000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
106100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106200     PERFORM IMS-STATUSCHECK                                              
106300     .                                                                    
106400                                                                          
106500 IMS-INSERT-ALTMSG SECTION.                                               
106600                                                                          
106700     MOVE SPACE TO GOOD-STATUSCODES                                       
106800     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
106900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
107000     PERFORM IMS-STATUSCHECK                                              
107100     .                                                                    
107101     EJECT                                                                
107102                                                                          
107110 IMS-GU-WDF101    SECTION.                                                
107111     MOVE 'IMS-GU-WDF101                ' TO IMS-SECTION                  
107120                                                                          
107130     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
107140             DELIMITED BY SIZE INTO SSA1                                  
107150     MOVE '  GE'                 TO GOOD-STATUSCODES                      
107160     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
107191     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
107192     PERFORM IMS-STATUSCHECK                                              
107194     .                                                                    
107195                                                                          
107200 IMS-GU-WDGX2257 SECTION.                                                 
107300     MOVE 'IMS-GU-WDGX2257              ' TO IMS-SECTION                  
107400                                                                          
107500     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2257-X ')'                    
107600            DELIMITED BY SIZE INTO SSA1                                   
107700     MOVE '    '                TO GOOD-STATUSCODES                       
107800     CALL CBLTDLI USING GU WDG3-PCB DLI-IO-WDGX01 SSA1                    
107900     MOVE WDG3-STATUS-CODE      TO STATUS-WS                              
108000     PERFORM IMS-STATUSCHECK                                              
108100     .                                                                    
108200                                                                          
108300 IMS-GNP-WDGX2258 SECTION.                                                
108400     MOVE 'IMS-GNP-WDGX2258             ' TO IMS-SECTION                  
108500                                                                          
108600     STRING 'WDGX2258(IDLEVNRS=>' W-WDGX2258-X ')'                        
108700          DELIMITED BY SIZE   INTO SSA1                                   
108800     MOVE '  GE'                TO GOOD-STATUSCODES                       
108900     CALL CBLTDLI USING GNP WDG3-PCB DLI-IO-WDGX2258 SSA1                 
109000     MOVE WDG3-STATUS-CODE      TO STATUS-WS                              
109100     PERFORM IMS-STATUSCHECK                                              
109200     .                                                                    
110601                                                                          
110602 IMS-GNP-WDGX2258-WDGX2260 SECTION.                                       
110603     MOVE 'IMS-GNP-WDGX2258-WDGX2260    ' TO IMS-SECTION                  
110605                                                                          
110606     STRING 'WDGX2258(IDLEVNRS =' W-WDGX2258-X ')'                        
110607          DELIMITED BY SIZE  INTO SSA1                                    
110608     STRING 'WDGX2260(DAAVROPF=>' W-DAAVROP-FOM-X                         
110609                    '&DAAVROPT=>' W-DAAVROP-TOM-X                         
110610                    '&IDANSKF =>' W-IDANSK-FOM-X                          
110611                    '&IDANSKT =>' W-IDANSK-TOM-X ')'                      
110612          DELIMITED BY SIZE  INTO SSA2                                    
110613     MOVE '  GE'               TO GOOD-STATUSCODES                        
110614     CALL CBLTDLI USING GNP WDG3-PCB DLI-IO-WDGX2260 SSA1                 
110615                                                     SSA2                 
110616     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
110617     PERFORM IMS-STATUSCHECK                                              
110618     .                                                                    
110619     EJECT                                                                
110620 IMS-GNP-WDGX2260  SECTION.                                               
110621     MOVE 'IMS-GNP-WDGX2260             ' TO IMS-SECTION                  
110622                                                                          
110623     MOVE 'WDGX2260 '          TO SSA1                                    
110624     MOVE '  GE'               TO GOOD-STATUSCODES                        
110625     CALL CBLTDLI USING GNP WDG3-PCB DLI-IO-WDGX2260 SSA1                 
110626     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
110627     PERFORM IMS-STATUSCHECK                                              
110628     .                                                                    
110629     EJECT                                                                
110630 IMS-GU-WDGX2258 SECTION.                                                 
110631     MOVE 'IMS-GU-WDGX2258             ' TO IMS-SECTION                   
110632                                                                          
110640     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2257-X ')'                    
110650          DELIMITED BY SIZE  INTO SSA1                                    
110660     STRING 'WDGX2258(IDLEVNRS =' W-WDGX2258-X ')'                        
110670          DELIMITED BY SIZE  INTO SSA2                                    
110680     MOVE '  GE'               TO GOOD-STATUSCODES                        
110690     CALL CBLTDLI USING GU WDG3-PCB DLI-IO-WDGX2258 SSA1 SSA2             
110691     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
110692     PERFORM IMS-STATUSCHECK                                              
110693     .                                                                    
110753                                                                          
112000 IMS-GHU-WDGX2258 SECTION.                                                
112100     MOVE 'IMS-GHU-WDGX2258             ' TO IMS-SECTION                  
112200                                                                          
112300     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2257-X ')'                    
112400          DELIMITED BY SIZE  INTO SSA1                                    
112500     STRING 'WDGX2258(IDLEVNRS =' W-WDGX2258-X ')'                        
112600          DELIMITED BY SIZE  INTO SSA2                                    
112700     MOVE '  GE'               TO GOOD-STATUSCODES                        
112800     CALL CBLTDLI USING GHU WDG3-PCB DLI-IO-WDGX2258 SSA1 SSA2            
112900     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
113000     PERFORM IMS-STATUSCHECK                                              
113100     .                                                                    
113200                                                                          
113300 IMS-GHU-WDGX2260 SECTION.                                                
113400     MOVE 'IMS-GHU-WDGX2260             ' TO IMS-SECTION                  
113500                                                                          
113600     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2257-X ')'                    
113700          DELIMITED BY SIZE  INTO SSA1                                    
113800     STRING 'WDGX2258(IDLEVNRS =' W-WDGX2258-X ')'                        
113900          DELIMITED BY SIZE  INTO SSA2                                    
114000     STRING 'WDGX2260(DAAVROPF =' W-DAAVROP-FOM-X                         
114100                    '&DAAVROPT =' W-DAAVROP-TOM-X                         
114200                    '&IDANSKF  =' W-IDANSK-FOM-X                          
114300                    '&IDANSKT  =' W-IDANSK-TOM-X ')'                      
114400          DELIMITED BY SIZE  INTO SSA3                                    
114500     MOVE '  GE'               TO GOOD-STATUSCODES                        
114600     CALL CBLTDLI USING GHU WDG3-PCB DLI-IO-WDGX2260 SSA1                 
114700                                                     SSA2                 
114800                                                     SSA3                 
114900     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
115000     PERFORM IMS-STATUSCHECK                                              
115100     .                                                                    
115200                                                                          
115300 IMS-ISRT-WDGX2258 SECTION.                                               
115400     MOVE 'IMS-ISRT-WDGX2258            ' TO IMS-SECTION                  
115500                                                                          
115600     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2257-X ')'                    
115700          DELIMITED BY SIZE  INTO SSA1                                    
115800     MOVE 'WDGX2258 '          TO SSA2                                    
115900     MOVE '  II'               TO GOOD-STATUSCODES                        
116000     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2258 SSA1 SSA2           
116100     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
116200     PERFORM IMS-STATUSCHECK                                              
116300     .                                                                    
116400                                                                          
116500 IMS-ISRT-WDGX2260 SECTION.                                               
116600     MOVE 'IMS-ISRT-WDGX2260            ' TO IMS-SECTION                  
116700                                                                          
116800     STRING 'WDG301  (WDG3KEY  =' W-WDGXKEY-2257-X ')'                    
116900          DELIMITED BY SIZE  INTO SSA1                                    
117000     STRING 'WDGX2258(IDLEVNRS =' W-WDGX2258-X ')'                        
117100          DELIMITED BY SIZE  INTO SSA2                                    
117200     MOVE 'WDGX2260 '          TO SSA3                                    
117300     MOVE '  II'               TO GOOD-STATUSCODES                        
117400     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2260 SSA1                
117500                                                      SSA2                
117600                                                      SSA3                
117700     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
117800     PERFORM IMS-STATUSCHECK                                              
117900     .                                                                    
118000     SKIP3                                                                
118100 IMS-DLET-WDGX2258 SECTION.                                               
118200     MOVE 'IMS-DLET-WDGX2258            ' TO IMS-SECTION                  
118300                                                                          
118400     MOVE '  '                 TO GOOD-STATUSCODES                        
118500     CALL CBLTDLI USING DLET WDG3-PCB DLI-IO-WDGX2258                     
118600     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
118700     PERFORM IMS-STATUSCHECK                                              
118800     .                                                                    
118900                                                                          
119000 IMS-DLET-WDGX2260 SECTION.                                               
119100     MOVE 'IMS-DLET-WDGX2260            ' TO IMS-SECTION                  
119200                                                                          
119300     MOVE '  '                 TO GOOD-STATUSCODES                        
119400     CALL CBLTDLI USING DLET WDG3-PCB DLI-IO-WDGX2260                     
119500     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
119600     PERFORM IMS-STATUSCHECK                                              
119700     .                                                                    
119800                                                                          
119900     EJECT                                                                
120000 IMS-STATUSCHECK SECTION.                                                 
120100                                                                          
120200     SET STATUS-IX TO 1                                                   
120300     SEARCH GOOD-STATUS                                                   
120400       AT END                                                             
120500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
120600         DELIMITED BY SIZE INTO ERROR-TEXT                                
120700         CALL FELLOG                                                      
120800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
120900         CONTINUE                                                         
121000     END-SEARCH                                                           
121100     .                                                                    
121110                                                                          
