000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4265500.                                                
000400 AUTHOR.         KENT JEBSEN                                              
000500 DATE-WRITTEN.   SEPTEMBER 2000.                                          
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        LÄSER EN FIL OCH SKRIVER KREDITNOT TILL D&P                      
001100*                                                                         
001200*        PROGRAMMET LÄSER   WLLEVA  (WDF1)                                
001300*        PROGRAMMET LÄSER   W6KVAE  (W6H7)                                
001400*        PROGRAMMET LÄSER   WLBENA  (WDD3)                                
001500*        PROGRAMMET LÄSER   WLXXKA  (WDR1)                                
001510*        PROGRAMMET LÄSER   WDL611  (WDL6)                                
001600*                                                                         
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000*                                                                         
002100 FILE-CONTROL.                                                            
002200*                                                                         
002300*          --- INFIL FAKTURANUMMER                                        
002400     SELECT W42691               ASSIGN TO W42655D1.                      
002410*          --- OUTPUT FILE                                                
002420     SELECT W42655               ASSIGN TO W42655D2.                      
002500*                                                                         
002600 DATA DIVISION.                                                           
002700 SKIP2                                                                    
002800 FILE SECTION.                                                            
002900 SKIP2                                                                    
003000 FD  W42691                                                               
003100     LABEL RECORD STANDARD                                                
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003440                                                                          
003500*01  INXE-POST  -COPY W426KRK -L                                          
003600 SKIP2                                                                    
003700*                                                                         
003720 FD  W42655                                                               
003730     RECORDING       V                                                    
003740     BLOCK CONTAINS  0.                                                   
003750                                                                          
003760 01  W42655-001                  PIC X(3000).                             
003800 EJECT                                                                    
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200*                                                                         
004300 77  IDPGM                       PIC X(08)   VALUE 'W4265500'.            
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004800 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
004900 77  STYR                        PIC X(1)   VALUE SPACE.                  
005000                                                                          
005100 77  WS-TOT-EXKL-MOMS            PIC 9(9)V9(2)  VALUE ZERO.               
005200 77  WS-TOT-INKL-MOMS            PIC 9(9)V9(2)  VALUE ZERO.               
005300 77  WS-SUOMK                    PIC 9(7)V9(2)  VALUE ZERO.               
005400 77  WS-SUMAT                    PIC 9(9)V9(2)  VALUE ZERO.               
005500 77  WS-SUOMK-SEK                PIC 9(7)V9(2)  VALUE ZERO.               
005600 77  WS-SUMAT-SEK                PIC 9(9)V9(2)  VALUE ZERO.               
005700 77  WS-IDARTNR                  PIC 9(9)       VALUE ZERO.               
005800 77  WS-TIKRED                   PIC 9(6)       VALUE ZERO.               
005900 77  WS-IDLANDX2                 PIC X(2)       VALUE SPACE.              
006000 77  WS-TOT-SEK                  PIC 9(9)V9(2)  VALUE ZERO.               
006100 77  MOMS-SATS                   PIC 9(2)V9(2)  VALUE 25.00.              
006200 77  MOMS-SATS-CN-1              PIC 9(2)V9(2)  VALUE 17.00.              
006201 77  MOMS-SATS-CN-2              PIC 9(2)V9(2)  VALUE 16.00.              
006202 77  MOMS-SATS-CN-3              PIC 9(2)V9(2)  VALUE 13.00.              
006210 77  MOMS-SATS-US                PIC 9(2)V9(2)  VALUE ZERO.               
006300                                                                          
006310 77  WS-MATCH-SW                 PIC X       VALUE 'N'.                   
006320                                                                          
006400 77  W42691-EOF-SW               PIC X       VALUE 'N'.                   
006500     88  END-OF-W42691                       VALUE 'J'.                   
006600                                                                          
007000                                                                          
007100     EJECT                                                                
007200                                                                          
007300 01  CURRENT-DATE.                                                        
007400     03  CURRENT-YY              PIC 9(2).                                
007500     03  CURRENT-MM              PIC 9(2).                                
007600     03  CURRENT-DD              PIC 9(2).                                
007700 01  DAGENS-DATUM REDEFINES CURRENT-DATE   PIC 9(6).                      
007800                                                                          
007900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008000 01  GENERELLA-SUBPROGRAM.                                                
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
008400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
008500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008700 SKIP2                                                                    
008800 01  ERRTEXT.                                                             
008900     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
009000     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
009100 01  KDRC-DISPLAY                PIC Z(5).                                
009200     EJECT                                                                
009300                                                                          
009400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009700     EJECT                                                                
009800                                                                          
010400*    --- PARAMETRAR TILL POSTSUM                                          
010500*                                                                         
010600*01  -COPY W0005    -PRE  POSTSUM-                                        
010700     EJECT                                                                
010800                                                                          
010900*01  -COPY W475CONS                                                       
011000     EJECT                                                                
011100                                                                          
011200*01  -COPY WWLEV02                                                        
011300     EJECT                                                                
011400                                                                          
011500 01  INXE-AREA-START             PIC X(24)   VALUE                        
011600                                 'INXE-AREA-START '.                      
011700     SKIP2                                                                
011800*                                                                         
011900*01  AREA -COPY W426KRK     -PRE INXE-                                    
012000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012300                                                                          
012400 01  NYCKLAR-TILL-DLI.                                                    
012500                                                                          
012510*----> DIREKTNYCKEL TILL WDL601                                           
012520                                                                          
012530     03  W-WDL601-IDARTNR-X.                                              
012540         05  W-WDL601-IDARTNR    PIC S9(9) COMP-3.                        
012550*----> DIREKTNYCKEL TILL WDL611                                           
012560                                                                          
012570     03  W-WDL611-DAINLEV-X.                                              
012580         05  W-WDL611-DAINLEV    PIC 9(16)  VALUE ZERO.                   
012600                                                                          
012700*----> DIREKTNYCKEL TILL WDD301                                           
012800                                                                          
012900     03  W-IDARTNR-X.                                                     
013000         05  W-IDARTNR           PIC S9(9) COMP-3.                        
013100                                                                          
013200*----> DIREKTNYCKEL TILL WDD311                                           
013300                                                                          
013400     03  W-IDSKYLT-X.                                                     
013500         05  W-IDSKYLT           PIC  X(3) VALUE 'GB '.                   
013600                                                                          
013700*----> DIREKTNYCKEL TILL WDF101                                           
013800                                                                          
013900     03  W-IDLEVNR-X.                                                     
014000         05  W-IDLEVNR           PIC X(5).                                
014100                                                                          
014200*----> DIREKTNYCKEL TILL W6H701                                           
014300                                                                          
014400     03  W-IDKR-X.                                                        
014500         05  W-IDKR              PIC 9(5).                                
014600                                                                          
014700*----> NYCKEL TILL W6H721                                                 
014800                                                                          
014900     03  W-IDSEGMNR-X.                                                    
015000         05  W-IDSEGMNR          PIC S9      VALUE +0 COMP-3.             
015100                                                                          
015200*----> FELKODER  XXLA                                                     
015300                                                                          
015400     03  W-4825-IDHTYP-X.                                                 
015500         05  W-4825-IDHTYP       PIC  X(4)  VALUE '4825'.                 
015600         05  W-4825-IDSKYLT      PIC  X(3)  VALUE 'GB '.                  
015700         05  W-4825-LOW-VALUE    PIC  X(23) VALUE LOW-VALUE.              
015800                                                                          
015900     03  W-4826-X.                                                        
016000         05  W-4826-IDKRFEL      PIC  X(2)  VALUE SPACE.                  
016100         05  W-4826-LOW-VALUE    PIC  X(8)  VALUE LOW-VALUE.              
016200                                                                          
016300*    --- STATUS-KOD FRÅN IMS                                              
016400                                                                          
016500 01  STATUS-WS                   PIC  X(02).                              
016600     88  SEGMENT-FINNS                       VALUE '  '.                  
016700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016800     88  BASEN-SLUT                          VALUE 'GB'.                  
016900     SKIP2                                                                
017000 01  GODK-STATUSKODER.                                                    
017100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017200                                                                          
017300 01  SSA1                        PIC X(96).                               
017400 01  SSA2                        PIC X(96).                               
017500 01  SSA3                        PIC X(96).                               
017600     EJECT                                                                
017700**************************************                                    
017800*  PRINTRADER FÖR KREDITNOTA                                              
017900**************************************                                    
018000                                                                          
018100 01  W001-DAP.                                                            
018200     03  FILLER              PIC X(165)  VALUE SPACE.                     
018300     EJECT                                                                
018400                                                                          
018500 01  WS-HDR-AREA-1.                                                       
018510     03 WS-IDOUTTYPE             PIC X(15).                               
018520     EJECT                                                                
018530 01  WS-HDR-AREA-2.                                                       
018540     03 WS-IDOUTREC              PIC X(30).                               
018550     EJECT                                                                
018560 01  WS-HDR-AREA-3.                                                       
018570     03 WS-IDLIST                PIC X(10).                               
018580     EJECT                                                                
018590                                                                          
       01  WS-DATE-TIME.                                                        
           03 WS-DATE                  PIC X(08).                               
           03 WS-TIME                  PIC X(06).                               
018591 01  WS-META-MARKET              PIC X(04).                               
018591 01  WS-META-FILE-NAME.                                                   
018591     03 WS-META-FILE-TYPE        PIC X(09) VALUE 'CLAIMCRE_'.             
018591     03 WS-META-DOC-NO           PIC 9(9).                                
018591     03 FILLER                   PIC X(02) VALUE '_D'.                    
018591     03 WS-META-DATE             PIC X(08).                               
018591     03 FILLER                   PIC X(02) VALUE '_T'.                    
018591     03 WS-META-TIME             PIC X(06).                               
018591 01  WS-META-ORDER-NO            PIC X(10) VALUE ZEROES.                  
018591                                                                          
018592     EJECT                                                                
018600 01  DOC-LINE-AREA.                                                       
018700     03 RAD-507-SKIP             PIC X(1).                                
018800     03 RAD-W4265502             PIC X(80) VALUE SPACE.                   
018900                                                                          
019000 01  W001-RAD.                                                            
019100                                                                          
019200     03  FILLER                  PIC X(80)   VALUE SPACE.                 
019300                                                                          
019400 01  BLANKRAD.                                                            
019500     03 FILLER                   PIC X(80)  VALUE ALL ' '.                
019600                                                                          
019700 01  HRAD5.                                                               
019800     03   FILLER                  PIC X(44) VALUE SPACE.                  
019900     03   HRAD5-TIKRED            PIC X(6).                               
020000     03   FILLER                  PIC X(3)  VALUE SPACE.                  
020100     03   HRAD5-IDVERNR           PIC Z(8)9.                              
020200     03   FILLER                  PIC X(4)  VALUE SPACE.                  
020300     03   HRAD5-TYP               PIC X(2)  VALUE SPACE.                  
020400     03   HRAD5-IDKR              PIC 9(5).                               
020500                                                                          
020600 01  HRAD9.                                                               
020700     03   FILLER                  PIC X(7)  VALUE SPACE.                  
020800     03   HRAD9-BELEV             PIC X(35).                              
020900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
021000     03   HRAD9-BENAEMN           PIC X(25).                              
021100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
021200     03   HRAD9-IDVERNR           PIC Z(7)9.                              
021300                                                                          
021400 01  HRAD10.                                                              
021500     03   FILLER                  PIC X(7)  VALUE SPACE.                  
021600     03   HRAD10-ADLEV-RAD1       PIC X(35).                              
021700     03   FILLER                  PIC X(2)  VALUE SPACE.                  
021800     03   HRAD10-IDTFN            PIC X(20).                              
021900                                                                          
022000 01  HRAD11.                                                              
022100     03   FILLER                  PIC X(7)  VALUE SPACE.                  
022200     03   HRAD11-ADLEV-RAD2       PIC X(35).                              
022300                                                                          
022400 01  HRAD12.                                                              
022500     03   FILLER                  PIC X(7)  VALUE SPACE.                  
022600     03   HRAD12-ADLEV-ORT        PIC X(35).                              
022700     03   FILLER                  PIC X(2)  VALUE SPACE.                  
022800     03   HRAD12-BENAEMN          PIC X(25).                              
022900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
023000     03   HRAD12-IDAVINR          PIC Z(6)9.                              
023100                                                                          
023200 01  HRAD13.                                                              
023300     03   FILLER                  PIC X(7)  VALUE SPACE.                  
023400     03   HRAD13-ADLEVLND         PIC X(35).                              
023500                                                                          
023600 01  HRAD16.                                                              
023700     03   FILLER                  PIC X(16) VALUE SPACE.                  
023800     03   HRAD16-IDLEVNR          PIC X(5).                               
023900     03   FILLER                  PIC X(33) VALUE SPACE.                  
023930     03   HRAD16-IDKUNDRF         PIC X(10).                              
023931                                                                          
023932 01  HRAD14.                                                              
023933     03   FILLER                  PIC X(7) VALUE SPACE.                   
023934     03   HRAD14-SUPVAT-TXT       PIC X(21) VALUE                         
023935                                      'Supplier VAT Reg No. '.            
023936     03   HRAD14-IDVAT            PIC X(17).                              
023940                                                                          
024000 01  HRAD22.                                                              
024100     03   FILLER                  PIC X(7)  VALUE SPACE.                  
024200     03   HRAD22-IDARTNR          PIC Z(8)9.                              
024300     03   FILLER                  PIC X(1) VALUE SPACE.                   
024400     03   HRAD22-BEART            PIC X(25).                              
024500     03   FILLER                  PIC X(25) VALUE SPACE.                  
024600     03   HRAD22-ARTIKEL-BELOPP   PIC Z(8)9.9(2).                         
024700                                                                          
024800 01  HRAD24.                                                              
024900    03 HRAD-24 OCCURS 3.                                                  
025000       05 FILLER                  PIC X(17) VALUE SPACE.                  
025100       05 HRAD24-ARB-KOSTNAD      PIC X(30).                              
025200       05 FILLER                  PIC X(21) VALUE SPACE.                  
025300       05 HRAD24-ARB-TOTAL        PIC Z(8)9.9(2).                         
025400                                                                          
025500 01  HRAD37.                                                              
025600     03 FILLER                    PIC X(68) VALUE SPACE.                  
025700     03 HRAD37-SPEC-TOTAL         PIC Z(8)9.9(2).                         
025800                                                                          
025900 01  HRAD43.                                                              
026000     03   FILLER                  PIC X(9)  VALUE SPACE.                  
026100     03   HRAD43-MOMS-BELOPP      PIC Z(8)9.9(2).                         
026200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
026300     03   HRAD43-MOMS-SATS        PIC Z(2)9.9(2).                         
026400     03   FILLER                  PIC X(5)  VALUE SPACE.                  
026500     03   HRAD43-PRMOMS           PIC Z(6)9.9(2).                         
026600     03   FILLER                  PIC X(3)  VALUE SPACE.                  
026700     03   HRAD43-BELOPP           PIC Z(8)9.9(2).                         
026800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
026900     03   HRAD43-KDVALISO         PIC X(3).                               
027000     03   FILLER                  PIC X(4)  VALUE SPACE.                  
027100     03   HRAD43-MOMS-TOTAL       PIC Z(8)9.9(2).                         
027200                                                                          
027300 01  HRAD46.                                                              
027400     03   FILLER                  PIC X(7)  VALUE SPACE.                  
027500     03   HRAD46-TEKREKON         PIC X(70).                              
027600                                                                          
027700 01  HRAD49.                                                              
027800     03   FILLER                  PIC X(7)  VALUE SPACE.                  
027900     03   HRAD49-BEKRFEL          PIC X(70).                              
028000                                                                          
028100 01  HRAD50.                                                              
028200    03 HRAD-50 OCCURS 16.                                                 
028300       05 FILLER                  PIC X(7)  VALUE SPACE.                  
028400       05 HRAD50-TEKRFEL          PIC X(70).                              
028500     EJECT                                                                
028600*    --- IMS FUNKTIONSKODER                                               
028700*01  -COPY W0003                                                          
028800     EJECT                                                                
028900*    ---  DLI INPUT-OUTPUT AREA                                           
029000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA01'.        
029100 01  DLI-IO-AREA01.                                                       
029200     03  IO-AREA01               PIC X(385)   VALUE SPACE.                
029300     03  W6H701 REDEFINES IO-AREA01.                                      
029400*        05 -COPY W6H701                                                  
029500 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA11'.        
029600 01  DLI-IO-AREA11.                                                       
029700     03  IO-AREA11              PIC X(14)   VALUE SPACE.                  
029800     03  W6H711 REDEFINES IO-AREA11.                                      
029900*        05 -COPY W6H711                                                  
030000 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA12'.        
030100 01  DLI-IO-AREA12.                                                       
030200     03  IO-AREA12              PIC X(294)   VALUE SPACE.                 
030300     03  W6H712 REDEFINES IO-AREA12.                                      
030400*        05 -COPY W6H712                                                  
030500 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA14'.        
030600 01  DLI-IO-AREA14.                                                       
030700     03  IO-AREA14              PIC X(991)   VALUE SPACE.                 
030800     03  W6H714 REDEFINES IO-AREA14.                                      
030900*        05 -COPY W6H714                                                  
031000     EJECT                                                                
031100 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA21'.        
031200 01  DLI-IO-AREA21.                                                       
031300     03  IO-AREA21              PIC X(991)   VALUE SPACE.                 
031400     03  W6H721 REDEFINES IO-AREA21.                                      
031500*        05 -COPY W6H721                                                  
031600     EJECT                                                                
031700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
031800                                                                          
031900 01  DLI-IO-AREA2.                                                        
032000                                                                          
032100     03  IO-AREA2                PIC X(500)  VALUE SPACE.                 
032200                                                                          
032300     03  WLLEVA01 REDEFINES IO-AREA2.                                     
032400*        05 -COPY WDF101                                                  
032500     03  WLLEVA14 REDEFINES IO-AREA2.                                     
032600*        05 -COPY WDF106                                                  
032700     EJECT                                                                
032800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
032900                                                                          
033000 01  DLI-IO-AREA3.                                                        
033100     03  WLBENA11.                                                        
033200*        05 -COPY WDD311                                                  
033300                                                                          
033400 01  DLI-IO-AREA4.                                                        
033500                                                                          
033600     03  IO-AREA4                PIC X(100)   VALUE SPACE.                
033700                                                                          
033800     03  WLXXLA01 REDEFINES IO-AREA4.                                     
033900*        05 -COPY WDGX4825                                                
034000     03  WLXXLA11 REDEFINES IO-AREA4.                                     
034100*        05 -COPY WDGX4826                                                
034200     EJECT                                                                
034210 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
034220 01  DLI-IO-WDL601.                                                       
034230*    03  -COPY WDL601                                                     
034240     EJECT                                                                
034250 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
034260 01  DLI-IO-WDL611.                                                       
034270*    03  -COPY WDL611                                                     
034280     EJECT                                                                
034300 LINKAGE SECTION.                                                         
034400                                                                          
034800*01  -COPY W0008      -PRE LEVA-                                          
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100*01  -COPY W0008      -PRE W6H7-                                          
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008      -PRE BENA-                                          
035500     05  FILLER                  PIC X.                                   
035600     EJECT                                                                
035700*01  -COPY W0008      -PRE XXLA-                                          
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
035910*01  -COPY W0008      -PRE WDL6-                                          
035922     05  FILLER                  PIC X.                                   
035930     EJECT                                                                
036000 PROCEDURE DIVISION  USING LEVA-PCB                                       
036100                           W6H7-PCB                                       
036200                           BENA-PCB                                       
036300                           XXLA-PCB                                       
036310                           WDL6-PCB.                                      
036400                                                                          
036500     ENTRY 'DLITCBL' USING LEVA-PCB                                       
036600                           W6H7-PCB                                       
036700                           BENA-PCB                                       
036800                           XXLA-PCB                                       
036810                           WDL6-PCB.                                      
036900                                                                          
037000     PERFORM A-INIT                                                       
037100                                                                          
037200     PERFORM S01-READ-W42691                                              
037300                                                                          
037400     PERFORM UNTIL END-OF-W42691                                          
037500       PERFORM B-LAES-OCH-BEHANDLA                                        
037600       PERFORM S01-READ-W42691                                            
037700     END-PERFORM                                                          
037800                                                                          
037900     PERFORM Z-FINIT                                                      
038000                                                                          
038100     MOVE ZERO TO RETURN-CODE                                             
038200     GOBACK                                                               
038300     .                                                                    
038400     EJECT                                                                
038500 A-INIT SECTION.                                                          
038600     OPEN INPUT  W42691                                                   
038610     OPEN OUTPUT W42655                                                   
038700                                                                          
038800     MOVE NEJ TO W42691-EOF-SW                                            
038900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
039000     ACCEPT CURRENT-DATE  FROM DATE                                       
039100     .                                                                    
039200     EJECT                                                                
039300 B-LAES-OCH-BEHANDLA SECTION.                                             
039400     PERFORM BD-BLANKA-UT-SIDA                                            
039500     MOVE INXE-KRK-IDKR            TO W-IDKR                              
039600     MOVE INXE-KRK-IDSEGMNR        TO W-IDSEGMNR                          
039700     PERFORM IMS-GU-W6H701                                                
039800     IF SEGMENT-FINNS                                                     
039900       PERFORM BA-INITIERA-NYCKLAR                                        
040000       PERFORM BB-REDIGERA-EKONOMI                                        
040100       PERFORM BC-SKRIV-KREDITNOT                                         
040200     END-IF                                                               
040300     .                                                                    
040400     EJECT                                                                
040500 BA-INITIERA-NYCKLAR  SECTION.                                            
040600      MOVE KR-IDARTNR              TO W-IDARTNR                           
040700                                      WS-IDARTNR                          
040800      MOVE BLANKRAD                TO HRAD22                              
040900      MOVE WS-IDARTNR              TO HRAD22-IDARTNR                      
041000                                                                          
041100      MOVE KR-BEKRANS              TO HRAD9-BENAEMN                       
041200      MOVE KR-ADATTENT(1:25)       TO HRAD12-BENAEMN                      
041300      MOVE KR-IDAVINR              TO HRAD12-IDAVINR                      
041400                                                                          
041500      MOVE KR-IDKRATLF             TO HRAD10-IDTFN                        
041600                                                                          
041700      MOVE KR-IDLEVNR              TO W-IDLEVNR                           
041800                                                                          
041900      PERFORM BAA-HAMTA-BENAMNING                                         
042000                                                                          
042100      PERFORM BAB-HAMTA-ADDRESS-INFO                                      
042200                                                                          
042300      PERFORM BAC-REDIGERA-FELKOD                                         
042400                                                                          
042410      PERFORM BAD-IDKUNDRF-INFO                                           
042420                                                                          
042500      PERFORM IMS-GNP-W6H714                                              
042600                                                                          
042700      IF SEGMENT-FINNS                                                    
042800        MOVE +1 TO IX                                                     
042900        PERFORM UNTIL IX > 15                                             
043000          MOVE TEXT-TEKRFEL(IX) TO HRAD50-TEKRFEL(IX)                     
043100          ADD +1                TO IX                                     
043200        END-PERFORM                                                       
043300      ELSE                                                                
043400        MOVE SPACE              TO HRAD50                                 
043500      END-IF                                                              
043600                                                                          
043700      IF KR-IDKRFEL(1:1) = 'P' OR 'K'                                     
043800        MOVE 'RA'                  TO HRAD5-TYP                           
043900      ELSE                                                                
044000        MOVE 'RT'                  TO HRAD5-TYP                           
044100      END-IF                                                              
044200                                                                          
044300      MOVE KR-IDKR                 TO HRAD5-IDKR                          
044400      MOVE KR-IDLEVNR              TO HRAD16-IDLEVNR                      
044500                                                                          
044600      MOVE BLANKRAD                TO HRAD-24(1)                          
044700                                      HRAD-24(2)                          
044800                                      HRAD-24(3)                          
044900                                                                          
045000      IF KR-TEKRSPEC-ATID   NOT = SPACE                                   
045100        MOVE KR-TEKRSPEC-ATID    TO HRAD24-ARB-KOSTNAD(1)                 
045200      ELSE                                                                
045300        MOVE 'LABOUR COSTS   '   TO HRAD24-ARB-KOSTNAD(1)                 
045400      END-IF                                                              
045500      IF KR-TEKRSPEC-OMK   NOT = SPACE                                    
045600        MOVE KR-TEKRSPEC-OMK     TO HRAD24-ARB-KOSTNAD(2)                 
045700      ELSE                                                                
045800        MOVE 'TRANSPORT COSTS  ' TO HRAD24-ARB-KOSTNAD(2)                 
045900      END-IF                                                              
046000      IF KR-TEKRSPEC-MAT    NOT = SPACE                                   
046100        MOVE KR-TEKRSPEC-MAT     TO HRAD24-ARB-KOSTNAD (3)                
046200      ELSE                                                                
046300        MOVE 'MATERIAL COSTS   ' TO HRAD24-ARB-KOSTNAD (3)                
046400      END-IF                                                              
046500      MOVE ZERO TO   WS-TOT-EXKL-MOMS                                     
046600                     WS-SUOMK                                             
046700                     WS-SUOMK-SEK                                         
046800                     WS-SUMAT                                             
046900                     WS-SUMAT-SEK                                         
047000                     WS-TOT-INKL-MOMS                                     
047100     .                                                                    
047200     EJECT                                                                
047300 BAA-HAMTA-BENAMNING SECTION.                                             
047400     PERFORM IMS-GU-BENA                                                  
047500     IF SEGMENT-FINNS                                                     
047600        MOVE TEXT-BEART TO HRAD22-BEART                                   
047700     END-IF                                                               
047800     .                                                                    
047900     EJECT                                                                
048000 BAB-HAMTA-ADDRESS-INFO SECTION.                                          
048100     PERFORM IMS-GU-LEVA                                                  
048200                                                                          
048300     IF SEGMENT-FINNS                                                     
048400       PERFORM IMS-GNP-LEVA-106                                           
048500       IF SEGMENT-FINNS                                                   
048600         MOVE ADR-BELEV            TO HRAD9-BELEV                         
048700         MOVE ADR-ADLEV-RAD1       TO HRAD10-ADLEV-RAD1                   
048800         MOVE ADR-ADLEV-RAD2       TO HRAD11-ADLEV-RAD2                   
048900         MOVE ADR-ADLEV-ORT        TO HRAD12-ADLEV-ORT                    
049000         MOVE ADR-ADLEVLND         TO HRAD13-ADLEVLND                     
049100         MOVE ADR-IDLANDX2         TO WS-IDLANDX2                         
049110         MOVE ADR-IDVAT            TO HRAD14-IDVAT                        
049200       END-IF                                                             
049300     END-IF                                                               
049400     .                                                                    
049500     EJECT                                                                
049600 BAC-REDIGERA-FELKOD SECTION.                                             
049700     MOVE BLANKRAD       TO HRAD49                                        
049800     MOVE KR-IDKRFEL     TO W-4826-IDKRFEL                                
049900                                                                          
050000     PERFORM IMS-GU-XXLA-4826                                             
050100     IF SEGMENT-FINNS                                                     
050200       MOVE 4826-BEKRFEL TO HRAD49-BEKRFEL                                
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050510 BAD-IDKUNDRF-INFO   SECTION.                                             
050530     MOVE KR-IDARTNR     TO W-WDL601-IDARTNR                              
050540                                                                          
050550     PERFORM IMS-GU-WDL601                                                
050560     IF SEGMENT-FINNS                                                     
050570       PERFORM IMS-GNP-WDL611                                             
050580       IF SEGMENT-FINNS                                                   
050590          PERFORM UNTIL SEGMENT-SAKNAS                                    
050595           IF INL-IDDC = KR-IDDC AND                                      
050596              INL-IDLEVNR = KR-IDLEVNR                                    
050597             MOVE INL-IDKUNDRF TO HRAD16-IDKUNDRF                         
050599             MOVE 'Y' TO WS-MATCH-SW                                      
050600           END-IF                                                         
050601           PERFORM IMS-GNP-WDL611                                         
050602          END-PERFORM                                                     
050603       END-IF                                                             
050604       IF WS-MATCH-SW = 'N' AND SEGMENT-SAKNAS                            
050605          MOVE SPACES TO HRAD16-IDKUNDRF                                  
050606       END-IF                                                             
050607     END-IF                                                               
050608     .                                                                    
050609     EJECT                                                                
050610 BB-REDIGERA-EKONOMI SECTION.                                             
050700     PERFORM IMS-GU-W6H701                                                
050800     PERFORM IMS-GNP-W6H712                                               
050900     IF SEGMENT-FINNS                                                     
051000       MOVE EK-IDVERNR             TO HRAD9-IDVERNR                       
051100                                                                          
051200       PERFORM IMS-GU-W6H721                                              
051300       IF SEGMENT-FINNS                                                   
051400                                                                          
051500         MOVE KRED-TIKRED          TO WS-TIKRED                           
051600         MOVE WS-TIKRED            TO HRAD5-TIKRED                        
051700         MOVE KRED-IDVERNR         TO HRAD5-IDVERNR                       
051800         MOVE KRED-TEKREKON-EXT    TO HRAD46-TEKREKON                     
051900                                                                          
052000         PERFORM BBA-RAEKNA-SUMMOR                                        
052100       END-IF                                                             
052200     END-IF                                                               
052300     .                                                                    
052400     EJECT                                                                
052500 BBA-RAEKNA-SUMMOR  SECTION.                                              
052600     COMPUTE HRAD22-ARTIKEL-BELOPP ROUNDED = KRED-PRARTBEL-PR             
052700                                                                          
052800     COMPUTE WS-SUOMK-SEK ROUNDED = KRED-SUOMK-INT-5DEC +                 
052900                                    KRED-SUOMK-EXT-5DEC                   
053000                                                                          
053100     COMPUTE WS-SUMAT-SEK ROUNDED = KRED-SUMAT-5DEC                       
053200     COMPUTE WS-SUMAT ROUNDED     = KRED-SUMAT-5DEC / EK-PRKURS           
053300                                                                          
053400     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-INT-5DEC / EK-PRKURS           
053500                                                                          
053600     IF WS-SUOMK NOT = ZERO                                               
053700       MOVE WS-SUOMK            TO HRAD24-ARB-TOTAL(1)                    
053800     ELSE                                                                 
053900       MOVE BLANKRAD            TO HRAD-24(1)                             
054000     END-IF                                                               
054100                                                                          
054200     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-EXT-5DEC / EK-PRKURS           
054300                                                                          
054400     IF WS-SUOMK NOT = ZERO                                               
054500       MOVE WS-SUOMK            TO HRAD24-ARB-TOTAL(2)                    
054600     ELSE                                                                 
054700       MOVE BLANKRAD            TO HRAD-24(2)                             
054800     END-IF                                                               
054900                                                                          
055000     IF WS-SUMAT NOT = ZERO                                               
055100       MOVE WS-SUMAT            TO HRAD24-ARB-TOTAL(3)                    
055200     ELSE                                                                 
055300       MOVE BLANKRAD            TO HRAD-24(3)                             
055400     END-IF                                                               
055500                                                                          
055600     COMPUTE WS-SUOMK ROUNDED = (KRED-SUOMK-INT-5DEC +                    
055700                                KRED-SUOMK-EXT-5DEC) / EK-PRKURS          
055800                                                                          
055900     COMPUTE WS-TOT-EXKL-MOMS   =  WS-SUOMK            +                  
056000                                   WS-SUMAT            +                  
056100                                   KRED-PRARTBEL-PR                       
056200                                                                          
056300     MOVE WS-TOT-EXKL-MOMS      TO HRAD37-SPEC-TOTAL                      
056400                                                                          
056500     IF KRED-PRMOMS NOT = ZERO                                            
056510       IF KR-IDFTG = 60                                                   
056520         IF CURRENT-YY = 18                                               
056530         AND EK-IDVERNR < 60000080                                        
056700           MOVE MOMS-SATS-CN-1    TO HRAD43-MOMS-SATS                     
056701         ELSE                                                             
056702           IF CURRENT-YY = 19                                             
056703           AND EK-IDVERNR < 60050026                                      
056704             MOVE MOMS-SATS-CN-2  TO HRAD43-MOMS-SATS                     
056705           ELSE                                                           
056706             MOVE MOMS-SATS-CN-3  TO HRAD43-MOMS-SATS                     
056707           END-IF                                                         
056708         END-IF                                                           
056713       END-IF                                                             
056714       IF KR-IDFTG = 53                                                   
056730         MOVE MOMS-SATS-US      TO HRAD43-MOMS-SATS                       
056740       END-IF                                                             
056750       IF KR-IDFTG = 57                                                   
056900         MOVE MOMS-SATS         TO HRAD43-MOMS-SATS                       
057000       END-IF                                                             
057100       MOVE WS-TOT-EXKL-MOMS    TO HRAD43-MOMS-BELOPP                     
057200       COMPUTE HRAD43-PRMOMS = KRED-PRMOMS / EK-PRKURS                    
057300     END-IF                                                               
057400                                                                          
057500     COMPUTE WS-TOT-INKL-MOMS = (KRED-PRMOMS / EK-PRKURS) +               
057600                                 WS-TOT-EXKL-MOMS                         
057700     MOVE WS-TOT-INKL-MOMS      TO HRAD43-MOMS-TOTAL                      
057800                                                                          
057900     IF EK-PRKURS = 1.00                                                  
058000       MOVE WS-TOT-INKL-MOMS    TO HRAD43-BELOPP                          
058100     ELSE                                                                 
058200       COMPUTE WS-TOT-SEK ROUNDED = WS-SUOMK-SEK + KRED-PRMOMS +          
058300                WS-SUMAT-SEK + (KRED-PRARTBEL-PR * EK-PRKURS)             
058400       MOVE WS-TOT-SEK          TO  HRAD43-BELOPP                         
058500     END-IF                                                               
058600                                                                          
058700     MOVE EK-KDVALISO           TO  HRAD43-KDVALISO                       
058800     .                                                                    
058900     EJECT                                                                
059000 BC-SKRIV-KREDITNOT  SECTION.                                             
059100**** HEADER TILL D&P                                                      
059600                                                                          
060000     MOVE SPACE                      TO WS-IDOUTREC                       
060100                                                                          
060200     MOVE 'W42655-001'               TO WS-IDOUTTYPE                      
060300     IF KR-IDFTG = 60                                                     
060310       MOVE 'CN05'                   TO WS-META-MARKET                    
060400       MOVE 'CN'                     TO WS-IDOUTREC(1:2)                  
060500     END-IF                                                               
060510     IF KR-IDFTG = 57                                                     
060520       MOVE 'SEPV'                   TO WS-META-MARKET                    
060600       MOVE 'SE'                     TO WS-IDOUTREC(1:2)                  
060700     END-IF                                                               
060710     IF KR-IDFTG = 53                                                     
060711       MOVE 'US01'                   TO WS-META-MARKET                    
060720       MOVE 'US'                     TO WS-IDOUTREC(1:2)                  
060730     END-IF                                                               
060800     IF KR-IDKRFEL(1:1) = 'P' OR 'K'                                      
060900       MOVE 'RA'                     TO WS-IDOUTREC(3:2)                  
061000     ELSE                                                                 
061100       MOVE 'RT'                     TO WS-IDOUTREC(3:2)                  
061200     END-IF                                                               
061300     MOVE W-IDLEVNR                  TO WS-IDOUTREC(5:5)                  
061400     MOVE EK-IDVERNR                 TO WS-IDLIST                         
061500                                                                          
061610     PERFORM S90-WRITE-HEADER                                             
061700                                                                          
061800     PERFORM S02-SKRIV-HUVUD                                              
061900                                                                          
062000     MOVE '0'            TO STYR                                          
062100     MOVE BLANKRAD       TO W001-RAD                                      
062200     PERFORM S05-SKRIV-RAD                                                
062300                                                                          
062400     MOVE '-'            TO STYR                                          
062500     MOVE BLANKRAD       TO W001-RAD                                      
062600     PERFORM S05-SKRIV-RAD                                                
062700                                                                          
062800     MOVE ' '            TO STYR                                          
062900     MOVE    HRAD22      TO W001-RAD                                      
063000     PERFORM S05-SKRIV-RAD                                                
063100                                                                          
063200     MOVE '0'            TO STYR                                          
063300     MOVE    HRAD-24(1)  TO W001-RAD                                      
063400     PERFORM S05-SKRIV-RAD                                                
063500                                                                          
063600     MOVE ' '            TO STYR                                          
063700     MOVE    HRAD-24(2)  TO W001-RAD                                      
063800     PERFORM S05-SKRIV-RAD                                                
063900                                                                          
064000     MOVE ' '            TO STYR                                          
064100     MOVE    HRAD-24(3)  TO W001-RAD                                      
064200     PERFORM S05-SKRIV-RAD                                                
064300                                                                          
064400     MOVE '0'            TO STYR                                          
064500     MOVE BLANKRAD       TO W001-RAD                                      
064600     PERFORM S05-SKRIV-RAD                                                
064700                                                                          
064800     MOVE ' '            TO STYR                                          
064900     MOVE BLANKRAD       TO W001-RAD                                      
065000     PERFORM S05-SKRIV-RAD                                                
065100                                                                          
065200     MOVE '0'            TO STYR                                          
065300     MOVE    HRAD37      TO W001-RAD                                      
065400     PERFORM S05-SKRIV-RAD                                                
065500                                                                          
065600     MOVE '0'            TO STYR                                          
065700     MOVE BLANKRAD       TO W001-RAD                                      
065800     PERFORM S05-SKRIV-RAD                                                
065900                                                                          
066000     MOVE '0'            TO STYR                                          
066100     MOVE BLANKRAD       TO W001-RAD                                      
066200     PERFORM S05-SKRIV-RAD                                                
066300                                                                          
066400     MOVE ' '            TO STYR                                          
066500     MOVE    HRAD43      TO W001-RAD                                      
066600     PERFORM S05-SKRIV-RAD                                                
066700                                                                          
066800     MOVE ' '            TO STYR                                          
066900     MOVE BLANKRAD       TO W001-RAD                                      
067000     PERFORM S05-SKRIV-RAD                                                
067100                                                                          
067200     MOVE ' '            TO STYR                                          
067300     MOVE BLANKRAD       TO W001-RAD                                      
067400     PERFORM S05-SKRIV-RAD                                                
067500                                                                          
067600     MOVE ' '            TO STYR                                          
067700     MOVE    HRAD46      TO W001-RAD                                      
067800     PERFORM S05-SKRIV-RAD                                                
067900                                                                          
068000     MOVE '-'            TO STYR                                          
068100     MOVE BLANKRAD       TO W001-RAD                                      
068200     PERFORM S05-SKRIV-RAD                                                
068300                                                                          
068400     MOVE ' '            TO STYR                                          
068500     MOVE    HRAD49      TO W001-RAD                                      
068600     PERFORM S05-SKRIV-RAD                                                
068700                                                                          
068800     MOVE ' '            TO STYR                                          
068900     MOVE    HRAD-50(1)  TO W001-RAD                                      
069000     PERFORM S05-SKRIV-RAD                                                
069100                                                                          
069200     MOVE ' '            TO STYR                                          
069300     MOVE    HRAD-50(2)  TO W001-RAD                                      
069400     PERFORM S05-SKRIV-RAD                                                
069500                                                                          
069600     MOVE ' '            TO STYR                                          
069700     MOVE    HRAD-50(3)  TO W001-RAD                                      
069800     PERFORM S05-SKRIV-RAD                                                
069900                                                                          
070000     MOVE ' '            TO STYR                                          
070100     MOVE    HRAD-50(4)  TO W001-RAD                                      
070200     PERFORM S05-SKRIV-RAD                                                
070300                                                                          
070400     MOVE ' '            TO STYR                                          
070500     MOVE    HRAD-50(5)  TO W001-RAD                                      
070600     PERFORM S05-SKRIV-RAD                                                
070700                                                                          
070800     MOVE ' '            TO STYR                                          
070900     MOVE    HRAD-50(6)  TO W001-RAD                                      
071000     PERFORM S05-SKRIV-RAD                                                
071100                                                                          
071200     MOVE ' '            TO STYR                                          
071300     MOVE    HRAD-50(7)  TO W001-RAD                                      
071400     PERFORM S05-SKRIV-RAD                                                
071500                                                                          
071600     MOVE ' '            TO STYR                                          
071700     MOVE    HRAD-50(8)  TO W001-RAD                                      
071800     PERFORM S05-SKRIV-RAD                                                
071900                                                                          
072000     MOVE ' '            TO STYR                                          
072100     MOVE    HRAD-50(9)  TO W001-RAD                                      
072200     PERFORM S05-SKRIV-RAD                                                
072300                                                                          
072400     MOVE ' '            TO STYR                                          
072500     MOVE    HRAD-50(10) TO W001-RAD                                      
072600     PERFORM S05-SKRIV-RAD                                                
072700                                                                          
072800     MOVE ' '            TO STYR                                          
072900     MOVE    HRAD-50(11) TO W001-RAD                                      
073000     PERFORM S05-SKRIV-RAD                                                
073100                                                                          
073200     MOVE ' '            TO STYR                                          
073300     MOVE    HRAD-50(12) TO W001-RAD                                      
073400     PERFORM S05-SKRIV-RAD                                                
073500                                                                          
073600     MOVE ' '            TO STYR                                          
073700     MOVE    HRAD-50(13) TO W001-RAD                                      
073800     PERFORM S05-SKRIV-RAD                                                
073900                                                                          
074000     MOVE ' '            TO STYR                                          
074100     MOVE    HRAD-50(14) TO W001-RAD                                      
074200     PERFORM S05-SKRIV-RAD                                                
074300                                                                          
074400     MOVE ' '            TO STYR                                          
074500     MOVE    HRAD-50(15) TO W001-RAD                                      
074600     PERFORM S05-SKRIV-RAD                                                
075100     .                                                                    
075200     EJECT                                                                
075300 BD-BLANKA-UT-SIDA SECTION.                                               
075400     MOVE SPACE TO HRAD5-TIKRED                                           
075500                   HRAD5-TYP                                              
075600                   HRAD9-BELEV                                            
075700                   HRAD9-BENAEMN                                          
075800                   HRAD10-ADLEV-RAD1                                      
075900                   HRAD10-IDTFN                                           
076000                   HRAD11-ADLEV-RAD2                                      
076100                   HRAD12-ADLEV-ORT                                       
076200                   HRAD12-BENAEMN                                         
076300                   HRAD13-ADLEVLND                                        
076400                   HRAD22                                                 
076500                   HRAD24-ARB-KOSTNAD(1)                                  
076600                   HRAD24-ARB-KOSTNAD(2)                                  
076700                   HRAD24-ARB-KOSTNAD(3)                                  
076800                   HRAD43-KDVALISO                                        
076900                   HRAD49-BEKRFEL                                         
077000     MOVE +1 TO IX                                                        
077100     PERFORM UNTIL IX > 15                                                
077200        MOVE SPACE TO HRAD50-TEKRFEL(IX)                                  
077300        ADD +1                TO IX                                       
077400     END-PERFORM                                                          
077500                                                                          
077600     MOVE ZERO TO  HRAD5-IDVERNR                                          
077700                   HRAD5-IDKR                                             
077800                   HRAD12-IDAVINR                                         
077900                   HRAD24-ARB-TOTAL(1)                                    
078000                   HRAD24-ARB-TOTAL(2)                                    
078100                   HRAD24-ARB-TOTAL(3)                                    
078200                   HRAD37-SPEC-TOTAL                                      
078300                   HRAD43-MOMS-BELOPP                                     
078400                   HRAD43-MOMS-SATS                                       
078500                   HRAD43-PRMOMS                                          
078600                   HRAD43-BELOPP                                          
078700                   HRAD43-MOMS-TOTAL                                      
078800     MOVE SPACE TO HRAD16-IDLEVNR                                         
078810                   HRAD14-IDVAT                                           
078900     .                                                                    
079000     EJECT                                                                
079100 S01-READ-W42691    SECTION.                                              
079200     READ W42691 RECORD INTO INXE-AREA                                    
079300     AT END                                                               
079400       MOVE JA           TO W42691-EOF-SW                                 
079500                                                                          
079600     NOT AT END                                                           
079700        MOVE 'W42691' TO POSTSUM-FDNAMN                                   
079800        MOVE 'W42655D1' TO POSTSUM-DDNAMN2                                
079900        MOVE INXE-KRK-IDPTYP TO POSTSUM-TRANSTYP                          
080000        CALL POSTSUM USING POSTSUM-PARM                                   
080100                                                                          
080200     END-READ                                                             
080300     .                                                                    
080400     EJECT                                                                
080500 S02-SKRIV-HUVUD SECTION.                                                 
080600     MOVE    SPACE       TO W001-RAD                                      
080700     MOVE '1'            TO STYR                                          
080800     PERFORM S04-SKRIV-NYSIDA                                             
080900                                                                          
081000     MOVE '-'            TO STYR                                          
081100     MOVE BLANKRAD       TO W001-RAD                                      
081200     PERFORM S05-SKRIV-RAD                                                
081300                                                                          
081400     MOVE ' '            TO STYR                                          
081500     MOVE    HRAD5       TO W001-RAD                                      
081600     PERFORM S05-SKRIV-RAD                                                
081700                                                                          
081800     MOVE '0'            TO STYR                                          
081900     MOVE BLANKRAD       TO W001-RAD                                      
082000     PERFORM S05-SKRIV-RAD                                                
082100                                                                          
082200     MOVE ' '            TO STYR                                          
082300     MOVE    HRAD9       TO W001-RAD                                      
082400     PERFORM S05-SKRIV-RAD                                                
082500                                                                          
082600     MOVE ' '            TO STYR                                          
082700     MOVE    HRAD10      TO W001-RAD                                      
082800     PERFORM S05-SKRIV-RAD                                                
082900                                                                          
083000     MOVE '0'            TO STYR                                          
083100     MOVE    HRAD11      TO W001-RAD                                      
083200     PERFORM S05-SKRIV-RAD                                                
083300                                                                          
083400     MOVE ' '            TO STYR                                          
083500     MOVE    HRAD12      TO W001-RAD                                      
083600     PERFORM S05-SKRIV-RAD                                                
083700                                                                          
083800     MOVE ' '            TO STYR                                          
083900     MOVE    HRAD13      TO W001-RAD                                      
084000     PERFORM S05-SKRIV-RAD                                                
084100                                                                          
084200     MOVE ' '            TO STYR                                          
084300     MOVE    BLANKRAD    TO W001-RAD                                      
084400     PERFORM S05-SKRIV-RAD                                                
084500                                                                          
084600     MOVE '0'            TO STYR                                          
084700     MOVE    HRAD16      TO W001-RAD                                      
084800     PERFORM S05-SKRIV-RAD                                                
084810                                                                          
084820     MOVE ' '            TO STYR                                          
084830     MOVE    HRAD14      TO W001-RAD                                      
084840     PERFORM S05-SKRIV-RAD                                                
084900     .                                                                    
085000     EJECT                                                                
085100 S04-SKRIV-NYSIDA SECTION.                                                
085200     MOVE STYR TO RAD-507-SKIP                                            
085300     MOVE W001-RAD TO RAD-W4265502                                        
085400     PERFORM S90-WRITE-DOC-LINE                                           
085500     .                                                                    
085600     EJECT                                                                
085700 S05-SKRIV-RAD SECTION.                                                   
085800     MOVE STYR TO RAD-507-SKIP                                            
085900     MOVE W001-RAD TO RAD-W4265502                                        
086000     PERFORM S90-WRITE-DOC-LINE                                           
086100     .                                                                    
086200     EJECT                                                                
087500                                                                          
087501 S90-WRITE-DOC-LINE SECTION.                                              
087502                                                                          
087503     WRITE W42655-001   FROM DOC-LINE-AREA                                
087504     .                                                                    
087505     EJECT                                                                
087506                                                                          
087510 S90-WRITE-HEADER SECTION.                                                
087520                                                                          
087530     PERFORM S91-SKRIV-DAP1-S                                             
087540     PERFORM S91-SKRIV-DAP2-S                                             
087550     PERFORM S91-SKRIV-DAP3-S                                             
087551     PERFORM S91-SKRIV-META-S                                             
087560     .                                                                    
087570                                                                          
087600 S91-SKRIV-DAP1-S SECTION.                                                
087800     STRING ' ¤DAP' WS-HDR-AREA-1                                         
087900            DELIMITED BY SIZE INTO W001-DAP                               
088000     WRITE W42655-001         FROM W001-DAP                               
088100                                                                          
088200     MOVE SPACE TO W001-DAP                                               
088300     .                                                                    
088400                                                                          
088500 S91-SKRIV-DAP2-S SECTION.                                                
088600                                                                          
088700     STRING ' ¤DAP' WS-HDR-AREA-2                                         
088800            DELIMITED BY SIZE INTO W001-DAP                               
088900     WRITE W42655-001         FROM W001-DAP                               
089000                                                                          
089100     MOVE SPACE TO W001-DAP                                               
089200     .                                                                    
089300 S91-SKRIV-DAP3-S SECTION.                                                
089400                                                                          
089500     STRING ' ¤DAP' WS-HDR-AREA-3                                         
089600            DELIMITED BY SIZE INTO W001-DAP                               
089700     WRITE W42655-001         FROM W001-DAP                               
089800                                                                          
089900     MOVE SPACE TO W001-DAP                                               
090000     .                                                                    
090100                                                                          
090200 S91-SKRIV-META-S SECTION.                                                
090300                                                                          
090320     MOVE SPACE TO W001-DAP                                               
090321                                                                          
090310     MOVE HRAD5-IDVERNR                TO WS-META-DOC-NO                  
107610     MOVE FUNCTION CURRENT-DATE (1:14) TO WS-DATE-TIME                    
107610     MOVE WS-DATE                      TO WS-META-DATE                    
107610     MOVE WS-TIME                      TO WS-META-TIME                    
107610                                                                          
090330     STRING '¤METAFile_Name=' WS-META-FILE-NAME                           
090340            DELIMITED BY SIZE INTO W001-DAP                               
090350     WRITE W42655-001         FROM W001-DAP                               
090360                                                                          
090320     MOVE SPACE TO W001-DAP                                               
090321                                                                          
090330     STRING '¤METAMarket=' WS-META-MARKET                                 
090340            DELIMITED BY SIZE INTO W001-DAP                               
090350     WRITE W42655-001         FROM W001-DAP                               
090360                                                                          
090370     MOVE SPACE TO W001-DAP                                               
090380                                                                          
090400     STRING '¤METADocument_Number=' HRAD5-IDVERNR                         
090500            DELIMITED BY SIZE INTO W001-DAP                               
090600     WRITE W42655-001         FROM W001-DAP                               
090700                                                                          
090800     MOVE SPACE TO W001-DAP                                               
090810                                                                          
090820     STRING '¤METAIR_Number=' HRAD5-TYP HRAD5-IDKR                        
090830            DELIMITED BY SIZE INTO W001-DAP                               
090840     WRITE W42655-001         FROM W001-DAP                               
090850                                                                          
090860     MOVE SPACE TO W001-DAP                                               
090870                                                                          
090880     STRING '¤METADate=' HRAD5-TIKRED                                     
090890            DELIMITED BY SIZE INTO W001-DAP                               
090891     WRITE W42655-001         FROM W001-DAP                               
090892                                                                          
090893     MOVE SPACE TO W001-DAP                                               
090894                                                                          
090895     STRING '¤METASupplier=' HRAD16-IDLEVNR                               
090896            DELIMITED BY SIZE INTO W001-DAP                               
090897     WRITE W42655-001         FROM W001-DAP                               
090898                                                                          
090899     MOVE SPACE TO W001-DAP                                               
090900                                                                          
090901     STRING '¤METASupplier_name=' HRAD9-BELEV                             
090902            DELIMITED BY SIZE INTO W001-DAP                               
090903     WRITE W42655-001         FROM W001-DAP                               
090904                                                                          
090905     MOVE SPACE TO W001-DAP                                               
090906                                                                          
090906     IF HRAD16-IDKUNDRF NOT = (SPACES AND LOW-VALUES)                     
090906        MOVE HRAD16-IDKUNDRF  TO WS-META-ORDER-NO                         
090906     END-IF                                                               
090907     STRING '¤METAOrder_Number=' WS-META-ORDER-NO                         
090908            DELIMITED BY SIZE INTO W001-DAP                               
090909     WRITE W42655-001         FROM W001-DAP                               
090910                                                                          
090911     MOVE SPACE TO W001-DAP                                               
090912                                                                          
090920     .                                                                    
091000                                                                          
091200 Z-FINIT      SECTION.                                                    
091300     CLOSE W42691                                                         
091400     SKIP2                                                                
091500     MOVE 'S' TO POSTSUM-OPKOD                                            
091600     CALL POSTSUM USING POSTSUM-PARM                                      
091700     .                                                                    
091800     EJECT                                                                
091900                                                                          
092000* --- IMS SEKTIONER ---                                                   
092100                                                                          
092200 IMS-GU-BENA SECTION.                                                     
092300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
092400          DELIMITED BY SIZE INTO SSA1                                     
092500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
092600          DELIMITED BY SIZE INTO SSA2                                     
092700     MOVE '  GE' TO GODK-STATUSKODER                                      
092800     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA3 SSA1 SSA2                
092900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
093000     PERFORM IMS-STATUSKONTROLL                                           
093100     .                                                                    
093200     EJECT                                                                
093210 IMS-GU-WDL601    SECTION.                                                
093220                                                                          
093230     STRING 'WDL601  (IDARTNR  =' W-WDL601-IDARTNR-X ')'                  
093240          DELIMITED BY SIZE INTO SSA1                                     
093250     MOVE '  GE'           TO GODK-STATUSKODER                            
093260     CALL CBLTDLI          USING GU WDL6-PCB DLI-IO-WDL601 SSA1           
093270     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
093280     PERFORM IMS-STATUSKONTROLL                                           
093290     .                                                                    
093291     EJECT                                                                
093292 IMS-GNP-WDL611   SECTION.                                                
093293     STRING 'WDL611  (DAINLEV =>' W-WDL611-DAINLEV-X ')'                  
093294          DELIMITED BY SIZE INTO SSA1                                     
093295     MOVE '  GE'           TO GODK-STATUSKODER                            
093296     CALL CBLTDLI          USING GNP WDL6-PCB DLI-IO-WDL611   SSA1        
093297     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
093298     PERFORM IMS-STATUSKONTROLL                                           
093299     .                                                                    
093300     EJECT                                                                
093310 IMS-GU-W6H701 SECTION.                                                   
093400     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
093500          DELIMITED BY SIZE INTO SSA1                                     
093600     MOVE '  GE' TO GODK-STATUSKODER                                      
093700     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-AREA01 SSA1                    
093800     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
093900     PERFORM IMS-STATUSKONTROLL                                           
094000     .                                                                    
094100     EJECT                                                                
094200 IMS-GNP-W6H712 SECTION.                                                  
094300     MOVE 'W6H712   ' TO SSA1                                             
094400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
094500     CALL CBLTDLI USING GNP W6H7-PCB DLI-IO-AREA12 SSA1                   
094600     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
094700     PERFORM IMS-STATUSKONTROLL                                           
094800     .                                                                    
094900     EJECT                                                                
095000 IMS-GNP-W6H714 SECTION.                                                  
095100     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
095200          DELIMITED BY SIZE INTO SSA1                                     
095300     MOVE 'W6H714   '           TO SSA2                                   
095400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
095500     CALL CBLTDLI USING GNP W6H7-PCB DLI-IO-AREA14 SSA1 SSA2              
095600     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
095700     PERFORM IMS-STATUSKONTROLL                                           
095800     .                                                                    
095900     EJECT                                                                
096000 IMS-GU-W6H721 SECTION.                                                   
096100     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
096200          DELIMITED BY SIZE INTO SSA1                                     
096300     MOVE 'W6H712   '          TO SSA2                                    
096400     STRING 'W6H721  (IDSEGMNR =' W-IDSEGMNR-X ')'                        
096500          DELIMITED BY SIZE INTO SSA3                                     
096600     MOVE '  GE' TO GODK-STATUSKODER                                      
096700     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-AREA21 SSA1 SSA2 SSA3          
096800     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
096900     PERFORM IMS-STATUSKONTROLL                                           
097000     .                                                                    
097100     EJECT                                                                
097200 IMS-GU-LEVA SECTION.                                                     
097300     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
097400          DELIMITED BY SIZE INTO SSA1                                     
097500     MOVE '  GE' TO GODK-STATUSKODER                                      
097600     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA2 SSA1                     
097700     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
097800     PERFORM IMS-STATUSKONTROLL                                           
097900     .                                                                    
098000     EJECT                                                                
098100 IMS-GNP-LEVA-106 SECTION.                                                
098200     MOVE 'WLLEVA14 ' TO SSA1                                             
098300     MOVE '  GE' TO GODK-STATUSKODER                                      
098400     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA2 SSA1                    
098500     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
098600     PERFORM IMS-STATUSKONTROLL                                           
098700     .                                                                    
098800     EJECT                                                                
098900 IMS-GU-XXLA-4826 SECTION.                                                
099000     STRING 'WLXXLA01(WDGXKEY  =' W-4825-IDHTYP-X ')'                     
099100          DELIMITED BY SIZE INTO SSA1                                     
099200     STRING 'WLXXLA11(WDGXKEY  =' W-4826-X ')'                            
099300          DELIMITED BY SIZE INTO SSA2                                     
099400     MOVE '  GE' TO GODK-STATUSKODER                                      
099500     CALL CBLTDLI USING GU XXLA-PCB DLI-IO-AREA4 SSA1 SSA2                
099600     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
099700     PERFORM IMS-STATUSKONTROLL                                           
099800     .                                                                    
099900     EJECT                                                                
100000 IMS-STATUSKONTROLL SECTION.                                              
100100     SET STATUS-IX TO 1                                                   
100200     SEARCH GODK-STATUS                                                   
100300       AT END CALL FELLOG                                                 
100400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
100500     END-SEARCH                                                           
100600     .                                                                    
