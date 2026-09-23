000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4262500.                                                
000400 AUTHOR.         GERRY CARMICHAEL                                         
000500 DATE-WRITTEN.   JANUARI 1992.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        LÄSER EN FIL OCH SKRIVER REKLAMATIONSFAKTUROR                    
001100*        TILL D&P                                                         
001200*                                                                         
001300*        PROGRAMMET LÄSER   WLLEVA  (WDF1)                                
001400*        PROGRAMMET LÄSER   W6KVAE  (W6H7)                                
001500*        PROGRAMMET LÄSER   WLBENA  (WDD3)                                
001600*        PROGRAMMET LÄSER   WLXXKA  (WDR1)                                
001700*        PROGRAMMET LÄSER   WDB6                                          
001800*                                                                         
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200*                                                                         
002300 FILE-CONTROL.                                                            
002400*                                                                         
002500*          --- INFIL FAKTURANUMMER                                        
002600     SELECT W42690               ASSIGN TO W42625D1.                      
002610*          --- DOCUMENT DATA RECORDS - OUTPUT                             
002620     SELECT W42625               ASSIGN TO W42625D2.                      
002700*                                                                         
002800 DATA DIVISION.                                                           
002900 SKIP2                                                                    
003000 FILE SECTION.                                                            
003100 SKIP2                                                                    
003200 FD  W42690                                                               
003300     LABEL RECORD STANDARD                                                
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  INXE-POST  -COPY W426PKRF -L                                         
003800 SKIP2                                                                    
003900*                                                                         
003910 FD  W42625                                                               
003920     RECORDING       V                                                    
003930     BLOCK CONTAINS  0.                                                   
003940                                                                          
003950 01  W42625-001                  PIC X(3000).                             
003960     EJECT                                                                
003970                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300*                                                                         
004400 77  IDPGM                       PIC X(08)   VALUE 'W4262500'.            
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
005000 77  STYR                        PIC X(1)   VALUE SPACE.                  
005100                                                                          
005200 77  WS-ARTIKEL-BELOPP           PIC S9(8)V9(3) VALUE +0 COMP-3.          
005300 77  WS-SUOMK-INT                PIC S9(7)V9(2) VALUE +0 COMP-3.          
005400 77  WS-SUOMK-EXT                PIC S9(7)V9(2) VALUE +0 COMP-3.          
005500 77  WS-SUOMK-TOT                PIC S9(7)V9(2) VALUE +0 COMP-3.          
005600 77  WS-SUMAT                    PIC S9(7)V9(2) VALUE +0 COMP-3.          
005700                                                                          
005800 77  WS-TOT-EXKL-MOMS            PIC 9(9)V9(2)  VALUE ZERO.               
005900 77  WS-TOT-INKL-MOMS            PIC 9(9)V9(2)  VALUE ZERO.               
006000 77  WS-TOT-SEK                  PIC 9(9)V9(2)  VALUE ZERO.               
006100 77  WS-TIFAKT                   PIC 9(6)       VALUE ZERO.               
006200 77  WS-IDLANDX2                 PIC X(2)       VALUE SPACE.              
006210 77  MOMS-SATS                   PIC 9(2)V9(2)  VALUE 25.00.              
006220 77  MOMS-SATS-CN-1              PIC 9(2)V9(2)  VALUE 17.00.              
006221 77  MOMS-SATS-CN-2              PIC 9(2)V9(2)  VALUE 16.00.              
006222 77  MOMS-SATS-CN-3              PIC 9(2)V9(2)  VALUE 13.00.              
006230 77  MOMS-SATS-US                PIC 9(2)V9(2)  VALUE ZERO.               
006300                                                                          
006800 77  WS-KVAVIS                   PIC Z(5)9      VALUE ZERO.               
006900 77  WS-KVANTMOT                 PIC Z(5)9      VALUE ZERO.               
007000 01  PA-TEXT               PIC X(27)                                      
007100           VALUE '  EXCESS, ADVICED QUANTITY '.                           
007200 01  PB-TEXT               PIC X(27)                                      
007300           VALUE 'SHORTAGE, ADVICED QUANTITY '.                           
007400 01  P-TEXT2              PIC X(9)                                        
007500           VALUE 'RECIEVED '.                                             
007600 01  P-TEXT3              PIC X(4)                                        
007700           VALUE 'PCS.'.                                                  
007800                                                                          
007810 01  WS-MATCH-SW                 PIC X       VALUE 'N'.                   
007900 77  W42690-EOF-SW               PIC X       VALUE 'N'.                   
008000     88  END-OF-W42690                       VALUE 'J'.                   
008100                                                                          
008200     EJECT                                                                
008300                                                                          
008400 01  CURRENT-DATE.                                                        
008500     03  CURRENT-YY              PIC 9(2).                                
008600     03  CURRENT-MM              PIC 9(2).                                
008700     03  CURRENT-DD              PIC 9(2).                                
008800 01  DAGENS-DATUM REDEFINES CURRENT-DATE   PIC 9(6).                      
008900                                                                          
009000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009100 01  GENERELLA-SUBPROGRAM.                                                
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009800                                                                          
009900 01  ERRTEXT.                                                             
010000     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
010100     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
010200 01  KDRC-DISPLAY                PIC Z(5).                                
010300     EJECT                                                                
010400                                                                          
010500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010800     EJECT                                                                
010900                                                                          
011500 SKIP2                                                                    
011600*    --- PARAMETRAR TILL POSTSUM                                          
011700*                                                                         
011800*01  -COPY W0005    -PRE  POSTSUM-                                        
011900     EJECT                                                                
012000                                                                          
012400*01  -COPY WWLEVEU                                                        
012500     EJECT                                                                
012600                                                                          
012700*01  -COPY WWLEV02                                                        
012800     EJECT                                                                
012900                                                                          
013000 01  INXE-AREA-START             PIC X(24)   VALUE                        
013100                                 'INXE-AREA-START '.                      
013200     SKIP2                                                                
013300*                                                                         
013400*01  AREA -COPY W426PKRF    -PRE INXE-                                    
013500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013800                                                                          
013900 01  NYCKLAR-TILL-DLI.                                                    
014000                                                                          
014100                                                                          
014101*----> DIREKTNYCKEL TILL WDL601                                           
014102                                                                          
014103     03  W-WDL601-IDARTNR-X.                                              
014104         05  W-WDL601-IDARTNR    PIC S9(9) COMP-3.                        
014105*----> DIREKTNYCKEL TILL WDL611                                           
014106                                                                          
014107     03  W-WDL611-DAINLEV-X.                                              
014108         05  W-WDL611-DAINLEV    PIC 9(16)  VALUE ZERO.                   
014200*----> DIREKTNYCKEL TILL WDD301                                           
014300                                                                          
014400     03  W-IDARTNR-X.                                                     
014500         05  W-IDARTNR           PIC S9(9) COMP-3.                        
014600                                                                          
014700*----> DIREKTNYCKEL TILL WDD311                                           
014800                                                                          
014900     03  W-IDSKYLT-X.                                                     
015000         05  W-IDSKYLT           PIC  X(3) VALUE 'GB '.                   
015100                                                                          
015200*----> DIREKTNYCKEL TILL WDF101                                           
015300                                                                          
015400     03  W-IDLEVNR-X.                                                     
015500         05  W-IDLEVNR           PIC X(5).                                
015600                                                                          
015700*----> DIREKTNYCKEL TILL W6H701                                           
015800                                                                          
015900     03  W-IDKR-X.                                                        
016000         05  W-IDKR              PIC 9(5).                                
016100                                                                          
016200*----> DIREKTNYCKEL TILL WDB601                                           
016300                                                                          
016400     03  W-IDDC-B6-X.                                                     
016500         05 W-IDDC-B6           PIC X(2)    VALUE '11'.                   
016600                                                                          
016700*----> FELKODER  XXLA                                                     
016800                                                                          
016900     03  W-4825-IDHTYP-X.                                                 
017000         05  W-4825-IDHTYP       PIC  X(4)  VALUE '4825'.                 
017100         05  W-4825-IDSKYLT      PIC  X(3)  VALUE 'GB '.                  
017200         05  W-4825-LOW-VALUE    PIC  X(23) VALUE LOW-VALUE.              
017300                                                                          
017400     03  W-4826-X.                                                        
017500         05  W-4826-IDKRFEL      PIC  X(2)  VALUE SPACE.                  
017600         05  W-4826-LOW-VALUE    PIC  X(8)  VALUE LOW-VALUE.              
017700                                                                          
017800*    --- STATUS-KOD FRÅN IMS                                              
017900                                                                          
018000 01  STATUS-WS                   PIC  X(02).                              
018100     88  SEGMENT-FINNS                       VALUE '  '.                  
018200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018300     88  BASEN-SLUT                          VALUE 'GB'.                  
018400     SKIP2                                                                
018500 01  GODK-STATUSKODER.                                                    
018600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018700                                                                          
018800 01  SSA1                        PIC X(96).                               
018900 01  SSA2                        PIC X(96).                               
019000     EJECT                                                                
019100**************************************                                    
019200*  PRINTRADER FÖR REKLAMMATIONSFAKTURA                                    
019300**************************************                                    
019400                                                                          
019500 01  W001-DAP.                                                            
019600     03  FILLER              PIC X(165)  VALUE SPACE.                     
019700     EJECT                                                                
019800                                                                          
019801 01  WS-DATE-TIME.                                                        
019801     03 WS-DATE                  PIC X(08).                               
019801     03 WS-TIME                  PIC X(06).                               
019801 01  WS-META-MARKET              PIC X(04).                               
019801 01  WS-META-FILE-NAME.                                                   
019801     03 WS-META-FILE-TYPE        PIC X(09) VALUE 'CLAIMINV_'.             
019801     03 WS-META-DOC-NO           PIC 9(8).                                
019801     03 FILLER                   PIC X(02) VALUE '_D'.                    
019801     03 WS-META-DATE             PIC X(08).                               
019801     03 FILLER                   PIC X(02) VALUE '_T'.                    
019801     03 WS-META-TIME             PIC X(06).                               
019801 01  WS-META-ORDER-NO            PIC X(10) VALUE ZEROES.                  
019802     EJECT                                                                
019810 01  WS-HDR-AREA-1.                                                       
019820     03 WS-IDOUTTYPE             PIC X(15).                               
019830     EJECT                                                                
019840 01  WS-HDR-AREA-2.                                                       
019850     03 WS-IDOUTREC              PIC X(30).                               
019860     EJECT                                                                
019870 01  WS-HDR-AREA-3.                                                       
019880     03 WS-IDLIST                PIC X(10).                               
019890     EJECT                                                                
019920                                                                          
020000 01  DOC-LINE-AREA.                                                       
020100     03 RAD-507-SKIP            PIC X(1).                                 
020200     03 RAD-W4262502             PIC X(80) VALUE SPACE.                   
020300                                                                          
020400 01  W001-RAD.                                                            
020500                                                                          
020600     03  FILLER                  PIC X(80)   VALUE SPACE.                 
020700                                                                          
020800 01  BLANKRAD.                                                            
020900     03 FILLER                   PIC X(80)  VALUE ALL ' '.                
021000                                                                          
021100 01  HRAD5.                                                               
021200     03   FILLER                  PIC X(44) VALUE SPACE.                  
021300     03   HRAD5-TIFAKT            PIC X(6).                               
021400     03   FILLER                  PIC X(4)  VALUE SPACE.                  
021500     03   HRAD5-IDVERNR           PIC Z(7)9.                              
021600     03   FILLER                  PIC X(4)  VALUE SPACE.                  
021700     03   HRAD5-TYP               PIC X(2)  VALUE SPACE.                  
021800     03   HRAD5-IDKR              PIC 9(5).                               
021900                                                                          
022000 01  HRAD9.                                                               
022100     03   FILLER                  PIC X(7)  VALUE SPACE.                  
022200     03   HRAD9-BELEV             PIC X(35).                              
022300     03   FILLER                  PIC X(2)  VALUE SPACE.                  
022400     03   HRAD9-BENAEMN           PIC X(25).                              
022500                                                                          
022600 01  HRAD10.                                                              
022700     03   FILLER                  PIC X(7)  VALUE SPACE.                  
022800     03   HRAD10-ADLEV-RAD1       PIC X(35).                              
022900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
023000     03   HRAD10-IDTFN            PIC X(20).                              
023100                                                                          
023200 01  HRAD11.                                                              
023300     03   FILLER                  PIC X(7)  VALUE SPACE.                  
023400     03   HRAD11-ADLEV-RAD2       PIC X(35).                              
023500                                                                          
023600 01  HRAD12.                                                              
023700     03   FILLER                  PIC X(7)  VALUE SPACE.                  
023800     03   HRAD12-ADLEV-ORT        PIC X(35).                              
023900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
024000     03   HRAD12-BENAEMN          PIC X(25).                              
024100     03   FILLER                  PIC X(2)  VALUE SPACE.                  
024200     03   HRAD12-IDAVINR          PIC Z(6)9.                              
024300                                                                          
024400 01  HRAD13.                                                              
024500     03   FILLER                  PIC X(7)  VALUE SPACE.                  
024600     03   HRAD13-ADLEVLND         PIC X(35).                              
024700                                                                          
024800 01  HRAD16.                                                              
024900     03   FILLER                  PIC X(16) VALUE SPACE.                  
025000     03   HRAD16-IDLEVNR          PIC X(5).                               
025100     03   FILLER                  PIC X(33) VALUE SPACE.                  
025130     03   HRAD16-IDKUNDRF         PIC X(10).                              
025140                                                                          
025150 01  HRAD14.                                                              
025151     03   FILLER                  PIC X(7)  VALUE SPACE.                  
025160     03   HRAD14-SUPVAT-TXT       PIC X(21) VALUE                         
025161                                      'Supplier VAT Reg No. '.            
025170     03   HRAD14-IDVAT            PIC X(17).                              
025180                                                                          
025200 01  HRAD22.                                                              
025300     03   FILLER                  PIC X(7)  VALUE SPACE.                  
025400     03   HRAD22-IDARTNR          PIC Z(8)9.                              
025500     03   FILLER                  PIC X(1) VALUE SPACE.                   
025600     03   HRAD22-BEART            PIC X(25).                              
025700     03   FILLER                  PIC X(1) VALUE SPACE.                   
025800     03   HRAD22-KVKRRET          PIC Z(6)9.                              
025900     03   FILLER                  PIC X(2)  VALUE SPACE.                  
026000     03   HRAD22-PRARTBEL-PR      PIC Z(7)9.9(3).                         
026100     03   FILLER                  PIC X(4)  VALUE SPACE.                  
026200     03   HRAD22-ARTIKEL-BELOPP   PIC Z(8)9.9(2).                         
026300                                                                          
026400 01  HRAD24.                                                              
026500    03 HRAD-24 OCCURS 3.                                                  
026600       05 FILLER                  PIC X(17) VALUE SPACE.                  
026700       05 HRAD24-ARB-KOSTNAD      PIC X(30).                              
026800       05 FILLER                  PIC X(21) VALUE SPACE.                  
026900       05 HRAD24-ARB-TOTAL        PIC Z(8)9.9(2).                         
027000                                                                          
027100 01  HRAD36.                                                              
027200     03 FILLER                    PIC X(7)  VALUE SPACE.                  
027300     03 HRAD36-VAT-TEXT           PIC X(18) VALUE                         
027400                                 'VAT REG NO.  DISP:'.                    
027500     03 FILLER                    PIC X(2) VALUE SPACE.                   
027600     03 HRAD36-VAT-NO             PIC X(17) VALUE SPACE.                  
027700                                                                          
027800 01  HRAD37-EU.                                                           
027900     03 FILLER                    PIC X(7)  VALUE SPACE.                  
028000     03 HRAD37-VAT-TEXT           PIC X(18) VALUE                         
028100                                 '             REC :'.                    
028200     03 FILLER                    PIC X(43) VALUE SPACE.                  
028300     03 HRAD37-SPEC-TOTAL-EU      PIC Z(8)9.9(2).                         
028400                                                                          
028500 01  HRAD37.                                                              
028600     03 FILLER                    PIC X(68) VALUE SPACE.                  
028700     03 HRAD37-SPEC-TOTAL         PIC Z(8)9.9(2).                         
028800                                                                          
028900 01  HRAD43.                                                              
029000     03   FILLER                  PIC X(9)  VALUE SPACE.                  
029100     03   HRAD43-MOMS-BELOPP      PIC Z(8)9.9(2).                         
029200     03   FILLER                  PIC X(2)  VALUE SPACE.                  
029300     03   HRAD43-MOMS-SATS        PIC Z(2)9.9(2).                         
029400     03   FILLER                  PIC X(5)  VALUE SPACE.                  
029500     03   HRAD43-PRMOMS           PIC Z(6)9.9(2).                         
029600     03   FILLER                  PIC X(3)  VALUE SPACE.                  
029700     03   HRAD43-BELOPP           PIC Z(8)9.9(2).                         
029800     03   FILLER                  PIC X(2)  VALUE SPACE.                  
029900     03   HRAD43-KDVALISO         PIC X(3).                               
030000     03   FILLER                  PIC X(4)  VALUE SPACE.                  
030100     03   HRAD43-MOMS-TOTAL       PIC Z(8)9.9(2).                         
030200                                                                          
030300 01  HRAD49.                                                              
030400     03   FILLER                  PIC X(7)  VALUE SPACE.                  
030500     03   HRAD49-BEKRFEL          PIC X(70).                              
030600                                                                          
030700 01  HRAD50.                                                              
030800    03 HRAD-50 OCCURS 16.                                                 
030900       05 FILLER                  PIC X(7)  VALUE SPACE.                  
031000       05 HRAD50-TEKRFEL          PIC X(70).                              
031100     EJECT                                                                
031200*    --- IMS FUNKTIONSKODER                                               
031300*01  -COPY W0003                                                          
031400     EJECT                                                                
031500*    ---  DLI INPUT-OUTPUT AREA                                           
031600 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA01'.        
031700 01  DLI-IO-AREA01.                                                       
031800     03  IO-AREA01               PIC X(382)   VALUE SPACE.                
031900     03  W6KVAE01 REDEFINES IO-AREA01.                                    
032000*        05 -COPY W6H701                                                  
032100 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA11'.        
032200 01  DLI-IO-AREA11.                                                       
032300     03  IO-AREA11              PIC X(14)   VALUE SPACE.                  
032400     03  W6KVAE11 REDEFINES IO-AREA11.                                    
032500*        05 -COPY W6H711                                                  
032600 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA12'.        
032700 01  DLI-IO-AREA12.                                                       
032800     03  IO-AREA12              PIC X(294)   VALUE SPACE.                 
032900     03  W6KVAE12 REDEFINES IO-AREA12.                                    
033000*        05 -COPY W6H712                                                  
033100 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA14'.        
033200 01  DLI-IO-AREA14.                                                       
033300     03  IO-AREA14              PIC X(991)   VALUE SPACE.                 
033400     03  W6KVAE14 REDEFINES IO-AREA14.                                    
033500*        05 -COPY W6H714                                                  
033600     EJECT                                                                
033700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
033800                                                                          
033900 01  DLI-IO-AREA2.                                                        
034000                                                                          
034100     03  IO-AREA2                PIC X(500)  VALUE SPACE.                 
034200                                                                          
034300     03  WLLEVA01 REDEFINES IO-AREA2.                                     
034400*        05 -COPY WDF101                                                  
034500     03  WLLEVA14 REDEFINES IO-AREA2.                                     
034600*        05 -COPY WDF106                                                  
034700     EJECT                                                                
034800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
034900                                                                          
035000 01  DLI-IO-AREA3.                                                        
035100     03  WLBENA11.                                                        
035200*        05 -COPY WDD311                                                  
035300                                                                          
035400 01  DLI-IO-AREA4.                                                        
035500                                                                          
035600     03  IO-AREA4                PIC X(100)   VALUE SPACE.                
035700                                                                          
035800     03  WLXXLA01 REDEFINES IO-AREA4.                                     
035900*        05 -COPY WDGX4825                                                
036000     03  WLXXLA11 REDEFINES IO-AREA4.                                     
036100*        05 -COPY WDGX4826                                                
036200                                                                          
036300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
036400 01   DLI-IO-AREA-B601.                                                   
036500*     03  -COPY WDB601                                                    
036600     EJECT                                                                
036610 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
036620 01  DLI-IO-WDL601.                                                       
036630     03  -COPY WDL601                                                     
036640     EJECT                                                                
036650 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
036660 01  DLI-IO-WDL611.                                                       
036670     03  -COPY WDL611                                                     
036680     EJECT                                                                
036800 LINKAGE SECTION.                                                         
037100     EJECT                                                                
037200*01  -COPY W0008      -PRE LEVA-                                          
037300     05  FILLER                  PIC X.                                   
037400     EJECT                                                                
037500*01  -COPY W0008      -PRE KVAE-                                          
037600     05  FILLER                  PIC X.                                   
037700     EJECT                                                                
037800*01  -COPY W0008      -PRE BENA-                                          
037900     05  FILLER                  PIC X.                                   
038000     EJECT                                                                
038100*01  -COPY W0008      -PRE XXLA-                                          
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400*01  -COPY W0008      -PRE WDB6-                                          
038500     05  FILLER                  PIC X.                                   
038510*01  -COPY W0008      -PRE WDL6-                                          
038520     05  FILLER                  PIC X.                                   
038600     EJECT                                                                
038700 PROCEDURE DIVISION  USING LEVA-PCB                                       
038800                           KVAE-PCB                                       
038900                           BENA-PCB                                       
039000                           XXLA-PCB                                       
039100                           WDB6-PCB                                       
039200                           WDL6-PCB.                                      
039300                                                                          
039400     ENTRY 'DLITCBL' USING LEVA-PCB                                       
039500                           KVAE-PCB                                       
039600                           BENA-PCB                                       
039700                           XXLA-PCB                                       
039710                           WDB6-PCB                                       
039720                           WDL6-PCB.                                      
039900     PERFORM A-INIT                                                       
040000                                                                          
040100     PERFORM S01-READ-W42690                                              
040200                                                                          
040300     PERFORM UNTIL END-OF-W42690                                          
040400       PERFORM B-LAES-OCH-BEHANDLA                                        
040500       PERFORM S01-READ-W42690                                            
040600     END-PERFORM                                                          
040700                                                                          
040800     PERFORM Z-FINIT                                                      
040900                                                                          
041000     MOVE ZERO TO RETURN-CODE                                             
041100     GOBACK                                                               
041200     .                                                                    
041300     EJECT                                                                
041400 A-INIT SECTION.                                                          
041500     OPEN INPUT  W42690                                                   
041510     OPEN OUTPUT W42625                                                   
041600                                                                          
041700     MOVE NEJ TO W42690-EOF-SW                                            
041800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
041900     ACCEPT CURRENT-DATE  FROM DATE                                       
042000     .                                                                    
042100     EJECT                                                                
042200 B-LAES-OCH-BEHANDLA SECTION.                                             
042300     PERFORM BD-BLANKA-UT-SIDA                                            
042400     MOVE INXE-KRF-IDKR            TO W-IDKR                              
042500     PERFORM IMS-GU-KVAE01                                                
042600     IF SEGMENT-FINNS                                                     
042700       PERFORM BA-INITIERA-NYCKLAR                                        
042800       PERFORM BB-REDIGERA-EKONOMI                                        
042900       PERFORM BC-SKRIV-FAKTURA                                           
043000     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 BA-INITIERA-NYCKLAR  SECTION.                                            
043400      MOVE KR-IDARTNR              TO W-IDARTNR                           
043500                                      HRAD22-IDARTNR                      
043600                                                                          
043700      MOVE KR-BEKRANS              TO HRAD9-BENAEMN                       
043800      MOVE KR-ADATTENT(1:25)       TO HRAD12-BENAEMN                      
043900      MOVE KR-IDAVINR              TO HRAD12-IDAVINR                      
044000                                                                          
044100      MOVE KR-IDKRATLF             TO HRAD10-IDTFN                        
044200                                                                          
044300      MOVE KR-IDLEVNR              TO W-IDLEVNR                           
044400                                                                          
044500      PERFORM BAA-HAMTA-BENAMNING                                         
044600                                                                          
044700      PERFORM BAB-HAMTA-ADDRESS-INFO                                      
044800                                                                          
044900      PERFORM BAC-REDIGERA-FELKOD                                         
045000                                                                          
045010      PERFORM BAD-IDKUNDRF-INFO                                           
045020                                                                          
045100      PERFORM IMS-GNP-W6KVAE14                                            
045200                                                                          
045300      IF SEGMENT-FINNS                                                    
045400        MOVE +1 TO IX                                                     
045500        PERFORM UNTIL IX > 15                                             
045600          MOVE TEXT-TEKRFEL(IX) TO HRAD50-TEKRFEL(IX)                     
045700          ADD +1                TO IX                                     
045800        END-PERFORM                                                       
045900      ELSE                                                                
046000        MOVE SPACE              TO HRAD50                                 
046100      END-IF                                                              
046200      IF KR-IDKR(1:1) = 'P'                                               
046300        IF KR-IDKR = 'PA'                                                 
046400          MOVE PA-TEXT    TO HRAD50-TEKRFEL(16)(1:27)                     
046500        ELSE                                                              
046600          MOVE PB-TEXT    TO HRAD50-TEKRFEL(16)(1:27)                     
046700        END-IF                                                            
046800        MOVE KR-KVAVIS    TO WS-KVAVIS                                    
046900        MOVE WS-KVAVIS    TO HRAD50-TEKRFEL(16)(28:6)                     
047000        MOVE P-TEXT2      TO HRAD50-TEKRFEL(16)(34:9)                     
047100        MOVE KR-KVANTMOT  TO WS-KVANTMOT                                  
047200        MOVE WS-KVANTMOT  TO HRAD50-TEKRFEL(16)(43:6)                     
047300        MOVE P-TEXT3      TO HRAD50-TEKRFEL(16)(49:4)                     
047400      END-IF                                                              
047500                                                                          
047600      IF KR-IDKRFEL(1:1) = 'P' OR 'K'                                     
047700        MOVE 'RA'                  TO HRAD5-TYP                           
047800      ELSE                                                                
047900        MOVE 'RT'                  TO HRAD5-TYP                           
048000      END-IF                                                              
048100                                                                          
048200      MOVE KR-IDKR                 TO HRAD5-IDKR                          
048300                                                                          
048400      MOVE KR-IDLEVNR              TO HRAD16-IDLEVNR                      
048500                                                                          
048600      MOVE BLANKRAD                TO HRAD-24(1)                          
048700                                      HRAD-24(2)                          
048800                                      HRAD-24(3)                          
048900                                                                          
049000      IF KR-TEKRSPEC-ATID   NOT = SPACE                                   
049100        MOVE KR-TEKRSPEC-ATID   TO HRAD24-ARB-KOSTNAD(1)                  
049200      ELSE                                                                
049300        MOVE 'LABOUR COSTS   '  TO HRAD24-ARB-KOSTNAD(1)                  
049400      END-IF                                                              
049500      IF KR-TEKRSPEC-OMK   NOT = SPACE                                    
049600        MOVE KR-TEKRSPEC-OMK    TO HRAD24-ARB-KOSTNAD(2)                  
049700      ELSE                                                                
049800        MOVE 'TRANSPORT COSTS'  TO HRAD24-ARB-KOSTNAD(2)                  
049900      END-IF                                                              
050000      IF KR-TEKRSPEC-MAT    NOT = SPACE                                   
050100        MOVE KR-TEKRSPEC-MAT      TO HRAD24-ARB-KOSTNAD(3)                
050200      ELSE                                                                
050300        MOVE 'MATERIAL COSTS   '  TO HRAD24-ARB-KOSTNAD(3)                
050400      END-IF                                                              
050500      MOVE ZERO TO   WS-ARTIKEL-BELOPP                                    
050600                     WS-TOT-EXKL-MOMS                                     
050700                     WS-SUOMK-INT                                         
050800                     WS-SUOMK-EXT                                         
050900                     WS-SUOMK-TOT                                         
051000                     WS-SUMAT                                             
051100                     WS-TOT-INKL-MOMS                                     
051200     .                                                                    
051300     EJECT                                                                
051400 BAA-HAMTA-BENAMNING SECTION.                                             
051500     PERFORM IMS-GU-BENA                                                  
051600     IF SEGMENT-FINNS                                                     
051700        MOVE TEXT-BEART TO HRAD22-BEART                                   
051800     END-IF                                                               
051900     .                                                                    
052000     EJECT                                                                
052100 BAB-HAMTA-ADDRESS-INFO SECTION.                                          
052200     PERFORM IMS-GU-LEVA                                                  
052300     IF SEGMENT-FINNS                                                     
052400       PERFORM IMS-GNP-LEVA-106                                           
052500       IF SEGMENT-FINNS                                                   
052600         MOVE ADR-BELEV            TO HRAD9-BELEV                         
052700         MOVE ADR-ADLEV-RAD1       TO HRAD10-ADLEV-RAD1                   
052800         MOVE ADR-ADLEV-RAD2       TO HRAD11-ADLEV-RAD2                   
052900         MOVE ADR-ADLEV-ORT        TO HRAD12-ADLEV-ORT                    
053000         MOVE ADR-ADLEVLND         TO HRAD13-ADLEVLND                     
053100         MOVE ADR-IDLANDX2         TO WS-IDLANDX2                         
053110         MOVE ADR-IDVAT               TO HRAD14-IDVAT                     
053200       END-IF                                                             
053300     END-IF                                                               
053400     .                                                                    
053500     EJECT                                                                
053600 BAC-REDIGERA-FELKOD SECTION.                                             
053700     MOVE BLANKRAD       TO HRAD49                                        
053800     MOVE KR-IDKRFEL     TO W-4826-IDKRFEL                                
053900                                                                          
054000     PERFORM IMS-GU-XXLA-4826                                             
054100     IF SEGMENT-FINNS                                                     
054200       MOVE 4826-BEKRFEL TO HRAD49-BEKRFEL                                
054300     END-IF                                                               
054400     .                                                                    
054500     EJECT                                                                
054510 BAD-IDKUNDRF-INFO   SECTION.                                             
054530     MOVE KR-IDARTNR     TO W-WDL601-IDARTNR                              
054550     PERFORM IMS-GU-WDL601                                                
054560     IF SEGMENT-FINNS                                                     
054592       PERFORM IMS-GNP-WDL611                                             
054593       IF SEGMENT-FINNS                                                   
054594          PERFORM UNTIL SEGMENT-SAKNAS                                    
054599           IF INL-IDDC = KR-IDDC AND                                      
054600              INL-IDLEVNR = KR-IDLEVNR                                    
054601             MOVE INL-IDKUNDRF TO HRAD16-IDKUNDRF                         
054602             MOVE 'Y' TO WS-MATCH-SW                                      
054604           END-IF                                                         
054605           PERFORM IMS-GNP-WDL611                                         
054606          END-PERFORM                                                     
054607       END-IF                                                             
054608       IF SEGMENT-SAKNAS AND WS-MATCH-SW = 'N'                            
054609          MOVE SPACES TO  HRAD16-IDKUNDRF                                 
054610       END-IF                                                             
054612     END-IF                                                               
054613     .                                                                    
054614     EJECT                                                                
054620 BB-REDIGERA-EKONOMI SECTION.                                             
054700     PERFORM IMS-GU-KVAE01                                                
054800     PERFORM IMS-GNP-KVAE12                                               
054900     IF SEGMENT-FINNS                                                     
055000                                                                          
055100       COMPUTE EK-SUOMK-INT = EK-SUOMK-INT  * -1                          
055200       COMPUTE EK-SUOMK-EXT = EK-SUOMK-EXT  * -1                          
055300       COMPUTE EK-SUMAT     = EK-SUMAT      * -1                          
055400       COMPUTE EK-PRMOMS    = EK-PRMOMS     * -1                          
055500                                                                          
055600       MOVE EK-TIFAKT        TO WS-TIFAKT                                 
055700       MOVE WS-TIFAKT        TO HRAD5-TIFAKT                              
055800                                                                          
055900       MOVE EK-IDVERNR       TO HRAD5-IDVERNR                             
056000                                                                          
056100       IF EK-KVKRRET > ZERO                                               
056200         MOVE EK-KVKRRET     TO HRAD22-KVKRRET                            
056300         MOVE EK-PRARTBEL-PR TO HRAD22-PRARTBEL-PR                        
056400       END-IF                                                             
056500                                                                          
056600       PERFORM BBA-RAEKNA-SUMMOR                                          
056700     END-IF                                                               
056800     .                                                                    
056900     EJECT                                                                
057000 BBA-RAEKNA-SUMMOR  SECTION.                                              
057100     IF EK-KVKRRET > ZERO                                                 
057200       COMPUTE WS-ARTIKEL-BELOPP ROUNDED = EK-KVKRRET *                   
057300                                  EK-PRARTBEL-PR                          
057400       MOVE WS-ARTIKEL-BELOPP TO HRAD22-ARTIKEL-BELOPP                    
057500     END-IF                                                               
057600                                                                          
057700     IF EK-SUOMK-INT NOT = ZERO                                           
057800       COMPUTE WS-SUOMK-INT ROUNDED = EK-SUOMK-INT / EK-PRKURS            
057900     END-IF                                                               
058000                                                                          
058100     IF EK-SUOMK-EXT NOT = ZERO                                           
058200       COMPUTE WS-SUOMK-EXT ROUNDED = EK-SUOMK-EXT / EK-PRKURS            
058300     END-IF                                                               
058400                                                                          
058500     COMPUTE WS-SUOMK-TOT = WS-SUOMK-INT + WS-SUOMK-EXT                   
058600                                                                          
058700     IF WS-SUOMK-INT NOT = ZERO                                           
058800       MOVE WS-SUOMK-INT       TO HRAD24-ARB-TOTAL(1)                     
058900     ELSE                                                                 
059000       MOVE BLANKRAD           TO HRAD-24(1)                              
059100     END-IF                                                               
059200                                                                          
059300     IF WS-SUOMK-EXT NOT = ZERO                                           
059400       MOVE WS-SUOMK-EXT       TO HRAD24-ARB-TOTAL(2)                     
059500     ELSE                                                                 
059600       MOVE BLANKRAD           TO HRAD-24(2)                              
059700     END-IF                                                               
059800                                                                          
059900     IF EK-SUMAT NOT = ZERO                                               
060000       COMPUTE WS-SUMAT ROUNDED = EK-SUMAT / EK-PRKURS                    
060100       MOVE WS-SUMAT           TO HRAD24-ARB-TOTAL(3)                     
060200     ELSE                                                                 
060300       MOVE BLANKRAD           TO HRAD-24(3)                              
060400     END-IF                                                               
060500                                                                          
060600     COMPUTE WS-TOT-EXKL-MOMS  = WS-SUOMK-TOT      +                      
060700                                 WS-SUMAT          +                      
060800                                 WS-ARTIKEL-BELOPP                        
060900     MOVE WS-TOT-EXKL-MOMS     TO HRAD37-SPEC-TOTAL                       
061000                                  HRAD37-SPEC-TOTAL-EU                    
061100                                                                          
061200     IF EK-PRKURS = 1.00                                                  
061300** SVENSK LEVERANTÖR                                                      
061400       COMPUTE WS-TOT-INKL-MOMS = WS-TOT-EXKL-MOMS + EK-PRMOMS            
061500       MOVE WS-TOT-INKL-MOMS   TO  HRAD43-BELOPP                          
061600                                   HRAD43-MOMS-TOTAL                      
061700     ELSE                                                                 
061800* MÅSTE RÄKNA FRAM SVENSKT TOTALPRIS                                      
061900       COMPUTE WS-TOT-SEK ROUNDED =                                       
062000          EK-SUOMK-INT + EK-SUOMK-EXT + EK-SUMAT +                        
062100          EK-PRMOMS + (EK-PRARTBEL-PR * EK-KVKRRET * EK-PRKURS)           
062200       MOVE WS-TOT-SEK         TO  HRAD43-BELOPP                          
062300       COMPUTE HRAD43-MOMS-TOTAL = (EK-PRMOMS / EK-PRKURS) +              
062400                                    WS-TOT-EXKL-MOMS                      
062500     END-IF                                                               
062600                                                                          
062610**** USA SKALL INTE HA MOMS                                               
062700     IF EK-PRMOMS NOT = ZERO                                              
062800       MOVE WS-TOT-EXKL-MOMS   TO HRAD43-MOMS-BELOPP                      
062810       IF KR-IDFTG = 60                                                   
062820         IF CURRENT-YY = 18                                               
062830         AND EK-IDVERNR < 60000080                                        
063000           MOVE MOMS-SATS-CN-1   TO HRAD43-MOMS-SATS                      
063001         ELSE                                                             
063002           IF CURRENT-YY = 19                                             
063003           AND EK-IDVERNR < 60050026                                      
063004             MOVE MOMS-SATS-CN-2 TO HRAD43-MOMS-SATS                      
063010           ELSE                                                           
063011             MOVE MOMS-SATS-CN-3 TO HRAD43-MOMS-SATS                      
063020           END-IF                                                         
063100         END-IF                                                           
063101       END-IF                                                             
063110       IF KR-IDFTG = 57                                                   
063200         MOVE MOMS-SATS        TO HRAD43-MOMS-SATS                        
063300       END-IF                                                             
063310       IF KR-IDFTG = 53                                                   
063320         MOVE MOMS-SATS-US     TO HRAD43-MOMS-SATS                        
063330       END-IF                                                             
063400       COMPUTE HRAD43-PRMOMS = EK-PRMOMS / EK-PRKURS                      
063500     END-IF                                                               
063600     MOVE EK-KDVALISO          TO  HRAD43-KDVALISO                        
063700     .                                                                    
063800     EJECT                                                                
063900 BC-SKRIV-FAKTURA  SECTION.                                               
064000**** HEADER TILL D&P                                                      
064500                                                                          
064810     MOVE SPACE                      TO WS-IDOUTREC                       
064900                                                                          
065000     MOVE 'W42625-001'               TO WS-IDOUTTYPE                      
065100     IF KR-IDFTG = 60                                                     
065200       MOVE 'CN'                     TO WS-IDOUTREC(1:2)                  
065210       MOVE 'CN05'                   TO WS-META-MARKET                    
065300     END-IF                                                               
065400     IF KR-IDFTG = 57                                                     
065401       MOVE 'SE'                     TO WS-IDOUTREC(1:2)                  
065402       MOVE 'SEPV'                   TO WS-META-MARKET                    
065500     END-IF                                                               
065510     IF KR-IDFTG = 53                                                     
065520       MOVE 'US'                     TO WS-IDOUTREC(1:2)                  
065521       MOVE 'US01'                   TO WS-META-MARKET                    
065530     END-IF                                                               
065600     IF KR-IDKRFEL(1:1) = 'P' OR 'K'                                      
065700       MOVE 'RA'                     TO WS-IDOUTREC(3:2)                  
065800     ELSE                                                                 
065900       MOVE 'RT'                     TO WS-IDOUTREC(3:2)                  
066000     END-IF                                                               
066100     MOVE W-IDLEVNR                  TO WS-IDOUTREC(5:5)                  
066300     MOVE EK-IDVERNR                 TO WS-IDLIST                         
066400                                                                          
066510     PERFORM S90-WRITE-HEADER                                             
066600                                                                          
066900     PERFORM IMS-GU-KVAE01                                                
067000     PERFORM IMS-GNP-KVAE11                                               
067100     IF SEGMENT-FINNS                                                     
067200       MOVE WS-IDLANDX2  TO LEV02-IDLANDX2                                
067300                            LEVEU-IDLANDX2                                
067400       IF NOT LEV02-IDLANDX2-SVE AND LEVEU-IDLANDX2-EU                    
067500         PERFORM IMS-GU-WDB601                                            
067510         IF DCS-USA                                                       
067520            MOVE SPACES TO HRAD36-VAT-NO                                  
067530         ELSE                                                             
067540            MOVE DCS-IDVAT TO HRAD36-VAT-NO                               
067600         END-IF                                                           
067700         PERFORM BCB-SKRIV-SIDA-ETT-EU                                    
067800       ELSE                                                               
067900         PERFORM BCA-SKRIV-SIDA-ETT                                       
068000       END-IF                                                             
068100     ELSE                                                                 
068200       PERFORM BCA-SKRIV-SIDA-ETT                                         
068300     END-IF                                                               
068700     .                                                                    
068800     EJECT                                                                
068900 BCA-SKRIV-SIDA-ETT SECTION.                                              
069000                                                                          
069002     PERFORM S91-SKRIV-META-S                                             
069003                                                                          
069010     PERFORM S02-SKRIV-HUVUD                                              
069020                                                                          
069100     MOVE '0'            TO STYR                                          
069200     MOVE BLANKRAD       TO W001-RAD                                      
069300     PERFORM S05-SKRIV-RAD                                                
069400                                                                          
069500     MOVE '-'            TO STYR                                          
069600     MOVE BLANKRAD       TO W001-RAD                                      
069700     PERFORM S05-SKRIV-RAD                                                
069800                                                                          
069900     MOVE ' '            TO STYR                                          
070000     MOVE    HRAD22      TO W001-RAD                                      
070100     PERFORM S05-SKRIV-RAD                                                
070200                                                                          
070300     MOVE '0'            TO STYR                                          
070400     MOVE    HRAD-24(1)  TO W001-RAD                                      
070500     PERFORM S05-SKRIV-RAD                                                
070600                                                                          
070700     MOVE ' '            TO STYR                                          
070800     MOVE    HRAD-24(2)  TO W001-RAD                                      
070900     PERFORM S05-SKRIV-RAD                                                
071000                                                                          
071100     MOVE ' '            TO STYR                                          
071200     MOVE    HRAD-24(3)  TO W001-RAD                                      
071300     PERFORM S05-SKRIV-RAD                                                
071400                                                                          
071500     MOVE '0'            TO STYR                                          
071600     MOVE BLANKRAD       TO W001-RAD                                      
071700     PERFORM S05-SKRIV-RAD                                                
071800                                                                          
071900     MOVE ' '            TO STYR                                          
072000     MOVE BLANKRAD       TO W001-RAD                                      
072100     PERFORM S05-SKRIV-RAD                                                
072200                                                                          
072300     MOVE '0'            TO STYR                                          
072400     MOVE    HRAD37      TO W001-RAD                                      
072500     PERFORM S05-SKRIV-RAD                                                
072600                                                                          
072700     MOVE '0'            TO STYR                                          
072800     MOVE BLANKRAD       TO W001-RAD                                      
072900     PERFORM S05-SKRIV-RAD                                                
073000                                                                          
073100     MOVE '0'            TO STYR                                          
073200     MOVE BLANKRAD       TO W001-RAD                                      
073300     PERFORM S05-SKRIV-RAD                                                
073400                                                                          
073500     MOVE ' '            TO STYR                                          
073600     MOVE    HRAD43      TO W001-RAD                                      
073700     PERFORM S05-SKRIV-RAD                                                
073800                                                                          
073900     MOVE '-'            TO STYR                                          
074000     MOVE BLANKRAD       TO W001-RAD                                      
074100     PERFORM S05-SKRIV-RAD                                                
074200                                                                          
074300     MOVE '-'            TO STYR                                          
074400     MOVE BLANKRAD       TO W001-RAD                                      
074500     PERFORM S05-SKRIV-RAD                                                
074600                                                                          
074700     MOVE ' '            TO STYR                                          
074800     MOVE    HRAD49      TO W001-RAD                                      
074900     PERFORM S05-SKRIV-RAD                                                
075000                                                                          
075100     MOVE ' '            TO STYR                                          
075200     MOVE    HRAD-50(1)  TO W001-RAD                                      
075300     PERFORM S05-SKRIV-RAD                                                
075400                                                                          
075500     MOVE ' '            TO STYR                                          
075600     MOVE    HRAD-50(2)  TO W001-RAD                                      
075700     PERFORM S05-SKRIV-RAD                                                
075800                                                                          
075900     MOVE ' '            TO STYR                                          
076000     MOVE    HRAD-50(3)  TO W001-RAD                                      
076100     PERFORM S05-SKRIV-RAD                                                
076200                                                                          
076300     MOVE ' '            TO STYR                                          
076400     MOVE    HRAD-50(4)  TO W001-RAD                                      
076500     PERFORM S05-SKRIV-RAD                                                
076600                                                                          
076700     MOVE ' '            TO STYR                                          
076800     MOVE    HRAD-50(5)  TO W001-RAD                                      
076900     PERFORM S05-SKRIV-RAD                                                
077000                                                                          
077100     MOVE ' '            TO STYR                                          
077200     MOVE    HRAD-50(6)  TO W001-RAD                                      
077300     PERFORM S05-SKRIV-RAD                                                
077400                                                                          
077500     MOVE ' '            TO STYR                                          
077600     MOVE    HRAD-50(7)  TO W001-RAD                                      
077700     PERFORM S05-SKRIV-RAD                                                
077800                                                                          
077900     MOVE ' '            TO STYR                                          
078000     MOVE    HRAD-50(8)  TO W001-RAD                                      
078100     PERFORM S05-SKRIV-RAD                                                
078200                                                                          
078300     MOVE ' '            TO STYR                                          
078400     MOVE    HRAD-50(9)  TO W001-RAD                                      
078500     PERFORM S05-SKRIV-RAD                                                
078600                                                                          
078700     MOVE ' '            TO STYR                                          
078800     MOVE    HRAD-50(10) TO W001-RAD                                      
078900     PERFORM S05-SKRIV-RAD                                                
079000                                                                          
079100     MOVE ' '            TO STYR                                          
079200     MOVE    HRAD-50(11) TO W001-RAD                                      
079300     PERFORM S05-SKRIV-RAD                                                
079400                                                                          
079500     MOVE ' '            TO STYR                                          
079600     MOVE    HRAD-50(12) TO W001-RAD                                      
079700     PERFORM S05-SKRIV-RAD                                                
079800                                                                          
079900     MOVE ' '            TO STYR                                          
080000     MOVE    HRAD-50(13) TO W001-RAD                                      
080100     PERFORM S05-SKRIV-RAD                                                
080200                                                                          
080300     MOVE ' '            TO STYR                                          
080400     MOVE    HRAD-50(14) TO W001-RAD                                      
080500     PERFORM S05-SKRIV-RAD                                                
080600                                                                          
080700     MOVE ' '            TO STYR                                          
080800     MOVE    HRAD-50(15) TO W001-RAD                                      
080900     PERFORM S05-SKRIV-RAD                                                
081000     .                                                                    
081100     EJECT                                                                
081200 BCB-SKRIV-SIDA-ETT-EU SECTION.                                           
081300                                                                          
081312     PERFORM S91-SKRIV-META-S                                             
081320                                                                          
081330     PERFORM S02-SKRIV-HUVUD                                              
081340                                                                          
081400     MOVE '0'            TO STYR                                          
081500     MOVE BLANKRAD       TO W001-RAD                                      
081600     PERFORM S05-SKRIV-RAD                                                
081700                                                                          
081800     MOVE '-'            TO STYR                                          
081900     MOVE BLANKRAD       TO W001-RAD                                      
082000     PERFORM S05-SKRIV-RAD                                                
082100                                                                          
082200     MOVE ' '            TO STYR                                          
082300     MOVE    HRAD22      TO W001-RAD                                      
082400     PERFORM S05-SKRIV-RAD                                                
082500                                                                          
082600     MOVE '0'            TO STYR                                          
082700     MOVE    HRAD-24(1)  TO W001-RAD                                      
082800     PERFORM S05-SKRIV-RAD                                                
082900                                                                          
083000     MOVE ' '            TO STYR                                          
083100     MOVE    HRAD-24(2)  TO W001-RAD                                      
083200     PERFORM S05-SKRIV-RAD                                                
083300                                                                          
083400     MOVE ' '            TO STYR                                          
083500     MOVE    HRAD-24(3)  TO W001-RAD                                      
083600     PERFORM S05-SKRIV-RAD                                                
083700                                                                          
083800     MOVE '0'            TO STYR                                          
083900     MOVE BLANKRAD       TO W001-RAD                                      
084000     PERFORM S05-SKRIV-RAD                                                
084100                                                                          
084200     MOVE ' '            TO STYR                                          
084300     MOVE BLANKRAD       TO W001-RAD                                      
084400     PERFORM S05-SKRIV-RAD                                                
084500                                                                          
084600     MOVE ' '            TO STYR                                          
084700     MOVE    HRAD36      TO W001-RAD                                      
084800     PERFORM S05-SKRIV-RAD                                                
084900                                                                          
085000     MOVE ' '            TO STYR                                          
085100     MOVE    HRAD37-EU   TO W001-RAD                                      
085200     PERFORM S05-SKRIV-RAD                                                
085300                                                                          
085400     MOVE '0'            TO STYR                                          
085500     MOVE BLANKRAD       TO W001-RAD                                      
085600     PERFORM S05-SKRIV-RAD                                                
085700                                                                          
085800     MOVE '0'            TO STYR                                          
085900     MOVE BLANKRAD       TO W001-RAD                                      
086000     PERFORM S05-SKRIV-RAD                                                
086100                                                                          
086200     MOVE ' '            TO STYR                                          
086300     MOVE    HRAD43      TO W001-RAD                                      
086400     PERFORM S05-SKRIV-RAD                                                
086500                                                                          
086600     MOVE '-'            TO STYR                                          
086700     MOVE BLANKRAD       TO W001-RAD                                      
086800     PERFORM S05-SKRIV-RAD                                                
086900                                                                          
087000     MOVE '-'            TO STYR                                          
087100     MOVE BLANKRAD       TO W001-RAD                                      
087200     PERFORM S05-SKRIV-RAD                                                
087300                                                                          
087400     MOVE ' '            TO STYR                                          
087500     MOVE    HRAD49      TO W001-RAD                                      
087600     PERFORM S05-SKRIV-RAD                                                
087700                                                                          
087800     MOVE ' '            TO STYR                                          
087900     MOVE    HRAD-50(1)  TO W001-RAD                                      
088000     PERFORM S05-SKRIV-RAD                                                
088100                                                                          
088200     MOVE ' '            TO STYR                                          
088300     MOVE    HRAD-50(2)  TO W001-RAD                                      
088400     PERFORM S05-SKRIV-RAD                                                
088500                                                                          
088600     MOVE ' '            TO STYR                                          
088700     MOVE    HRAD-50(3)  TO W001-RAD                                      
088800     PERFORM S05-SKRIV-RAD                                                
088900                                                                          
089000     MOVE ' '            TO STYR                                          
089100     MOVE    HRAD-50(4)  TO W001-RAD                                      
089200     PERFORM S05-SKRIV-RAD                                                
089300                                                                          
089400     MOVE ' '            TO STYR                                          
089500     MOVE    HRAD-50(5)  TO W001-RAD                                      
089600     PERFORM S05-SKRIV-RAD                                                
089700                                                                          
089800     MOVE ' '            TO STYR                                          
089900     MOVE    HRAD-50(6)  TO W001-RAD                                      
090000     PERFORM S05-SKRIV-RAD                                                
090100                                                                          
090200     MOVE ' '            TO STYR                                          
090300     MOVE    HRAD-50(7)  TO W001-RAD                                      
090400     PERFORM S05-SKRIV-RAD                                                
090500                                                                          
090600     MOVE ' '            TO STYR                                          
090700     MOVE    HRAD-50(8)  TO W001-RAD                                      
090800     PERFORM S05-SKRIV-RAD                                                
090900                                                                          
091000     MOVE ' '            TO STYR                                          
091100     MOVE    HRAD-50(9)  TO W001-RAD                                      
091200     PERFORM S05-SKRIV-RAD                                                
091300                                                                          
091400     MOVE ' '            TO STYR                                          
091500     MOVE    HRAD-50(10) TO W001-RAD                                      
091600     PERFORM S05-SKRIV-RAD                                                
091700                                                                          
091800     MOVE ' '            TO STYR                                          
091900     MOVE    HRAD-50(11) TO W001-RAD                                      
092000     PERFORM S05-SKRIV-RAD                                                
092100                                                                          
092200     MOVE ' '            TO STYR                                          
092300     MOVE    HRAD-50(12) TO W001-RAD                                      
092400     PERFORM S05-SKRIV-RAD                                                
092500                                                                          
092600     MOVE ' '            TO STYR                                          
092700     MOVE    HRAD-50(13) TO W001-RAD                                      
092800     PERFORM S05-SKRIV-RAD                                                
092900                                                                          
093000     MOVE ' '            TO STYR                                          
093100     MOVE    HRAD-50(14) TO W001-RAD                                      
093200     PERFORM S05-SKRIV-RAD                                                
093300                                                                          
093400     MOVE ' '            TO STYR                                          
093500     MOVE    HRAD-50(15) TO W001-RAD                                      
093600     PERFORM S05-SKRIV-RAD                                                
093700     .                                                                    
093800     EJECT                                                                
093900 BD-BLANKA-UT-SIDA SECTION.                                               
094000     MOVE SPACE TO HRAD5-TIFAKT                                           
094100                   HRAD5-TYP                                              
094200                   HRAD9-BELEV                                            
094300                   HRAD9-BENAEMN                                          
094400                   HRAD10-ADLEV-RAD1                                      
094500                   HRAD10-IDTFN                                           
094600                   HRAD11-ADLEV-RAD2                                      
094700                   HRAD12-ADLEV-ORT                                       
094800                   HRAD12-BENAEMN                                         
094900                   HRAD13-ADLEVLND                                        
095000                   HRAD22                                                 
095100                   HRAD24-ARB-KOSTNAD(1)                                  
095200                   HRAD24-ARB-KOSTNAD(2)                                  
095300                   HRAD24-ARB-KOSTNAD(3)                                  
095400                   HRAD36-VAT-NO                                          
095500                   HRAD43-KDVALISO                                        
095600                   HRAD49-BEKRFEL                                         
095700     MOVE +1 TO IX                                                        
095800     PERFORM UNTIL IX > 15                                                
095900        MOVE SPACE TO HRAD50-TEKRFEL(IX)                                  
096000        ADD +1                TO IX                                       
096100     END-PERFORM                                                          
096200                                                                          
096300     MOVE ZERO TO  HRAD5-IDVERNR                                          
096400                   HRAD5-IDKR                                             
096500                   HRAD12-IDAVINR                                         
096600                   HRAD24-ARB-TOTAL(1)                                    
096700                   HRAD24-ARB-TOTAL(2)                                    
096800                   HRAD24-ARB-TOTAL(3)                                    
096900                   HRAD37-SPEC-TOTAL-EU                                   
097000                   HRAD37-SPEC-TOTAL                                      
097100                   HRAD43-MOMS-BELOPP                                     
097200                   HRAD43-MOMS-SATS                                       
097300                   HRAD43-PRMOMS                                          
097400                   HRAD43-BELOPP                                          
097500                   HRAD43-MOMS-TOTAL                                      
097600     MOVE SPACE TO HRAD16-IDLEVNR                                         
097610                   HRAD14-IDVAT                                           
097700     .                                                                    
097800     EJECT                                                                
097900 S01-READ-W42690    SECTION.                                              
098000     READ W42690 RECORD INTO INXE-AREA                                    
098100     AT END                                                               
098200       MOVE JA               TO W42690-EOF-SW                             
098300                                                                          
098400     NOT AT END                                                           
098500        MOVE 'W42690' TO POSTSUM-FDNAMN                                   
098600        MOVE 'W42625D1' TO POSTSUM-DDNAMN2                                
098700        MOVE INXE-KRF-IDPTYP TO POSTSUM-TRANSTYP                          
098800        CALL POSTSUM USING POSTSUM-PARM                                   
098900                                                                          
099000     END-READ                                                             
099100     .                                                                    
099200     EJECT                                                                
099300 S02-SKRIV-HUVUD SECTION.                                                 
099400     MOVE    SPACE       TO W001-RAD                                      
099500     MOVE '1'            TO STYR                                          
099600     PERFORM S04-SKRIV-NYSIDA                                             
099700                                                                          
099800     MOVE '-'            TO STYR                                          
099900     MOVE BLANKRAD       TO W001-RAD                                      
100000     PERFORM S05-SKRIV-RAD                                                
100100                                                                          
100200     MOVE ' '            TO STYR                                          
100300     MOVE    HRAD5       TO W001-RAD                                      
100400     PERFORM S05-SKRIV-RAD                                                
100500                                                                          
100600     MOVE '0'            TO STYR                                          
100700     MOVE BLANKRAD       TO W001-RAD                                      
100800     PERFORM S05-SKRIV-RAD                                                
100900                                                                          
101000     MOVE ' '            TO STYR                                          
101100     MOVE    HRAD9       TO W001-RAD                                      
101200     PERFORM S05-SKRIV-RAD                                                
101300                                                                          
101400     MOVE ' '            TO STYR                                          
101500     MOVE    HRAD10      TO W001-RAD                                      
101600     PERFORM S05-SKRIV-RAD                                                
101700                                                                          
101800     MOVE '0'            TO STYR                                          
101900     MOVE    HRAD11      TO W001-RAD                                      
102000     PERFORM S05-SKRIV-RAD                                                
102100                                                                          
102200     MOVE ' '            TO STYR                                          
102300     MOVE    HRAD12      TO W001-RAD                                      
102400     PERFORM S05-SKRIV-RAD                                                
102500                                                                          
102600     MOVE ' '            TO STYR                                          
102700     MOVE    HRAD13      TO W001-RAD                                      
102800     PERFORM S05-SKRIV-RAD                                                
102900                                                                          
103000                                                                          
103100     MOVE ' '            TO STYR                                          
103200     MOVE    BLANKRAD    TO W001-RAD                                      
103300     PERFORM S05-SKRIV-RAD                                                
103310                                                                          
103400     MOVE '0'            TO STYR                                          
103500     MOVE    HRAD16      TO W001-RAD                                      
103600     PERFORM S05-SKRIV-RAD                                                
103601                                                                          
103610     MOVE ' '            TO STYR                                          
103620     MOVE    HRAD14      TO W001-RAD                                      
103630     PERFORM S05-SKRIV-RAD                                                
103700     .                                                                    
103800     EJECT                                                                
103900 S04-SKRIV-NYSIDA SECTION.                                                
104000     MOVE STYR TO RAD-507-SKIP                                            
104100     MOVE W001-RAD TO RAD-W4262502                                        
104200     PERFORM S90-WRITE-DOC-LINE                                           
104300     .                                                                    
104400     EJECT                                                                
104500 S05-SKRIV-RAD SECTION.                                                   
104600     MOVE STYR TO RAD-507-SKIP                                            
104700     MOVE W001-RAD TO RAD-W4262502                                        
104800     PERFORM S90-WRITE-DOC-LINE                                           
104900     .                                                                    
105000     EJECT                                                                
105010 S90-WRITE-DOC-LINE SECTION.                                              
105011                                                                          
105012     WRITE W42625-001   FROM DOC-LINE-AREA                                
105013     .                                                                    
105014     EJECT                                                                
105015 S90-WRITE-HEADER SECTION.                                                
105020                                                                          
105030     PERFORM S91-SKRIV-DAP1-S                                             
105040     PERFORM S91-SKRIV-DAP2-S                                             
105050     PERFORM S91-SKRIV-DAP3-S                                             
105060     .                                                                    
105070     EJECT                                                                
105100 S91-SKRIV-DAP1-S SECTION.                                                
105200                                                                          
105300     STRING ' ¤DAP' WS-HDR-AREA-1                                         
105400            DELIMITED BY SIZE INTO W001-DAP                               
105500     WRITE W42625-001     FROM W001-DAP                                   
105600                                                                          
105700     MOVE SPACE TO W001-DAP                                               
105800     .                                                                    
105900                                                                          
106000 S91-SKRIV-DAP2-S SECTION.                                                
106100                                                                          
106200     STRING ' ¤DAP' WS-HDR-AREA-2                                         
106300            DELIMITED BY SIZE INTO W001-DAP                               
106400     WRITE W42625-001         FROM W001-DAP                               
106500                                                                          
106600     MOVE SPACE TO W001-DAP                                               
106700     .                                                                    
106800 S91-SKRIV-DAP3-S SECTION.                                                
106900                                                                          
107000     STRING ' ¤DAP' WS-HDR-AREA-3                                         
107100            DELIMITED BY SIZE INTO W001-DAP                               
107200     WRITE W42625-001         FROM W001-DAP                               
107300                                                                          
107400     MOVE SPACE TO W001-DAP                                               
107500     .                                                                    
107600 S91-SKRIV-META-S SECTION.                                                
107610                                                                          
107670     MOVE SPACE TO W001-DAP                                               
107680                                                                          
107610     MOVE FUNCTION CURRENT-DATE (1:14) TO WS-DATE-TIME                    
107610     MOVE HRAD5-IDVERNR                TO WS-META-DOC-NO                  
107610     MOVE WS-DATE                      TO WS-META-DATE                    
107610     MOVE WS-TIME                      TO WS-META-TIME                    
107630     STRING '¤METAFile_Name=' WS-META-FILE-NAME                           
107640            DELIMITED BY SIZE INTO W001-DAP                               
107650     WRITE W42625-001         FROM W001-DAP                               
107650                                                                          
107670     MOVE SPACE TO W001-DAP                                               
107680                                                                          
107630     STRING '¤METAMarket=' WS-META-MARKET                                 
107640            DELIMITED BY SIZE INTO W001-DAP                               
107650     WRITE W42625-001         FROM W001-DAP                               
107660                                                                          
107670     MOVE SPACE TO W001-DAP                                               
107680                                                                          
107800     STRING '¤METADocument_Number=' HRAD5-IDVERNR                         
107900            DELIMITED BY SIZE INTO W001-DAP                               
108000     WRITE W42625-001         FROM W001-DAP                               
108100                                                                          
108200     MOVE SPACE TO W001-DAP                                               
108210                                                                          
108220     STRING '¤METAIR_Number=' HRAD5-TYP HRAD5-IDKR                        
108230            DELIMITED BY SIZE INTO W001-DAP                               
108240     WRITE W42625-001         FROM W001-DAP                               
108250                                                                          
108260     MOVE SPACE TO W001-DAP                                               
108270                                                                          
108280     STRING '¤METADate=' HRAD5-TIFAKT                                     
108290            DELIMITED BY SIZE INTO W001-DAP                               
108291     WRITE W42625-001         FROM W001-DAP                               
108292                                                                          
108293     MOVE SPACE TO W001-DAP                                               
108294                                                                          
108295     STRING '¤METASupplier=' HRAD16-IDLEVNR                               
108296            DELIMITED BY SIZE INTO W001-DAP                               
108297     WRITE W42625-001         FROM W001-DAP                               
108298                                                                          
108299     MOVE SPACE TO W001-DAP                                               
108300                                                                          
108310     STRING '¤METASupplier_name=' HRAD9-BELEV                             
108320            DELIMITED BY SIZE INTO W001-DAP                               
108330     WRITE W42625-001         FROM W001-DAP                               
108340                                                                          
108350     MOVE SPACE TO W001-DAP                                               
108360                                                                          
108361     IF HRAD16-IDKUNDRF NOT = (LOW-VALUES AND SPACES)                     
108362        MOVE HRAD16-IDKUNDRF  TO WS-META-ORDER-NO                         
108363     END-IF                                                               
108370     STRING '¤METAOrder_number=' WS-META-ORDER-NO                         
108380            DELIMITED BY SIZE INTO W001-DAP                               
108390     WRITE W42625-001         FROM W001-DAP                               
108391                                                                          
108392     MOVE SPACE TO W001-DAP                                               
108400     .                                                                    
108500                                                                          
110000 Z-FINIT      SECTION.                                                    
110100                                                                          
110200     CLOSE W42690                                                         
110300     SKIP2                                                                
110400     MOVE 'S' TO POSTSUM-OPKOD                                            
110500     CALL POSTSUM USING POSTSUM-PARM                                      
110600     .                                                                    
110700     EJECT                                                                
110800                                                                          
110900* --- IMS SEKTIONER ---                                                   
111000                                                                          
111100 IMS-GU-BENA SECTION.                                                     
111200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
111300          DELIMITED BY SIZE INTO SSA1                                     
111400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
111500          DELIMITED BY SIZE INTO SSA2                                     
111600     MOVE '  GE' TO GODK-STATUSKODER                                      
111700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA3 SSA1 SSA2                
111800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
111900     PERFORM IMS-STATUSKONTROLL                                           
112000     .                                                                    
112100     EJECT                                                                
112110 IMS-GU-WDL601    SECTION.                                                
112120                                                                          
112130     STRING 'WDL601  (IDARTNR  =' W-WDL601-IDARTNR-X ')'                  
112140          DELIMITED BY SIZE INTO SSA1                                     
112150     MOVE '  GE'           TO GODK-STATUSKODER                            
112160     CALL CBLTDLI          USING GU WDL6-PCB DLI-IO-WDL601 SSA1           
112170     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
112180     PERFORM IMS-STATUSKONTROLL                                           
112190     .                                                                    
112191     EJECT                                                                
112192 IMS-GNP-WDL611   SECTION.                                                
112193     STRING 'WDL611  (DAINLEV =>' W-WDL611-DAINLEV-X ')'                  
112194          DELIMITED BY SIZE INTO SSA1                                     
112195     MOVE '  GE'           TO GODK-STATUSKODER                            
112196     CALL CBLTDLI          USING GNP WDL6-PCB DLI-IO-WDL611   SSA1        
112197     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
112198     PERFORM IMS-STATUSKONTROLL                                           
112199     .                                                                    
112200     EJECT                                                                
112210 IMS-GU-KVAE01 SECTION.                                                   
112300     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
112400          DELIMITED BY SIZE INTO SSA1                                     
112500     MOVE '  GE' TO GODK-STATUSKODER                                      
112600     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA01 SSA1                    
112700     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
112800     PERFORM IMS-STATUSKONTROLL                                           
112900     .                                                                    
113000     EJECT                                                                
113100 IMS-GNP-KVAE11 SECTION.                                                  
113200     MOVE 'W6KVAE11 ' TO SSA1                                             
113300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
113400     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA11 SSA1                   
113500     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
113600     PERFORM IMS-STATUSKONTROLL                                           
113700     .                                                                    
113800     EJECT                                                                
113900 IMS-GNP-KVAE12 SECTION.                                                  
114000     MOVE 'W6KVAE12 ' TO SSA1                                             
114100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
114200     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA12 SSA1                   
114300     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
114400     PERFORM IMS-STATUSKONTROLL                                           
114500     .                                                                    
114600     EJECT                                                                
114700 IMS-GNP-W6KVAE14 SECTION.                                                
114800     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
114900          DELIMITED BY SIZE INTO SSA1                                     
115000     MOVE 'W6KVAE14 '         TO SSA2                                     
115100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
115200     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA14 SSA1 SSA2              
115300     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
115400     PERFORM IMS-STATUSKONTROLL                                           
115500     .                                                                    
115600     EJECT                                                                
115700 IMS-GU-LEVA SECTION.                                                     
115800     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
115900          DELIMITED BY SIZE INTO SSA1                                     
116000     MOVE '  GE' TO GODK-STATUSKODER                                      
116100     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA2 SSA1                     
116200     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
116300     PERFORM IMS-STATUSKONTROLL                                           
116400     .                                                                    
116500     EJECT                                                                
116600 IMS-GNP-LEVA-106 SECTION.                                                
116700     MOVE 'WLLEVA14 ' TO SSA1                                             
116800     MOVE '  GE' TO GODK-STATUSKODER                                      
116900     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA2 SSA1                    
117000     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
117100     PERFORM IMS-STATUSKONTROLL                                           
117200     .                                                                    
117300     EJECT                                                                
117400 IMS-GU-XXLA-4826 SECTION.                                                
117500     STRING 'WLXXLA01(WDGXKEY  =' W-4825-IDHTYP-X ')'                     
117600          DELIMITED BY SIZE INTO SSA1                                     
117700     STRING 'WLXXLA11(WDGXKEY  =' W-4826-X ')'                            
117800          DELIMITED BY SIZE INTO SSA2                                     
117900     MOVE '  GE' TO GODK-STATUSKODER                                      
118000     CALL CBLTDLI USING GU XXLA-PCB DLI-IO-AREA4 SSA1 SSA2                
118100     MOVE XXLA-STATUS-CODE TO STATUS-WS                                   
118200     PERFORM IMS-STATUSKONTROLL                                           
118300     .                                                                    
118400     EJECT                                                                
118500 IMS-GU-WDB601    SECTION.                                                
118600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
118700          DELIMITED BY SIZE INTO SSA1                                     
118800     MOVE '  ' TO GODK-STATUSKODER                                        
118900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
119000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
119100     PERFORM IMS-STATUSKONTROLL                                           
119200     .                                                                    
119300     EJECT                                                                
119400 IMS-STATUSKONTROLL SECTION.                                              
119500     SET STATUS-IX TO 1                                                   
119600     SEARCH GODK-STATUS                                                   
119700       AT END CALL FELLOG                                                 
119800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
119900     END-SEARCH                                                           
120000     .                                                                    
