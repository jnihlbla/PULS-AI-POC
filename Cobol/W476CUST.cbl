000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W476CUST.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   21/09/03.                                                
000500 DATE-COMPILED.                                                           
000610                                                                          
000700*    FUNCTION:                                                            
000800*        SUBPROGRAM TO WRITE 'CUSTOM INFORMATION' TRANSPORT               
000900*        DOCUMENT FOR DUBAI (IDDC = 87).                                  
001000*        IT IS CALLED BY A PROGRAM W40631. DOCUMENT                       
001100*        SHOWS STAT.NR., COUNTRY OF ORIGIN, DELIVERED QUANTITY,           
001200*        NET WEIGHT AND TOTAL.                                            
001300*        AT THE END OF THE DOCUMENT IT SHOWS THE TOTAL FOR THE            
001400*        FULL SHIPMENT/DIST                                               
001500*                                                                         
001600*        THE PROGRAM READS     WDE1                                       
001700*                                                                         
001800*    ABENDCODES:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)   VALUE 'W476CUST'.             
003800 01  ERRTEXT.                                                             
003900     03 FILLER                   PIC X(8)   VALUE SPACE.                  
004000     03 ERRTEXT-STR              PIC X(72)  VALUE SPACE.                  
004100 77  KDRC-DISPLAY                PIC Z(5).                                
004200                                                                          
004300 77  YES                         PIC X      VALUE 'J'.                    
004400 77  NOO                         PIC X      VALUE 'N'.                    
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  IA                          PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  IB                          PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  ISM                         PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  WS-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
005000 77  IXURS                       PIC S9(4)  COMP   VALUE ZERO.            
005100 77  IXSTATNR                    PIC S9(4)  COMP   VALUE ZERO.            
005200 77  IXARTURS                    PIC S9(4)  COMP   VALUE ZERO.            
005300*77  TOTAL-VKORDNTO              PIC S9(11)V9 COMP-3  VALUE ZERO.         
005400 77  TOTAL-VKORDNTO             PIC S9(11)V999 COMP-3 VALUE ZERO.         
005500*77  TOTAL-VKORDBTO              PIC S9(11)V9 COMP-3  VALUE ZERO.         
005600 77  TOTAL-VKORDBTO              PIC S9(11)V999 COMP-3 VALUE ZERO.        
005700*77  TOTAL2-VKORDBTO             PIC S9(11)V9 COMP-3 VALUE ZERO.          
005800 77  TOTAL2-VKORDBTO             PIC S9(11)V999 COMP-3 VALUE ZERO.        
005900*77  TOTAL3-VKORDBTO             PIC S9(11)V9 COMP-3  VALUE ZERO.         
006000 77  TOTAL3-VKORDBTO             PIC S9(11)V999 COMP-3 VALUE ZERO.        
006100*77  WX-BTO                      PIC S9(11)V99 COMP-3 VALUE ZERO.         
006200 77  WX-BTO                      PIC S9(11)V999 COMP-3 VALUE ZERO.        
006300 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
006400 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
006500 77  WS-KDARTURS                 PIC X(3)    VALUE SPACE.                 
006600 77  WS-TOT-TILLAGG              PIC S9(11)V99 COMP-3 VALUE ZERO.         
006700 77  WS-TOT-TILLAGG-USD          PIC S9(11)V99 COMP-3 VALUE ZERO.         
006800 77  SHIP-INDX                   PIC S9(9)   VALUE +15.                   
006900                                                                          
007000*    --- GENERELL STATISTICAL NUMBER FOR DUBAI                            
007100 77  GEN-IDSTATNR                PIC S9(9) COMP-3 VALUE 87089900.         
007200                                                                          
007300 77  IXURS-PRESENT-SW            PIC X(1)   VALUE 'N'.                    
007400     88  IXURS-PRESENT                      VALUE 'J'.                    
007500                                                                          
007600 77  W-PAGE-NO                   PIC S9(3)  COMP-3 VALUE ZERO.            
007700*                                                                         
007800 01  FILLER                      PIC X(16)  VALUE 'WNDCADRE '.            
007900*   -COPY WNDCADRE                                                        
008000     EJECT                                                                
008100 01  TEST-IDDISTR               PIC 9(5)   COMP-3.                        
008200*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
008201*01  FILLER   -COPY WWDIST20    -RED TEST-IDDISTR.                        
008210*01  FILLER   -COPY WWDIST34    -RED TEST-IDDISTR.                        
008300*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
008400     EJECT                                                                
008500*01    -COPY WWDC99                                                       
008600                                                                          
008700                                                                          
008800 01  W-YYMMDD                    PIC 9(06)  VALUE ZERO.                   
008900                                                                          
009000 01  W-LINE.                                                              
009100     03  W-LINE-COUNT            PIC 9(02)  VALUE ZERO.                   
009200     03  W-LINE-MAX              PIC 9(02)  VALUE 41.                     
009300                                                                          
009400*    --- STYRTECKEN PRINTER                                               
009500 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
009600 01  WS-SKIP1                      PIC X      VALUE ' '.                  
009700 01  WS-SKIP2                      PIC X      VALUE '0'.                  
009800 01  WS-SKIP3                      PIC X      VALUE '-'.                  
009900                                                                          
010000 01  WS-TYP-IDSHIP                 PIC X(18) VALUE                        
010100                                   'CUSTOM INFORMATION'.                  
010200                                                                          
010300 01  TODAYS-DATE                 PIC 9(6)   VALUE ZERO.                   
010400 01  FILLER REDEFINES TODAYS-DATE.                                        
010500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
010600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
010700     03  TODAYS-DATE-DAY         PIC 9(2).                                
010800     EJECT                                                                
010900 77  FLLOCCUR-SW                 PIC X(01)  VALUE 'N'.                    
011000     88 FLLOCCUR                            VALUE 'J'.                    
011100                                                                          
011200 77  FLFIRSTTIME-SW              PIC X(01)  VALUE 'N'.                    
011300     88 FLFIRSTTIME                         VALUE 'J'.                    
011400                                                                          
011500 01  WS-MONEY                    PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
011600 01  WY-MONEY                    PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
011700 01  WS-REVALUTA-LOCCUR          PIC S9(5)      COMP-3 VALUE 1.           
011800 01  WS-PRKURS-LOCCUR            PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
011900 01  WS-PRKURS-INR               PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
012000 01  WS-PRKURS-USD               PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
012100 01  ST-URS-TOT-SUORDV-TOT-INR   PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
012200 01  ST-URS-SUBTOT-SUORDV-TOT-INR                                         
012300                                 PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
012400 01  ST-URS-SUORDV-TOT-INR       PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
012500                                                                          
012600 01  ARB-RAD                     PIC X(132)  VALUE SPACE.                 
012700                                                                          
012800 01  RAD-HEAD.                                                            
012900     03  FILLER                  PIC X(15).                               
013000     03  RAD1H-INVOICE-TEXT      PIC X(08).                               
013100     03  FILLER                  PIC X(01)    VALUE X'05'.                
013200     03  RAD1H-INVOICE           PIC X(10).                               
013300     03  FILLER                  PIC X(01)    VALUE X'05'.                
013400                                                                          
013500 01  RAD-2.                                                               
013600     05  FILLER                  PIC X(1).                                
013700     05  RAD2-IDSTATNR           PIC X(9).                                
013800     05  FILLER                  PIC X(01)    VALUE X'05'.                
013900     05  RAD2-DESCRIPTION        PIC X(08).                               
014000     05  FILLER                  PIC X(01)    VALUE X'05'.                
014100*    05  FILLER                  PIC X(3).                                
014200*    05  RAD2-KDARTURS           PIC X(3).                                
014300*    05  FILLER                  PIC X(01)    VALUE X'05'.                
014400     05  RAD2-KDARTURS-LAND      PIC X(8).                                
014500     05  FILLER                  PIC X(01)    VALUE X'05'.                
014600     05  RAD2-ASTERISK           PIC X(2).                                
014700     05  FILLER                  PIC X(01)    VALUE X'05'.                
014800*    05  FILLER                  PIC X(8).                                
014900     05  RAD2-KVART              PIC X(9)  JUSTIFIED RIGHT.               
015000     05  FILLER                  PIC X(01)    VALUE X'05'.                
015100**** 05  FILLER                  PIC X(01).                               
015200    05  FILLER                  PIC X(03).                                
015300     05  RAD2-VKORDNTO           PIC X(14) JUSTIFIED RIGHT.               
015400     05  FILLER                  PIC X(01)    VALUE X'05'.                
015500     05  FILLER                  PIC X(02).                               
015600     05  RAD2-VKORDBTO           PIC X(14) JUSTIFIED RIGHT.               
015700     05  FILLER                  PIC X(01)    VALUE X'05'.                
015800     05  FILLER                  PIC X(02).                               
015900     05  RAD2-SUORDV-TOT         PIC X(14) JUSTIFIED RIGHT.               
016000     05  FILLER                  PIC X(01)    VALUE X'05'.                
016100     05  FILLER                  PIC X(04).                               
016200     05  RAD2-KDVALISO           PIC X(08).                               
016300     05  FILLER                  PIC X(01)    VALUE X'05'.                
016400                                                                          
016500 01  RAD-3.                                                               
016600     05  FILLER                  PIC X(1).                                
016700     05  RAD3-IDSTATNR           PIC Z9(4)B9(3).                          
016800     05  FILLER                  REDEFINES RAD3-IDSTATNR.                 
016900       07 RAD3-STATNR            PIC X(09).                               
017000*    05  FILLER                  PIC X(3).                                
017100     05  FILLER                  PIC X(01)    VALUE X'05'.                
017200     05  RAD3-DESCRIPTION        PIC X(08).                               
017300     05  FILLER                  PIC X(01)    VALUE X'05'.                
017400*    05  RAD3-KDARTURS           PIC X(3).                                
017500*    05  FILLER                  PIC X(01)    VALUE X'05'.                
017600     05  RAD3-KDARTURS-LAND      PIC X(15).                               
017700*    05  FILLER                  PIC X(6).                                
017800     05  FILLER                  PIC X(01)    VALUE X'05'.                
017900     05  RAD3-ASTERISK           PIC X(2).                                
018000     05  FILLER                  PIC X(01)    VALUE X'05'.                
018100     05  RAD3-KVART              PIC Z(8)9.                               
018200*    05  FILLER                  PIC X(01).                               
018300     05  FILLER                  PIC X(01)    VALUE X'05'.                
018400*****05  RAD3-VKORDNTO           PIC Z(11)9.9(1).                         
018500     05  RAD3-VKORDNTO           PIC Z(11)9.9(3).                         
018600*    05  FILLER                  PIC X(02).                               
018700     05  FILLER                  PIC X(01)    VALUE X'05'.                
018800*****05  RAD3-VKORDBTO           PIC Z(11)9.9(1).                         
018900     05  RAD3-VKORDBTO           PIC Z(11)9.9(3).                         
019000     05  FILLER                  REDEFINES RAD3-VKORDBTO.                 
019100       07 RAD3-BTO               PIC X(16).                               
019200*    05  FILLER                  PIC X(02).                               
019300     05  FILLER                  PIC X(01)    VALUE X'05'.                
019400     05  RAD3-SUORDV-TOT         PIC Z(10)9.9(2).                         
019500*    05  FILLER                  PIC X(04).                               
019600     05  FILLER                  PIC X(01)    VALUE X'05'.                
019700     05  RAD3-KDVALISO           PIC X(03).                               
019800*    05  FILLER                  PIC X(05).                               
019900     05  FILLER                  PIC X(01)    VALUE X'05'.                
020000                                                                          
020100 01  RADY.                                                                
020200     03 FILLER                    PIC X(32).                              
020300     03 RADY-TEXT                 PIC X(30).                              
020400     03 FILLER                    PIC X(6).                               
020500     03  FILLER                  PIC X(01)    VALUE X'05'.                
020600     03 RADY-DIFF                 PIC -(8)9.99.                           
020700     03  FILLER                  PIC X(01)    VALUE X'05'.                
020800                                                                          
020900 01  RAD4.                                                                
021000     03  RAD4-TEXT               PIC X(10)    VALUE 'NET WEIGHT'.         
021100     03  FILLER                  PIC X(01)    VALUE X'05'.                
021200     03  RAD4-VKORDNTO           PIC Z(11)9.9(3).                         
021300     03  FILLER                  PIC X(01)    VALUE X'05'.                
021400                                                                          
021500 01  RAD5.                                                                
021600     03  RAD5-TEXT               PIC X(12)    VALUE 'GROSS WGHT'.         
021700     03  FILLER                  PIC X(01)    VALUE X'05'.                
021800     03  RAD5-VKORDBTO           PIC Z(11)9.9(3).                         
021900     03  FILLER                  PIC X(01)    VALUE X'05'.                
022000                                                                          
022100 01  RAD6.                                                                
022200     03  RAD6-TEXT               PIC X(10)    VALUE 'TOTAL PACK'.         
022300     03  FILLER                  PIC X(01)    VALUE X'05'.                
022400     03  RAD6-KVKOLLI            PIC Z(8)9.                               
022500     03  FILLER                  PIC X(01)    VALUE X'05'.                
022600                                                                          
022700 77  W-KDSPRAK                   PIC S9      COMP-3.                      
022800*77  W-VKVIKT                    PIC S9(08)V9(1)  VALUE ZERO.             
022900 77  W-VKVIKT                    PIC S9(08)V9(3)  VALUE ZERO.             
023000*77  W-VKVIKTB                   PIC S9(08)V9(1)  VALUE ZERO.             
023100 77  W-VKVIKTB                   PIC S9(08)V9(3)  VALUE ZERO.             
023200*77  W-VKVIKTC                   PIC S9(08)V9(1)  VALUE ZERO.             
023300 77  W-VKVIKTC                   PIC S9(08)V9(3)  VALUE ZERO.             
023400 77  W-SUPRIS-TEST               PIC S9(09)V9(5)  VALUE ZERO.             
023500 77  W-SUPRIS-TOT                PIC S9(09)V9(5)  VALUE ZERO.             
023600 77  W-SUPRIS                    PIC S9(09)V9(2)  VALUE ZERO.             
023700 77  W-SUPRIS-L                  PIC S9(09)V9(5)  VALUE ZERO.             
023800 77  W-SUPRIS-USD                PIC S9(09)V9(5)  VALUE ZERO.             
023900 77  W-KDVALISO                  PIC X(3)    VALUE SPACE.                 
024000 77  W-ST-URS-SUBTOT-KDVALISO    PIC X(3)    VALUE SPACE.                 
024100 77  W-ST-URS-TOT-KDVALISO       PIC X(3)    VALUE SPACE.                 
024200 77  DUMMY-AREA                  PIC X(50)   VALUE SPACE.                 
024300                                                                          
024400 77  KDFKBIL-SW                  PIC S9(1)   VALUE ZERO.                  
024500                                                                          
024600     88  KDFKBIL-ST                          VALUE +1.                    
024700     88  KDFKBIL-URS                         VALUE +2.                    
024800     88  KDFKBIL-SRA                         VALUE +3.                    
024900     88  KDFKBIL-ST-URS-SRA                  VALUE +4.                    
025000     88  KDFKBIL-ST-URS                      VALUE +5.                    
025100     88  KDFKBIL-ST-SRA                      VALUE +6.                    
025200     88  KDFKBIL-URS-SRA                     VALUE +7.                    
025300                                                                          
025400 77  W-CONST-99                  PIC X(2)    VALUE '99'.                  
025500                                                                          
025600 01  FILLER                      PIC X(32)  VALUE 'LEDTEXT TABLE'.        
025700* 01 -COPY W475W552                                                       
025800     EJECT                                                                
025900* 01 -COPY W476W003                                                       
026000     EJECT                                                                
026100* 01 -COPY W476W001                                                       
026200     EJECT                                                                
026300                                                                          
026400 01  W-KDVALISO-TBL.                                                      
026500     02  W-KDVALISO-POST-1 OCCURS  300.                                   
026600         03  W-KDVALISO-POST-2 OCCURS  50.                                
026700             05  W-ST-URS-KDVALISO PIC X(03).                             
026800                                                                          
026900*    --- PARAMETRAR TILL COPYTEXT W400ARTU                                
027000*01  -COPY W400ARTU                                                       
027100     EJECT                                                                
027200 01  GENERAL-SUBPROGRAMS.                                                 
027300*                                                                         
027400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
027500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
027600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
027700     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
027800     03  INTSOR                  PIC X(8)    VALUE 'INTSOR  '.            
027900     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
028000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
028100     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
028200     SKIP2                                                                
028300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
028400                                                                          
028500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
028600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
028700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
028800     SKIP2                                                                
028900     EJECT                                                                
029000*    --- PARAMETERS FOR SUBPROGRAM W510CURR                               
029100     EJECT                                                                
029200*01  -COPY W510CURR                                                       
029300*    --- PARAMETERS FOR SUBPROGRAM W006PRS1                               
029400     EJECT                                                                
029500*01  -COPY W006PRAR                                                       
029600 01  W-IDPRTLST                  PIC X(8).                                
029700     SKIP3                                                                
029800*    --- PARAMETRS  FOR SUBPROGRAM INTSOR                                 
029900 01  INTSOR-HJAELP-AREA.                                                  
030000    03  INTSOR-POST-ANTAL          PIC S9(3)  VALUE ZERO COMP-3.          
030100    03  INTSOR-POST-LAENGD         PIC S9(3)  VALUE ZERO COMP-3.          
030200    03  INTSOR-SORT-FAELT-LAENGD   PIC S9(3)  VALUE ZERO COMP-3.          
030300     SKIP3                                                                
030400*    --- AREAS FOR IMS-SECTIONS                                           
030500*                                                                         
030600     EJECT                                                                
030700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
030800     SKIP3                                                                
030900 01  KEYS-TO-DLI.                                                         
031000                                                                          
031100     03  W-IDSHIPM-X.                                                     
031200         05  W-IDSHIPM            PIC 9(7)    VALUE ZERO.                 
031300                                                                          
031400     03  W-WDE111KY-X.                                                    
031500        05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.            
031600        05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.            
031700                                                                          
031800     03  W-WDE111KY-MIN.                                                  
031900         05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.          
032000         05  FILLER               PIC X(04)   VALUE LOW-VALUES.           
032100                                                                          
032200     03  W-WDE111KY-MAX.                                                  
032300         05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.          
032400         05  FILLER               PIC X(04)   VALUE HIGH-VALUES.          
032500                                                                          
032600     03  W-WDE121KY-X.                                                    
032700        05  W-WDE121-IDPRODNR    PIC S9(07)  VALUE ZERO COMP-3.           
032800        05  W-WDE121-IDKOLLI     PIC S9(05)  VALUE ZERO COMP-3.           
032900                                                                          
033000     03  W-WDE121KY-MIN.                                                  
033100         05  W-WDE121-IDPRODNR-MIN PIC S9(07)  VALUE ZERO COMP-3.         
033200         05  W-WDE121-IDKOLLI-MIN  PIC S9(05)  VALUE ZERO COMP-3.         
033300                                                                          
033400     03  W-WDE121KY-MAX.                                                  
033500         05  W-WDE121-IDPRODNR-MAX PIC S9(07)  VALUE ZERO COMP-3.         
033600         05  W-WDE121-IDKOLLI-MAX  PIC S9(05)  VALUE ZERO COMP-3.         
033700                                                                          
033800     03  W-WDB201KY-X.                                                    
033900         05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
034000         05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
034100                                                                          
034200     03  W-WDB101KY-X.                                                    
034300         05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                 
034400         05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                  
034500                                                                          
034600     03  W-IDARTNR-X.                                                     
034700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
034800                                                                          
034900     03  W-IDSKYLT-X.                                                     
035000         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
035100*                                                                         
035200     SKIP2                                                                
035300*    --- STATUS-KOD FRÅN IMS                                              
035400 01  STATUS-WS                    PIC XX.                                 
035500     88  SEGMENT-FOUND                       VALUE '  '.                  
035600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
035700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
035800     SKIP2                                                                
035900 01  GOOD-STATUSCODES.                                                    
036000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036100     SKIP3                                                                
036200 01  SSA1                        PIC X(64).                               
036300 01  SSA2                        PIC X(64).                               
036400 01  SSA3                        PIC X(64).                               
036500 01  SSA4                        PIC X(64).                               
036600     EJECT                                                                
036700                                                                          
036800 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
036900 01  SEND-AREA.                                                           
037000*    03  -COPY WZ01SEND                                                   
037100                                                                          
037200 01  SEND-RAD-STYRTECKEN.                                                 
037300     03  STYRTECKEN-RAD          PIC X.                                   
037400     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
037500                                                                          
037600 01  DAP-AREA-START              PIC X(24)   VALUE                        
037700                                             'DAP-AREA-START'.            
037800                                                                          
037900*    --- IMS FUNCTION CODES                                               
038000*01  -COPY W0003                                                          
038100     EJECT                                                                
038200*    ---  DLI INPUT-OUTPUT AREA                                           
038300 01  FILLER         PIC X(25) VALUE 'DLI-IO-WDE101-11-21-31'.             
038400 01  DLI-IO-WDE101-11-21-31.                                              
038500     03  DLI-IO-WDE101.                                                   
038600*        05  -COPY WDE101                                                 
038700     03  DLI-IO-WDE111.                                                   
038800*        05  -COPY WDE111                                                 
038900     03  DLI-IO-WDE121.                                                   
039000*        05  -COPY WDE121                                                 
039100     03  DLI-IO-WDE122.                                                   
039200*        05  -COPY WDE122                                                 
039300     03  DLI-IO-WDE131.                                                   
039400*        05  -COPY WDE131                                                 
039500                                                                          
039600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
039700 01  DLI-IO-WDB101.                                                       
039800*    03  -COPY WDB101                                                     
039900                                                                          
040000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
040100 01  DLI-IO-WDB201.                                                       
040200*    03  -COPY WDB201                                                     
040300                                                                          
040400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
040500 01  DLI-IO-WDD301.                                                       
040600*    03  -COPY WDD301                                                     
040700                                                                          
040800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
040900 01  DLI-IO-WDD311.                                                       
041000*    03  -COPY WDD311                                                     
041100                                                                          
041200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
041300 01  DLI-IO-WDK601.                                                       
041400*    03  -COPY WDK601                                                     
041500                                                                          
041600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
041700 01  DLI-IO-WDK611.                                                       
041800*    03  -COPY WDK611                                                     
041900                                                                          
042000     EJECT                                                                
042100 LINKAGE SECTION.                                                         
042200                                                                          
042300*01  -COPY W476TRPD                                                       
042400                                                                          
042500 01  ALT-PCB                     PIC X(32).                               
042600                                                                          
042700*01  -COPY W0008  -PRE WDE1-                                              
042800     05  FILLER                  PIC X.                                   
042900                                                                          
043000*01  -COPY W0008  -PRE WDB2-                                              
043100     05  FILLER                  PIC X.                                   
043200                                                                          
043300*01  -COPY W0008  -PRE WDB1-                                              
043400     05  FILLER                  PIC X.                                   
043500                                                                          
043600*01  -COPY W0008  -PRE WDG2-                                              
043700     05  FILLER                  PIC X.                                   
043800                                                                          
043900*01  -COPY W0008  -PRE WDD3-                                              
044000     05  FILLER                  PIC X.                                   
044100                                                                          
044200*01  -COPY W0008  -PRE WDK6-                                              
044300     05  FILLER                  PIC X.                                   
044400                                                                          
044500     EJECT                                                                
044600 PROCEDURE DIVISION  USING TRPD-W476TRPD ALT-PCB                          
044700                           WDE1-PCB WDB2-PCB WDB1-PCB                     
044800                           WDG2-PCB WDD3-PCB WDK6-PCB.                    
044900 MAIN SECTION.                                                            
045000     ENTRY 'DLITCBL' USING TRPD-W476TRPD ALT-PCB                          
045100                           WDE1-PCB WDB2-PCB WDB1-PCB                     
045200                           WDG2-PCB WDD3-PCB WDK6-PCB.                    
045300                                                                          
045400     PERFORM A-INIT                                                       
045500                                                                          
045600     PERFORM S44-LAES-WDE1-TOT                                            
045700     PERFORM IMS-GU-WDE101-11                                             
045800     MOVE SGMT-IDDC           TO WS-IDDC                                  
045900                                                                          
046000*     IF (CDC-SE OR DDC-SE) AND                                           
046100*       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                        
046200*        PERFORM C-OMVANDLA-VALUTA                                        
046300*    END-IF                                                               
046400                                                                          
046500     MOVE SGMT-IDDISTR    TO W-WDE111-IDDISTR                             
046600     MOVE SGMT-IDKUNDNR   TO W-WDE111-IDKUNDNR                            
046700** KDFKBIL IS SAME FOR ALL THE CUSTOMER UNDER SAME DISTRICT               
046800     IF SEGMENT-FOUND                                                     
046900       MOVE SGMT-KDFKBIL  TO KDFKBIL-SW                                   
047000     END-IF                                                               
047100     MOVE SGMT-PRKURS     TO WS-PRKURS-LOCCUR                             
047200                                                                          
047300     MOVE SHIP-PRKURS-BET TO WS-PRKURS-USD                                
047400                                                                          
047500     PERFORM IMS-GNP-WDE122                                               
047600     IF SEGMENT-FOUND                                                     
047700       PERFORM S70-COUNT-TILLAGG                                          
047800     END-IF                                                               
047900                                                                          
048000     PERFORM UNTIL NOT SEGMENT-FOUND                                      
048100       PERFORM IMS-GNP-WDE121                                             
048200       MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                       
048300       MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                        
048400       PERFORM UNTIL NOT SEGMENT-FOUND                                    
048500        ADD SKOLLI-VKORDBTO-KOLLI  TO TOTAL2-VKORDBTO                     
048600        ADD +1                     TO ST-URS-TOT-KVKOLLI                  
048700        PERFORM IMS-GNP-WDE131                                            
048800        PERFORM UNTIL NOT SEGMENT-FOUND                                   
048900         PERFORM B-BUILD-TABLE                                            
049000         PERFORM IMS-GNP-WDE131                                           
049100        END-PERFORM                                                       
049200        PERFORM IMS-GNP-WDE121                                            
049300        MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                      
049400        MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                       
049500       END-PERFORM                                                        
049600                                                                          
049700       PERFORM IMS-GNP-WDE111                                             
049800       MOVE SGMT-IDKUNDNR     TO W-WDE111-IDKUNDNR                        
049900     END-PERFORM                                                          
050000                                                                          
050100     PERFORM S20-HAMTA-WDB2                                               
050200     PERFORM E-PRINT-DOCUMENT                                             
050300                                                                          
050400     MOVE ZERO TO RETURN-CODE                                             
050500     GOBACK                                                               
050600     .                                                                    
050700     EJECT                                                                
050800                                                                          
050900 A-INIT SECTION.                                                          
051000                                                                          
051100     MOVE LENGTH OF SEND-RAD           TO SEND-KVDLEN                     
051200     ACCEPT TODAYS-DATE  FROM DATE                                        
051300                                                                          
051400     MOVE TRPD-IDPRTLST   TO W-IDPRTLST                                   
051500     MOVE TRPD-IDSHIPM    TO W-IDSHIPM                                    
051600     MOVE TRPD-PFDEF-OVR  TO PRT-PFDEF-OVR                                
051700     MOVE TRPD-IDDISTR    TO W-WDE111-IDDISTR-MIN                         
051800                             W-WDE111-IDDISTR-MAX                         
051900                             W-WDB201-IDDISTR                             
052000                             TEST-IDDISTR                                 
052100     MOVE HIGH-VALUE      TO W-WDE121KY-MAX                               
052200     MOVE 2               TO W-KDSPRAK                                    
052300     MOVE ZERO            TO W-PAGE-NO                                    
052400                             WS-IX                                        
052500                                                                          
052600*    MOVE SPACE           TO RAD-HEAD                                     
052700*                            RAD-2                                        
052800*                            RAD-3                                        
052900     MOVE SPACE           TO RADY                                         
053000     PERFORM S87-MOVE-SPACE-RAD2                                          
053100     PERFORM S88-MOVE-SPACE-RAD3                                          
053200     PERFORM S89-MOVE-SPACE-RAD-HEAD                                      
053300                                                                          
053400** BUILD HEADER                                                           
053500     MOVE IDSTATNR-LEDTEXT   (W-KDSPRAK) TO RAD2-IDSTATNR                 
053600*    MOVE BEARTURS-LEDTEXT   (W-KDSPRAK) TO RAD2-KDARTURS                 
053700     MOVE KBIL-KVANT-LEDTEXT (W-KDSPRAK) TO RAD2-KVART                    
053800     MOVE VKORDNTO-LEDTEXT   (W-KDSPRAK) TO RAD2-VKORDNTO                 
053900     MOVE VKORDBTO-LEDTEXT   (W-KDSPRAK) TO RAD2-VKORDBTO                 
054000     MOVE KBIL-TOTAL-LEDTEXT (W-KDSPRAK) TO RAD2-SUORDV-TOT               
054100     MOVE KDVALISO-LEDTEXT   (W-KDSPRAK) TO RAD2-KDVALISO                 
054200     MOVE 'DESCRIPT'                     TO RAD2-DESCRIPTION              
054300     MOVE ' COUNTRY'                     TO RAD2-KDARTURS-LAND            
054400                                                                          
054500     MOVE ST-URS-MAX-ANTAL-STATNR        TO ST-URS-ANTAL-STATNR           
054600                                                                          
054700     MOVE +1                             TO IXSTATNR                      
054800     PERFORM UNTIL IXSTATNR > ST-URS-ANTAL-STATNR                         
054900       MOVE ALL '9'        TO ST-URS-IDSTATNR (IXSTATNR)                  
055000       MOVE ZERO           TO ST-URS-IDSTATNR-PNR (IXSTATNR)              
055100       MOVE +1             TO IXARTURS                                    
055200       PERFORM UNTIL IXARTURS > ST-URS-MAX-ANTAL-ARTURS                   
055300          IF IXSTATNR = +1                                                
055400            MOVE W-CONST-99 TO ST-URS-AKTUELL-KDARTURS (IXARTURS)         
055500          END-IF                                                          
055600          MOVE W-CONST-99 TO ST-URS-KDARTURS (IXSTATNR, IXARTURS)         
055700          MOVE ZERO TO ST-URS-KVART       (IXSTATNR, IXARTURS)            
055800                       ST-URS-VKORDNTO    (IXSTATNR, IXARTURS)            
055900                       ST-URS-VKORDBTO    (IXSTATNR, IXARTURS)            
056000                       ST-URS-SUORDV-TOT  (IXSTATNR, IXARTURS)            
056100          MOVE SPACE TO W-ST-URS-KDVALISO (IXSTATNR, IXARTURS)            
056200          ADD +1 TO IXARTURS                                              
056300       END-PERFORM                                                        
056400       ADD +1 TO IXSTATNR                                                 
056500     END-PERFORM                                                          
056600                                                                          
056700     MOVE ZERO  TO ST-URS-ANTAL-STATNR                                    
056800                   ST-URS-ANTAL-ARTURS                                    
056900                   ST-URS-TOT-KVKOLLI                                     
057000                   ST-URS-TOT-KVART                                       
057100                   ST-URS-TOT-VKORDNTO                                    
057200                   ST-URS-TOT-VKORDBTO                                    
057300                   ST-URS-TOT-SUORDV-TOT                                  
057400                   ST-URS-TOT-SUORDV-TOT-INR                              
057500                   ST-URS-SUBTOT-KVART                                    
057600                   ST-URS-SUBTOT-VKORDNTO                                 
057700                   ST-URS-SUBTOT-VKORDBTO                                 
057800                   ST-URS-SUBTOT-SUORDV-TOT                               
057900                   ST-URS-SUBTOT-SUORDV-TOT-INR                           
058000                                                                          
058100     MOVE SPACE TO W-ST-URS-SUBTOT-KDVALISO                               
058200                   W-ST-URS-TOT-KDVALISO                                  
058300     MOVE YES   TO FLFIRSTTIME-SW                                         
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700 B-BUILD-TABLE SECTION.                                                   
058800                                                                          
058900     MOVE ZERO  TO W-VKVIKT                                               
059000                   W-VKVIKTB                                              
059100                   W-SUPRIS                                               
059200                   W-SUPRIS-USD                                           
059300     MOVE SPACE TO W-KDVALISO                                             
059400                                                                          
059500     MOVE SRAD-IDARTNR  TO W-IDARTNR                                      
059600*     PERFORM S50-HAMTA-WDD3-INFO                                         
059700                                                                          
059800     PERFORM S60-LAES-WDK6                                                
059900                                                                          
060000**   COMPUTE W-VKVIKT = SRAD-VKARTNTO * SRAD-KVLEVART                     
060100     COMPUTE W-VKVIKT = SRAD-VKART-NTO-KG * SRAD-KVLEVART                 
060200                                                                          
060300     MOVE W-VKVIKT    TO W-VKVIKTC                                        
060400     IF SKOLLI-VKORDNTO-KOLLI = ZERO                                      
060500       MOVE 0.1     TO SKOLLI-VKORDNTO-KOLLI                              
060600       IF W-VKVIKTC  = ZERO                                               
060700       MOVE 0.1     TO W-VKVIKTC                                          
060800       END-IF                                                             
060900     END-IF                                                               
061000*    W-VKVIKTB ANVÄNDS FÖR BERÄKNING AV BRUTTOVIKT MEN                    
061100*    BRUTTOVIKT VISAS VISAS INTE LÄNGRE (SE RAD3-BTO)                     
061200                                                                          
061300     COMPUTE W-VKVIKTB = (W-VKVIKTC) *                                    
061400          SKOLLI-VKORDBTO-KOLLI / SKOLLI-VKORDNTO-KOLLI                   
061500                                                                          
061600     IF DIST35-CDC-AE-REFILL                                              
061700       IF SRAD-PRARTNTO > ZERO                                            
061800         COMPUTE W-SUPRIS-USD = SRAD-PRARTNTO / WS-PRKURS-USD             
061900         COMPUTE W-SUPRIS-L = W-SUPRIS-USD * SRAD-KVLEVART                
062000         COMPUTE W-SUPRIS ROUNDED =                                       
062100             W-SUPRIS-L + ((W-SUPRIS-L / W-SUPRIS-TOT) *                  
062200                  (WS-TOT-TILLAGG-USD))                                   
062300         MOVE 'USD'             TO W-KDVALISO                             
062400*        MOVE SHIP-KDVALISO-BET TO W-KDVALISO                             
062500       END-IF                                                             
062600     ELSE                                                                 
062610       IF DIST18-SCRAP-NDC-SC        OR                                   
062620          DIST18-SCRAP-NDC-QUAL      OR                                   
062630          DIST18-SCRAP-NDC-SC-LOCAL  OR                                   
062640          DIST20-EMBALLAGE-SDC       OR                                   
062670          DIST35-AE-CDC-RETURNS                                           
062672         COMPUTE W-SUPRIS-L = SRAD-PRAVCOST * SRAD-KVLEVART               
062673         ADD W-SUPRIS-L         TO W-SUPRIS                               
062677         MOVE SHIP-KDVALISO-BET TO W-KDVALISO                             
062680       ELSE                                                               
062700         IF SRAD-PRARTNTO > ZERO                                          
062800           COMPUTE W-SUPRIS-USD = SRAD-PRARTNTO / WS-PRKURS-USD           
062900           COMPUTE W-SUPRIS-L = W-SUPRIS-USD * SRAD-KVLEVART              
063000           COMPUTE W-SUPRIS ROUNDED =                                     
063100               W-SUPRIS-L + ((W-SUPRIS-L / W-SUPRIS-TOT) *                
063200                    (WS-TOT-TILLAGG-USD))                                 
063300           MOVE 'USD'           TO W-KDVALISO                             
063400         END-IF                                                           
063410       END-IF                                                             
063500     END-IF                                                               
063600     ADD W-VKVIKTB          TO TOTAL3-VKORDBTO                            
063700                                                                          
063800     IF KDFKBIL-ST              OR                                        
063900        KDFKBIL-URS             OR                                        
064000        KDFKBIL-SRA             OR                                        
064100        KDFKBIL-ST-URS-SRA      OR                                        
064200        KDFKBIL-ST-URS          OR                                        
064300        KDFKBIL-ST-SRA          OR                                        
064400        KDFKBIL-URS-SRA                                                   
064500                                                                          
064600       MOVE +1  TO IXURS                                                  
064700       MOVE NOO TO IXURS-PRESENT-SW                                       
064800                                                                          
064900       PERFORM UNTIL IXURS  > ST-URS-MAX-ANTAL-ARTURS  OR                 
065000                     IXURS-PRESENT                                        
065100         IF SRAD-KDARTURS = ST-URS-AKTUELL-KDARTURS (IXURS)               
065200           MOVE YES TO IXURS-PRESENT-SW                                   
065300         ELSE                                                             
065400           IF IXURS > ST-URS-ANTAL-ARTURS                                 
065500             MOVE IXURS         TO ST-URS-ANTAL-ARTURS                    
065600             MOVE SRAD-KDARTURS TO ST-URS-AKTUELL-KDARTURS (IXURS)        
065700             MOVE YES           TO IXURS-PRESENT-SW                       
065800           END-IF                                                         
065900         END-IF                                                           
066000         ADD +1 TO IXURS                                                  
066100       END-PERFORM                                                        
066200                                                                          
066300** TABEL OVERFLOW  ****                                                   
066400       IF IXURS-PRESENT-SW = NOO                                          
066500         MOVE 'TABLE ST-URS-AKTUELL-KDARTURS IS SMALL'                    
066600                                    TO ERRTEXT                            
066700         CALL FELLOG                                                      
066800       END-IF                                                             
066900                                                                          
067000       MOVE ZERO TO IXSTATNR                                              
067100                    IXARTURS                                              
067200       MOVE +1   TO INDX                                                  
067300                                                                          
067400       PERFORM UNTIL  INDX > ST-URS-MAX-ANTAL-STATNR                      
067500         IF INDX > ST-URS-ANTAL-STATNR                                    
067600           MOVE INDX                 TO ST-URS-ANTAL-STATNR               
067700                                        IXSTATNR                          
067800                                        ST-URS-IDSTATNR-PNR (INDX)        
067900*          MOVE SRAD-IDSTATNR        TO ST-URS-IDSTATNR (IXSTATNR)        
068000           IF CLAG-IDSTATNR (1) > ZERO                                    
068100             MOVE CLAG-IDSTATNR(1)   TO ST-URS-IDSTATNR (IXSTATNR)        
068200           ELSE                                                           
068300             MOVE GEN-IDSTATNR       TO ST-URS-IDSTATNR (IXSTATNR)        
068400           END-IF                                                         
068500           MOVE ST-URS-MAX-ANTAL-STATNR                                   
068600                                     TO INDX                              
068700         ELSE                                                             
068800           IF CLAG-IDSTATNR (1) > ZERO                                    
068900             IF CLAG-IDSTATNR (1) = ST-URS-IDSTATNR (INDX)                
069000*              MOVE CLAG-IDSTATNR(1) TO ST-URS-IDSTATNR (IXSTATNR)        
069100               MOVE INDX             TO IXSTATNR                          
069200               MOVE ST-URS-MAX-ANTAL-STATNR                               
069300                                       TO INDX                            
069400             END-IF                                                       
069500           ELSE                                                           
069600             IF GEN-IDSTATNR      = ST-URS-IDSTATNR (INDX)                
069700*              MOVE GEN-IDSTATNR     TO ST-URS-IDSTATNR (IXSTATNR)        
069800               MOVE INDX             TO IXSTATNR                          
069900               MOVE ST-URS-MAX-ANTAL-STATNR                               
070000                                       TO INDX                            
070100             END-IF                                                       
070200           END-IF                                                         
070300                                                                          
070400         END-IF                                                           
070500                                                                          
070600         ADD  +1 TO INDX                                                  
070700       END-PERFORM                                                        
070800                                                                          
070900       IF IXSTATNR NOT = ZERO                                             
071000         MOVE +1 TO INDX                                                  
071100         PERFORM UNTIL INDX > ST-URS-ANTAL-ARTURS                         
071200           IF ST-URS-KDARTURS (IXSTATNR, INDX) = W-CONST-99               
071300             MOVE SRAD-KDARTURS         TO                                
071400                            ST-URS-KDARTURS (IXSTATNR, INDX)              
071500           END-IF                                                         
071600                                                                          
071700           IF SRAD-KDARTURS = ST-URS-KDARTURS (IXSTATNR, INDX)            
071800             MOVE INDX                  TO IXARTURS                       
071900             MOVE ST-URS-MAX-ANTAL-ARTURS                                 
072000                                        TO INDX                           
072100           END-IF                                                         
072200                                                                          
072300           ADD +1 TO INDX                                                 
072400         END-PERFORM                                                      
072500       END-IF                                                             
072600                                                                          
072700       ADD SRAD-KVLEVART     TO ST-URS-TOT-KVART                          
072800       ADD W-VKVIKT          TO ST-URS-TOT-VKORDNTO                       
072900       ADD W-VKVIKTB         TO ST-URS-TOT-VKORDBTO                       
073000                                                                          
073100*      VI SUMMERAR INTE INDISKA SUMMOR HÄR, VI SUMMERAR ISTÄLLET          
073200*      OMVANDLADE AVRUNDADE DELSUMMOR I S03-, SÅ SUMMORNA STÄMMER         
073300*      FÖR INDIEN SUMMERAS GRANDTOT I SEK DESSUTOM                        
073400       ADD W-SUPRIS          TO ST-URS-TOT-SUORDV-TOT                     
073500       MOVE W-KDVALISO       TO W-ST-URS-TOT-KDVALISO                     
073600                                                                          
073700       IF IXSTATNR = ZERO OR IXARTURS = ZERO                              
073800** TABEL OVERFLOW                                                         
073900         MOVE ' ST-URS-TABEL IS SMALL' TO ERRTEXT                         
074000         CALL FELLOG                                                      
074100       ELSE                                                               
074200         ADD SRAD-KVLEVART TO ST-URS-KVART (IXSTATNR, IXARTURS)           
074300         ADD W-VKVIKT      TO ST-URS-VKORDNTO (IXSTATNR, IXARTURS)        
074400         ADD W-VKVIKTB     TO ST-URS-VKORDBTO (IXSTATNR, IXARTURS)        
074500         ADD W-SUPRIS      TO                                             
074600                            ST-URS-SUORDV-TOT (IXSTATNR, IXARTURS)        
074700         MOVE W-KDVALISO   TO                                             
074800                            W-ST-URS-KDVALISO (IXSTATNR, IXARTURS)        
074900       END-IF                                                             
075000     END-IF                                                               
075100                                                                          
075200     .                                                                    
075300     EJECT                                                                
075400                                                                          
075500 C-OMVANDLA-VALUTA SECTION.                                               
075600                                                                          
075700*    HÄMTA KURS FÖR OMRÄKNING TILL INR                                    
075800     MOVE 'INR'                      TO CURR-KDVALISO-ROW                 
075900     MOVE TODAYS-DATE-YEAR           TO W-DATE-AAMM(1:2)                  
076000     MOVE TODAYS-DATE-MONTH          TO W-DATE-AAMM(3:2)                  
076100     MOVE W-DATE-AAMM                TO CURR-TIAAMM                       
076200     MOVE WS-KDVALISO-HUV            TO CURR-KDVALISO-HUV                 
076300     MOVE 'M'                        TO CURR-KDVALTYP                     
076400                                                                          
076500     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
076600     IF CURR-KDSVAR = ' '                                                 
076700        CONTINUE                                                          
076800     ELSE                                                                 
076900        MOVE 1                       TO CURR-PRKURS-NEW                   
077000     END-IF                                                               
077100     COMPUTE WS-PRKURS-INR ROUNDED = 1 / CURR-PRKURS-NEW                  
077200     .                                                                    
077300                                                                          
077400     EJECT                                                                
077500                                                                          
077600 E-PRINT-DOCUMENT SECTION.                                                
077700                                                                          
077800     PERFORM EC-BRUTTOVIKT                                                
077900     IF TOTAL2-VKORDBTO NOT = TOTAL3-VKORDBTO                             
078000       MOVE 1          TO ISM                                             
078100*      PERFORM UNTIL ISM > 3                                              
078200*        PERFORM EC-BRUTTOVIKT                                            
078300*        ADD 1         TO ISM                                             
078400*      END-PERFORM                                                        
078500     END-IF                                                               
078600                                                                          
078700     IF KDFKBIL-ST OR                                                     
078800        KDFKBIL-ST-URS-SRA OR                                             
078900        KDFKBIL-ST-URS OR                                                 
079000        KDFKBIL-ST-SRA                                                    
079100                                                                          
079200       PERFORM EA-PRINT-STATNR-ORG                                        
079300                                                                          
079400     END-IF                                                               
079500                                                                          
079600*    IF KDFKBIL-URS OR                                                    
079700*       KDFKBIL-SRA OR                                                    
079800*       KDFKBIL-ST-URS-SRA OR                                             
079900*       KDFKBIL-ST-URS OR                                                 
080000*       KDFKBIL-URS-SRA                                                   
080100*                                                                         
080200*      PERFORM EB-PRINT-ORG-STATNR                                        
080300*                                                                         
080400*    END-IF                                                               
080500                                                                          
080600     .                                                                    
080700     EJECT                                                                
080800                                                                          
080900 EA-PRINT-STATNR-ORG  SECTION.                                            
081000                                                                          
081100******************************************************************        
081200*    PRINT  IN ORDER   STATNR/URSPRUNG                           *        
081300******************************************************************        
081400                                                                          
081500     MOVE ZERO                        TO TOTAL-VKORDNTO                   
081600     MOVE ZERO                        TO TOTAL-VKORDBTO                   
081700                                                                          
081800     IF ST-URS-ANTAL-STATNR NOT = ZERO                                    
081900       IF ST-URS-ANTAL-STATNR > +1                                        
082000         MOVE ST-URS-ANTAL-STATNR     TO INTSOR-POST-ANTAL                
082100         MOVE ST-URS-IDSTATNR-IX-LGD  TO INTSOR-POST-LAENGD               
082200         MOVE ST-URS-IDSTATNR-LAENGD  TO INTSOR-SORT-FAELT-LAENGD         
082300         CALL INTSOR USING ST-URS-IDSTATNR-IX-TABEL (+1)                  
082400                           INTSOR-POST-LAENGD                             
082500                           INTSOR-POST-ANTAL                              
082600                           ST-URS-IDSTATNR (+1)                           
082700                           INTSOR-SORT-FAELT-LAENGD                       
082800       END-IF                                                             
082900                                                                          
083000       MOVE +99 TO W-LINE-COUNT                                           
083100       MOVE +1  TO INDX                                                   
083200                                                                          
083300       PERFORM UNTIL INDX > ST-URS-ANTAL-STATNR                           
083400         IF ST-URS-IDSTATNR (INDX) NOT = ZERO                             
083500           MOVE ST-URS-IDSTATNR (INDX)   TO RAD3-IDSTATNR                 
083600         END-IF                                                           
083700         MOVE ST-URS-IDSTATNR-PNR (INDX) TO IXSTATNR                      
083800         MOVE ZERO  TO ST-URS-SUBTOT-KVART                                
083900                       ST-URS-SUBTOT-VKORDNTO                             
084000                       ST-URS-SUBTOT-VKORDBTO                             
084100                       ST-URS-SUBTOT-SUORDV-TOT                           
084200                       ST-URS-SUBTOT-SUORDV-TOT-INR                       
084300         MOVE SPACE TO W-ST-URS-SUBTOT-KDVALISO                           
084400                                                                          
084500         MOVE +1    TO IXARTURS                                           
084600         PERFORM UNTIL IXARTURS > ST-URS-ANTAL-ARTURS OR                  
084700                ST-URS-KDARTURS (IXSTATNR, IXARTURS) = W-CONST-99         
084800                                                                          
084900           IF W-LINE-COUNT > W-LINE-MAX                                   
085000             IF FLFIRSTTIME                                               
085100               PERFORM S05-PRINT-HEAD-1                                   
085200               PERFORM S06-PRINT-HEAD-2                                   
085300               MOVE NOO TO FLFIRSTTIME-SW                                 
085400             END-IF                                                       
085500*            IF ST-URS-IDSTATNR (INDX) NOT = ZERO                         
085600*              MOVE ST-URS-IDSTATNR (INDX) TO RAD3-IDSTATNR               
085700*            END-IF                                                       
085800           END-IF                                                         
085900                                                                          
086000           MOVE ST-URS-IDSTATNR (INDX)     TO RAD3-IDSTATNR               
086100           MOVE ST-URS-KDARTURS (IXSTATNR, IXARTURS)                      
086200*                                          TO RAD3-KDARTURS               
086300                                           TO WS-KDARTURS                 
086400*          IF RAD3-KDARTURS = 'XS '                                       
086500*            MOVE 'RS ' TO RAD3-KDARTURS                                  
086600*          END-IF                                                         
086700           PERFORM S40-HAMTA-LAND                                         
086800           PERFORM S03-BUILD-DETAIL                                       
086900*          MOVE SPACES             TO RAD-3                               
087000           PERFORM S88-MOVE-SPACE-RAD3                                    
087100           ADD +1                  TO IXARTURS                            
087200         END-PERFORM                                                      
087300         PERFORM S01-BUILD-SUBTOTAL                                       
087400         ADD  +1    TO INDX                                               
087500       END-PERFORM                                                        
087600       PERFORM S02-BUILD-GRTOTAL                                          
087700     END-IF                                                               
087800     .                                                                    
087900     EJECT                                                                
088000                                                                          
088100 EB-PRINT-ORG-STATNR  SECTION.                                            
088200                                                                          
088300******************************************************************        
088400*    PRINT IN ORDER   URSPRUNG/STATNR                            *        
088500******************************************************************        
088600                                                                          
088700     MOVE ZERO                        TO TOTAL-VKORDNTO                   
088800     MOVE ZERO                        TO TOTAL-VKORDBTO                   
088900                                                                          
089000     IF ST-URS-ANTAL-ARTURS NOT = ZERO                                    
089100        MOVE +99 TO W-LINE-COUNT                                          
089200        MOVE +1  TO IXURS                                                 
089300        PERFORM UNTIL IXURS > ST-URS-ANTAL-ARTURS                         
089400          MOVE ZERO  TO ST-URS-SUBTOT-KVART                               
089500                        ST-URS-SUBTOT-VKORDNTO                            
089600                        ST-URS-SUBTOT-VKORDBTO                            
089700                        ST-URS-SUBTOT-SUORDV-TOT                          
089800                        ST-URS-SUBTOT-SUORDV-TOT-INR                      
089900          MOVE SPACE TO W-ST-URS-SUBTOT-KDVALISO                          
090000                                                                          
090100*         MOVE ST-URS-AKTUELL-KDARTURS (IXURS) TO RAD3-KDARTURS           
090200          MOVE ST-URS-AKTUELL-KDARTURS (IXURS) TO WS-KDARTURS             
090300*          IF RAD3-KDARTURS = 'XS '                                       
090400*            MOVE 'RS ' TO RAD3-KDARTURS                                  
090500*          END-IF                                                         
090600          PERFORM S40-HAMTA-LAND                                          
090700          MOVE +1      TO INDX                                            
090800                                                                          
090900          PERFORM UNTIL INDX > ST-URS-ANTAL-STATNR                        
091000                                                                          
091100            MOVE ST-URS-IDSTATNR-PNR (INDX) TO IXSTATNR                   
091200            MOVE +1 TO IXARTURS                                           
091300            PERFORM UNTIL IXARTURS > ST-URS-ANTAL-ARTURS OR               
091400                ST-URS-KDARTURS (IXSTATNR, IXARTURS) = W-CONST-99         
091500                                                                          
091600              IF ST-URS-KDARTURS (IXSTATNR, IXARTURS) =                   
091700                                 ST-URS-AKTUELL-KDARTURS (IXURS)          
091800                IF W-LINE-COUNT > W-LINE-MAX                              
091900                  PERFORM S05-PRINT-HEAD-1                                
092000*                 PERFORM S06-PRINT-HEAD-2                                
092100                  MOVE ST-URS-AKTUELL-KDARTURS (IXURS)                    
092200*                                            TO RAD3-KDARTURS             
092300                                             TO WS-KDARTURS               
092400*                 IF RAD3-KDARTURS = 'XS '                                
092500*                   MOVE 'RS ' TO RAD3-KDARTURS                           
092600*                 END-IF                                                  
092700                  PERFORM S40-HAMTA-LAND                                  
092800                END-IF                                                    
092900                IF ST-URS-IDSTATNR (INDX) NOT = ZERO                      
093000                  MOVE ST-URS-IDSTATNR (INDX) TO RAD3-IDSTATNR            
093100                END-IF                                                    
093200                                                                          
093300                PERFORM S03-BUILD-DETAIL                                  
093400*               MOVE SPACES                   TO RAD-3                    
093500                PERFORM S88-MOVE-SPACE-RAD3                               
093600                MOVE ST-URS-MAX-ANTAL-ARTURS  TO IXARTURS                 
093700                                                                          
093800              END-IF                                                      
093900              ADD +1 TO IXARTURS                                          
094000                                                                          
094100            END-PERFORM                                                   
094200            ADD +1 TO INDX                                                
094300                                                                          
094400          END-PERFORM                                                     
094500          PERFORM S01-BUILD-SUBTOTAL                                      
094600          ADD  +1  TO IXURS                                               
094700                                                                          
094800        END-PERFORM                                                       
094900        PERFORM S02-BUILD-GRTOTAL                                         
095000     END-IF                                                               
095100     .                                                                    
095200     EJECT                                                                
095300 EC-BRUTTOVIKT  SECTION.                                                  
095400                                                                          
095500     IF TOTAL2-VKORDBTO NOT = TOTAL3-VKORDBTO                             
095600       MOVE 1 TO IA                                                       
095700       PERFORM UNTIL IA > ST-URS-ANTAL-STATNR                             
095800        MOVE 1    TO IB                                                   
095900        PERFORM UNTIL IB > ST-URS-ANTAL-ARTURS                            
096000        IF ST-URS-VKORDBTO(IA, IB) < 0.05                                 
096100          AND ST-URS-KVART(IA, IB) NOT = 0                                
096200          COMPUTE WX-BTO = 0.1 - ST-URS-VKORDBTO(IA, IB)                  
096300          MOVE 0.1          TO ST-URS-VKORDBTO (IA, IB)                   
096400          ADD WX-BTO        TO TOTAL3-VKORDBTO                            
096500        END-IF                                                            
096600        IF TOTAL2-VKORDBTO NOT = TOTAL3-VKORDBTO                          
096700        IF TOTAL2-VKORDBTO > TOTAL3-VKORDBTO                              
096800*       KOLLI-VIKT > RAD-VIKTERNA                                         
096900         IF ST-URS-VKORDBTO (IA, IB) > ZERO                               
097000          ADD 0.1         TO   ST-URS-VKORDBTO (IA, IB)                   
097100          ADD 0.1         TO   TOTAL3-VKORDBTO                            
097200         END-IF                                                           
097300        ELSE                                                              
097400         IF ST-URS-VKORDBTO (IA, IB) > 0.15                               
097500          SUBTRACT 0.1      FROM ST-URS-VKORDBTO (IA, IB)                 
097600          SUBTRACT 0.1      FROM TOTAL3-VKORDBTO                          
097700         END-IF                                                           
097800        END-IF                                                            
097900        END-IF                                                            
098000        ADD 1      TO IB                                                  
098100        END-PERFORM                                                       
098200        ADD 1    TO IA                                                    
098300       END-PERFORM                                                        
098400       COMPUTE TOTAL3-VKORDBTO =                                          
098500                      TOTAL2-VKORDBTO - TOTAL3-VKORDBTO                   
098600       ADD TOTAL3-VKORDBTO     TO ST-URS-VKORDBTO (1, 1)                  
098700     END-IF                                                               
098800     .                                                                    
098900     EJECT                                                                
099000                                                                          
099100 S01-BUILD-SUBTOTAL   SECTION.                                            
099200                                                                          
099300     MOVE SPACES                     TO ARB-RAD                           
099400                                        SEND-RAD                          
099500*    MOVE 'S='                       TO RAD3-ASTERISK                     
099600*    MOVE ST-URS-SUBTOT-KVART        TO RAD3-KVART                        
099700*    IF ST-URS-SUBTOT-VKORDNTO < 0.05                                     
099800*      MOVE 0.1 TO ST-URS-SUBTOT-VKORDNTO                                 
099900*    END-IF                                                               
100000*    COMPUTE RAD3-VKORDNTO ROUNDED = ST-URS-SUBTOT-VKORDNTO               
100100*                                                                         
100200     ADD ST-URS-SUBTOT-VKORDNTO      TO TOTAL-VKORDNTO                    
100300*                                                                         
100400*    IF ST-URS-SUBTOT-VKORDBTO < 0.05                                     
100500*      MOVE 0.1 TO ST-URS-SUBTOT-VKORDBTO                                 
100600*    END-IF                                                               
100700*    COMPUTE RAD3-VKORDBTO ROUNDED = ST-URS-SUBTOT-VKORDBTO               
100800*                                                                         
100900     ADD ST-URS-SUBTOT-VKORDBTO      TO TOTAL-VKORDBTO                    
101000*                                                                         
101100*    IF (CDC-SE OR DDC-SE) AND                                            
101200*      (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
101300*      MOVE ST-URS-SUBTOT-SUORDV-TOT-INR TO RAD3-SUORDV-TOT               
101400*      MOVE 'INR'                     TO RAD3-KDVALISO                    
101500*    ELSE                                                                 
101600*      MOVE ST-URS-SUBTOT-SUORDV-TOT TO RAD3-SUORDV-TOT                   
101700*      MOVE W-ST-URS-SUBTOT-KDVALISO TO RAD3-KDVALISO                     
101800*      IF FLLOCCUR                                                        
101900*        COMPUTE WS-MONEY ROUNDED = (ST-URS-SUBTOT-SUORDV-TOT *           
102000*                WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                   
102100*        MOVE WS-MONEY               TO RAD3-SUORDV-TOT                   
102200*        MOVE SGMT-KDVALISO          TO RAD3-KDVALISO                     
102300*      END-IF                                                             
102400*    END-IF                                                               
102500*    MOVE RAD-3                      TO ARB-RAD                           
102600*                                       SEND-RAD                          
102700*    MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
102800*    MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
102900*    ADD 1                           TO W-LINE-COUNT                      
103000*    PERFORM S04-PRINT-LINE                                               
103100*    MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
103200*    MOVE SPACE                      TO ARB-RAD                           
103300*                                       SEND-RAD                          
103400*    MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
103500*    MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
103600*    ADD 1                           TO W-LINE-COUNT                      
103700*    PERFORM S04-PRINT-LINE                                               
103800*    MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
103900*    MOVE SPACES                     TO RAD3-ASTERISK                     
104000     .                                                                    
104100     EJECT                                                                
104200                                                                          
104300 S02-BUILD-GRTOTAL    SECTION.                                            
104400                                                                          
104500     MOVE SPACES                  TO ARB-RAD                              
104600*                                    SEND-RAD                             
104700     PERFORM S88-MOVE-SPACE-RAD3                                          
104800     MOVE 'T='                    TO RAD3-ASTERISK                        
104900     MOVE ST-URS-TOT-KVART        TO RAD3-KVART                           
105000*    IF ST-URS-TOT-VKORDNTO < 0.05                                        
105100*      MOVE 0.1                   TO ST-URS-TOT-VKORDNTO                  
105200*    END-IF                                                               
105300     COMPUTE RAD3-VKORDNTO ROUNDED = ST-URS-TOT-VKORDNTO                  
105400                                                                          
105500     COMPUTE RAD3-VKORDNTO         = TOTAL-VKORDNTO                       
105600     COMPUTE RAD4-VKORDNTO         = TOTAL-VKORDNTO                       
105700                                                                          
105800*    IF ST-URS-TOT-VKORDBTO < 0.05                                        
105900*      MOVE 0.1                   TO ST-URS-TOT-VKORDBTO                  
106000*    END-IF                                                               
106100     COMPUTE RAD3-VKORDBTO ROUNDED = ST-URS-TOT-VKORDBTO                  
106200                                                                          
106300     COMPUTE RAD3-VKORDBTO         = TOTAL-VKORDBTO                       
106400     COMPUTE RAD5-VKORDBTO         = TOTAL-VKORDBTO                       
106500                                                                          
106600                                                                          
106700     IF (CDC-SE OR DDC-SE) AND                                            
106800       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
106900                                                                          
107000*      ALT1. ÄR ATT VISA INDISKA DELSUMMORS TOTAL                         
107100*      - DÅ STÄMMER TOTALERNA INOM DETTA TRP DOK (GÄLLER NU)              
107200*      ALT2. ÄR ATT VISA SVENSKA TOTALEN KONVERTERAD TILL INR             
107300*      - DÅ STÄMMER TOTALERNA MELLAN OLIKA TRP DOK                        
107400                                                                          
107500       MOVE ST-URS-TOT-SUORDV-TOT-INR TO RAD3-SUORDV-TOT                  
107600                                                                          
107700**ALT2 COMPUTE WS-MONEY ROUNDED =                                         
107800******   ST-URS-TOT-SUORDV-TOT * WS-PRKURS-INR                            
107900****** MOVE WS-MONEY                 TO RAD3-SUORDV-TOT                   
108000                                                                          
108100       MOVE 'INR'                    TO RAD3-KDVALISO                     
108200     ELSE                                                                 
108300       MOVE ST-URS-TOT-SUORDV-TOT TO RAD3-SUORDV-TOT                      
108400       MOVE W-ST-URS-TOT-KDVALISO TO RAD3-KDVALISO                        
108500*      IF FLLOCCUR                                                        
108600*        COMPUTE WS-MONEY ROUNDED = (ST-URS-TOT-SUORDV-TOT *              
108700*                WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                   
108800*        MOVE WS-MONEY               TO RAD3-SUORDV-TOT                   
108900*        MOVE SGMT-KDVALISO          TO RAD3-KDVALISO                     
109000*      END-IF                                                             
109100     END-IF                                                               
109200     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
109300*    MOVE WS-SKIP1                TO STYRTECKEN-RAD                       
109400     ADD 1                        TO W-LINE-COUNT                         
109500     MOVE RAD-3                   TO ARB-RAD                              
109600                                     SEND-RAD                             
109700     PERFORM S04-PRINT-LINE                                               
109800     IF (CDC-SE OR DDC-SE) AND                                            
109900       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
110000       CONTINUE                                                           
110100     ELSE                                                                 
110200       IF FLLOCCUR                                                        
110300        IF WS-MONEY NOT = WY-MONEY                                        
110400         SUBTRACT WS-MONEY      FROM WY-MONEY                             
110500         IF WY-MONEY NOT = ZERO                                           
110600           MOVE WY-MONEY        TO RADY-DIFF                              
110700           MOVE 'CURRENCY CONVERSION ADJUSTMENT' TO RADY-TEXT             
110800           MOVE RADY            TO ARB-RAD                                
110900                                     SEND-RAD                             
111000           MOVE PRT-AFTER-2     TO PRT-RADSKIP                            
111100*          MOVE WS-SKIP2        TO STYRTECKEN-RAD                         
111200           ADD 2                TO W-LINE-COUNT                           
111300           PERFORM S04-PRINT-LINE                                         
111400         END-IF                                                           
111500        END-IF                                                            
111600       END-IF                                                             
111700     END-IF                                                               
111800     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
111900*    MOVE WS-SKIP1                TO STYRTECKEN-RAD                       
112000     ADD 1                        TO W-LINE-COUNT                         
112100     MOVE RAD4                    TO ARB-RAD                              
112200                                     SEND-RAD                             
112300     PERFORM S04-PRINT-LINE                                               
112400     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
112500*    MOVE WS-SKIP1                TO STYRTECKEN-RAD                       
112600     ADD 1                        TO W-LINE-COUNT                         
112700     MOVE RAD5                    TO ARB-RAD                              
112800                                     SEND-RAD                             
112900     PERFORM S04-PRINT-LINE                                               
113000     MOVE ST-URS-TOT-KVKOLLI      TO RAD6-KVKOLLI                         
113100     MOVE PRT-AFTER-1             TO PRT-RADSKIP                          
113200*    MOVE WS-SKIP1                TO STYRTECKEN-RAD                       
113300     ADD 1                        TO W-LINE-COUNT                         
113400     MOVE RAD6                    TO ARB-RAD                              
113500                                     SEND-RAD                             
113600     PERFORM S04-PRINT-LINE                                               
113700                                                                          
113800*    MOVE WS-SKIP1                TO STYRTECKEN-RAD                       
113900     MOVE SPACES                  TO RAD3-ASTERISK                        
114000     .                                                                    
114100     EJECT                                                                
114200                                                                          
114300 S03-BUILD-DETAIL     SECTION.                                            
114400                                                                          
114500     MOVE ST-URS-KVART (IXSTATNR, IXARTURS) TO RAD3-KVART                 
114600                                                                          
114700*    IF ST-URS-VKORDNTO (IXSTATNR, IXARTURS) < 0.05                       
114800*      MOVE 0.1         TO ST-URS-VKORDNTO (IXSTATNR, IXARTURS)           
114900*    END-IF                                                               
115000                                                                          
115100     COMPUTE RAD3-VKORDNTO ROUNDED =                                      
115200                           ST-URS-VKORDNTO (IXSTATNR, IXARTURS)           
115300*    IF ST-URS-VKORDBTO (IXSTATNR, IXARTURS) < 0.05                       
115400*      MOVE 0.1         TO ST-URS-VKORDBTO (IXSTATNR, IXARTURS)           
115500*    END-IF                                                               
115600                                                                          
115700     COMPUTE RAD3-VKORDBTO ROUNDED =                                      
115800                           ST-URS-VKORDBTO (IXSTATNR, IXARTURS)           
115900     IF (CDC-SE OR DDC-SE) AND                                            
116000       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
116100       COMPUTE WS-MONEY ROUNDED =                                         
116200         ST-URS-SUORDV-TOT (IXSTATNR, IXARTURS) *                         
116300                                     WS-PRKURS-INR                        
116400       MOVE WS-MONEY                 TO RAD3-SUORDV-TOT                   
116500                                        ST-URS-SUORDV-TOT-INR             
116600       MOVE 'INR'                    TO RAD3-KDVALISO                     
116700     ELSE                                                                 
116800       MOVE ST-URS-SUORDV-TOT (IXSTATNR, IXARTURS)                        
116900                                        TO RAD3-SUORDV-TOT                
117000       MOVE W-ST-URS-KDVALISO (IXSTATNR, IXARTURS)                        
117100                                        TO RAD3-KDVALISO                  
117200*      IF FLLOCCUR                                                        
117300*        COMPUTE WS-MONEY ROUNDED = (ST-URS-SUORDV-TOT                    
117400*                                       (IXSTATNR, IXARTURS) *            
117500*                WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                   
117600*        MOVE WS-MONEY               TO RAD3-SUORDV-TOT                   
117700*        ADD WS-MONEY                TO WY-MONEY                          
117800*        MOVE SGMT-KDVALISO          TO RAD3-KDVALISO                     
117900*      END-IF                                                             
118000     END-IF                                                               
118100     MOVE RAD-3                       TO ARB-RAD                          
118200                                         SEND-RAD                         
118300     MOVE PRT-AFTER-1                 TO PRT-RADSKIP                      
118400*    MOVE WS-SKIP1                    TO STYRTECKEN-RAD                   
118500     ADD  1                           TO W-LINE-COUNT                     
118600     PERFORM S04-PRINT-LINE                                               
118700*    MOVE WS-SKIP1                    TO STYRTECKEN-RAD                   
118800     ADD ST-URS-KVART (IXSTATNR, IXARTURS)                                
118900                                      TO ST-URS-SUBTOT-KVART              
119000     ADD ST-URS-VKORDNTO (IXSTATNR, IXARTURS)                             
119100                                      TO ST-URS-SUBTOT-VKORDNTO           
119200     ADD ST-URS-VKORDBTO (IXSTATNR, IXARTURS)                             
119300                                      TO ST-URS-SUBTOT-VKORDBTO           
119400**   REDAN OMVANDLAD INDISK VALUTA SUMMERAS TILL BÄGGE TOTALERNA          
119500**   FÖR UNDVIKA AVRUNDNINGSFEL I TOTALERNA                               
119600     IF (CDC-SE OR DDC-SE) AND                                            
119700       (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                         
119800       ADD ST-URS-SUORDV-TOT-INR   TO ST-URS-SUBTOT-SUORDV-TOT-INR        
119900       ADD ST-URS-SUORDV-TOT-INR   TO ST-URS-TOT-SUORDV-TOT-INR           
120000       MOVE 'INR'                     TO W-ST-URS-SUBTOT-KDVALISO         
120100     ELSE                                                                 
120200       ADD ST-URS-SUORDV-TOT (IXSTATNR, IXARTURS)                         
120300                                      TO ST-URS-SUBTOT-SUORDV-TOT         
120400       MOVE W-ST-URS-KDVALISO (IXSTATNR, IXARTURS)                        
120500                                      TO W-ST-URS-SUBTOT-KDVALISO         
120600     END-IF                                                               
120700     .                                                                    
120800     EJECT                                                                
120900                                                                          
121000 S04-PRINT-LINE SECTION.                                                  
121100                                                                          
121200*    -- PRINT TO ON-DEMAND IF SO SPECIFIED ON 4456                        
121300     PERFORM S90-PUT-DOC-LINE                                             
121400                                                                          
121500*    -- PRINT TO PAPER IF SO SPECIFIED ON 4664 (FLSKRIV-NU = YES)         
121600*    -- OR ALWAYS IF WE COME FROM 4622                                    
121700*    -- OR ALWAYS IF IT IS A WEB DC (WHERE FLSKRIV-NU IS N AND            
121800*    -- CANNOT BE SET TO J)                                               
121900     IF SHIP-FLSKRIV-NU = YES OR TRPD-IDPGM = 'W4062200'                  
122000     OR TRPD-FLLDCKND = YES                                               
122100       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
122200                           PRT-WRITE                                      
122300                           W-IDPRTLST                                     
122400                           ALT-PCB                                        
122500                           PRT-RADSKIP                                    
122600                           ARB-RAD                                        
122700     END-IF                                                               
122800     .                                                                    
122900     EJECT                                                                
123000                                                                          
123100 S05-PRINT-HEAD-1 SECTION.                                                
123200                                                                          
123300*    MOVE SPACE                      TO SEND-RAD                          
123400*    MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
123500*    PERFORM S90-PUT-DOC-LINE                                             
123600*    MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
123700*    PERFORM S90-PUT-DOC-LINE                                             
123800*    MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
123900*    PERFORM S90-PUT-DOC-LINE                                             
124000                                                                          
124100     MOVE 'INVOICE     '        TO RAD1H-INVOICE-TEXT                     
124200     MOVE SKOLLI-IDFAKT         TO RAD1H-INVOICE                          
124300     MOVE RAD-HEAD              TO ARB-RAD                                
124400                                   SEND-RAD                               
124500*    MOVE WS-SKIP1              TO STYRTECKEN-RAD                         
124600*    MOVE PRT-NYSIDA-RAD7       TO PRT-RADSKIP                            
124700*    MOVE 7                     TO W-LINE-COUNT                           
124800     PERFORM S04-PRINT-LINE                                               
124900                                                                          
125000     MOVE SPACE                      TO SEND-RAD                          
125100*    MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
125200*    PERFORM S90-PUT-DOC-LINE                                             
125300*    PERFORM S90-PUT-DOC-LINE                                             
125400                                                                          
125500     MOVE SPACE                      TO SEND-RAD                          
125600                                                                          
125700     ADD 1                    TO W-LINE-COUNT                             
125800     .                                                                    
125900     EJECT                                                                
126000                                                                          
126100 S06-PRINT-HEAD-2 SECTION.                                                
126200                                                                          
126300*    MOVE SPACES           TO ARB-RAD                                     
126400     MOVE RAD-2            TO ARB-RAD                                     
126500                              SEND-RAD                                    
126600     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
126700*    MOVE WS-SKIP2         TO STYRTECKEN-RAD                              
126800     ADD 2                 TO W-LINE-COUNT                                
126900     PERFORM  S04-PRINT-LINE                                              
127000*    MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
127100                                                                          
127200     MOVE  SPACE           TO ARB-RAD                                     
127300                              SEND-RAD                                    
127400     MOVE  PRT-AFTER-1     TO PRT-RADSKIP                                 
127500*    MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
127600     ADD   1               TO W-LINE-COUNT                                
127700     PERFORM S04-PRINT-LINE                                               
127800*    MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
127900     .                                                                    
128000     EJECT                                                                
128100 S20-HAMTA-WDB2  SECTION.                                                 
128200                                                                          
128300     MOVE SGMT-IDKUNDNR      TO W-WDB201-IDKUNDNR                         
128400     PERFORM IMS-GU-WDB201                                                
128500     IF GMT-KDSPRAK  <  ZERO OR > +5                                      
128600       MOVE +2 TO W-KDSPRAK                                               
128700     ELSE                                                                 
128800       COMPUTE W-KDSPRAK = GMT-KDSPRAK + +1                               
128900     END-IF                                                               
129000     IF W-KDSPRAK NOT = +2                                                
129100       MOVE IDSTATNR-LEDTEXT   (W-KDSPRAK) TO RAD2-IDSTATNR               
129200*      MOVE BEARTURS-LEDTEXT   (W-KDSPRAK) TO RAD2-KDARTURS               
129300       MOVE KBIL-KVANT-LEDTEXT (W-KDSPRAK) TO RAD2-KVART                  
129400       MOVE VKORDNTO-LEDTEXT   (W-KDSPRAK) TO RAD2-VKORDNTO               
129500       MOVE VKORDBTO-LEDTEXT   (W-KDSPRAK) TO RAD2-VKORDBTO               
129600       MOVE KBIL-TOTAL-LEDTEXT (W-KDSPRAK) TO RAD2-SUORDV-TOT             
129700       MOVE KDVALISO-LEDTEXT   (W-KDSPRAK) TO RAD2-KDVALISO               
129800       MOVE 'DESCRIPT'                     TO RAD2-DESCRIPTION            
129900       MOVE ' COUNTRY'                     TO RAD2-KDARTURS-LAND          
130000     END-IF                                                               
130100     .                                                                    
130200     EJECT                                                                
130300 S40-HAMTA-LAND SECTION.                                                  
130400                                                                          
130500*    MOVE SRAD-KDARTURS       TO ARTU-KDARTURS                            
130600     MOVE WS-KDARTURS         TO ARTU-KDARTURS                            
130700     MOVE ZERO                TO ARTU-IDDISTR                             
130800     MOVE SPACE               TO ARTU-IDDC                                
130900                                                                          
131000     CALL W400ARTU USING ARTU-W400ARTU                                    
131100     MOVE ARTU-BEARTURS-ENG TO RAD3-KDARTURS-LAND                         
131200     .                                                                    
131300     EJECT                                                                
131400                                                                          
131500 S44-LAES-WDE1-TOT   SECTION.                                             
131600                                                                          
131700     PERFORM IMS-GU-WDE101-11                                             
131800     MOVE SGMT-IDDC           TO WS-IDDC                                  
131900                                                                          
132000     MOVE SGMT-IDDISTR    TO W-WDE111-IDDISTR                             
132100     MOVE SGMT-IDKUNDNR   TO W-WDE111-IDKUNDNR                            
132200     MOVE SHIP-PRKURS-BET TO WS-PRKURS-USD                                
132300                                                                          
132400     PERFORM UNTIL NOT SEGMENT-FOUND                                      
132500       PERFORM IMS-GNP-WDE121                                             
132600       MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                       
132700       MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                        
132800       PERFORM UNTIL NOT SEGMENT-FOUND                                    
132900        PERFORM IMS-GNP-WDE131                                            
133000        PERFORM UNTIL NOT SEGMENT-FOUND                                   
133100         PERFORM S44A-LAES-TOTKOST                                        
133200         PERFORM IMS-GNP-WDE131                                           
133300        END-PERFORM                                                       
133400        PERFORM IMS-GNP-WDE121                                            
133500        MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                      
133600        MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                       
133700       END-PERFORM                                                        
133800                                                                          
133900       PERFORM IMS-GNP-WDE111                                             
134000       MOVE SGMT-IDKUNDNR     TO W-WDE111-IDKUNDNR                        
134100     END-PERFORM                                                          
134200     .                                                                    
134300     EJECT                                                                
134400                                                                          
134500 S44A-LAES-TOTKOST  SECTION.                                              
134600                                                                          
134700     IF DIST35-CDC-AE-REFILL                                              
134800       IF SRAD-PRARTNTO > ZERO                                            
134900         COMPUTE W-SUPRIS-USD = SRAD-PRARTNTO / WS-PRKURS-USD             
135100         COMPUTE W-SUPRIS-TEST = W-SUPRIS-USD * SRAD-KVLEVART             
135200         MOVE 'USD'             TO W-KDVALISO                             
135400         ADD W-SUPRIS-TEST      TO W-SUPRIS-TOT                           
135500       END-IF                                                             
135501     ELSE                                                                 
135510       IF DIST18-SCRAP-NDC-SC        OR                                   
135520          DIST18-SCRAP-NDC-QUAL      OR                                   
135530          DIST18-SCRAP-NDC-SC-LOCAL  OR                                   
135540          DIST20-EMBALLAGE-SDC       OR                                   
135550          DIST35-AE-CDC-RETURNS                                           
135580         COMPUTE W-SUPRIS-TEST = SRAD-PRAVCOST * SRAD-KVLEVART            
135590         MOVE 'USD'             TO W-KDVALISO                             
135592         ADD W-SUPRIS-TEST      TO W-SUPRIS-TOT                           
135600       ELSE                                                               
135700         IF SRAD-PRARTNTO > ZERO                                          
135800           COMPUTE W-SUPRIS-USD = SRAD-PRARTNTO / WS-PRKURS-USD           
136000           COMPUTE W-SUPRIS-TEST = W-SUPRIS-USD * SRAD-KVLEVART           
136100           MOVE 'USD'           TO W-KDVALISO                             
136300           ADD W-SUPRIS-TEST    TO W-SUPRIS-TOT                           
136400         END-IF                                                           
136410       END-IF                                                             
136500     END-IF                                                               
136600     .                                                                    
136700     EJECT                                                                
136800 S50-HAMTA-WDD3-INFO SECTION.                                             
136900                                                                          
137000     PERFORM IMS-GU-WDD301                                                
137100                                                                          
137200     MOVE 'GB ' TO            W-IDSKYLT                                   
137300     PERFORM IMS-GNP-WDD311                                               
137400     IF SEGMENT-FOUND                                                     
137500         MOVE TEXT-BEART TO RAD3-DESCRIPTION                              
137600     END-IF                                                               
137700     .                                                                    
137800     EJECT                                                                
137900                                                                          
138000 S60-LAES-WDK6 SECTION.                                                   
138100                                                                          
138200*    MOVE SRAD-IDARTNR            TO W-IDARTNR                            
138300     PERFORM IMS-GU-WDK601                                                
138400     PERFORM IMS-GNP-WDK611                                               
138500     .                                                                    
138600     EJECT                                                                
138700                                                                          
138800 S70-COUNT-TILLAGG SECTION.                                               
138900                                                                          
139000     IF DIST35-CDC-AE-REFILL                                              
139100       COMPUTE WS-TOT-TILLAGG = TILL-PREMBHNT + TILL-PRFRAKT +            
139200                                TILL-PRFOERS + TILL-PRLEGKST              
139300       COMPUTE WS-TOT-TILLAGG-USD = WS-TOT-TILLAGG /                      
139400                                    WS-PRKURS-USD                         
139500     ELSE                                                                 
139600       COMPUTE WS-TOT-TILLAGG-USD = TILL-PREMBHNT + TILL-PRFRAKT +        
139700                                    TILL-PRFOERS + TILL-PRLEGKST          
139800     END-IF                                                               
139900     .                                                                    
140000     EJECT                                                                
140100                                                                          
140200 S87-MOVE-SPACE-RAD2 SECTION.                                             
140300                                                                          
140400     MOVE SPACE             TO RAD2-IDSTATNR                              
140500                               RAD2-DESCRIPTION                           
140600                               RAD2-KDARTURS-LAND                         
140700                               RAD2-ASTERISK                              
140800                               RAD2-KVART                                 
140900                               RAD2-VKORDNTO                              
141000                               RAD2-VKORDBTO                              
141100                               RAD2-SUORDV-TOT                            
141200                               RAD2-KDVALISO                              
141300     .                                                                    
141400     EJECT                                                                
141500 S88-MOVE-SPACE-RAD3 SECTION.                                             
141600                                                                          
141700*    MOVE SPACE             TO RAD3-IDSTATNR                              
141800     MOVE SPACE             TO RAD3-STATNR                                
141900     MOVE SPACE             TO RAD3-DESCRIPTION                           
142000                               RAD3-KDARTURS-LAND                         
142100                               RAD3-ASTERISK                              
142200*                              RAD3-KVART                                 
142300*                              RAD3-VKORDNTO                              
142400*                              RAD3-VKORDBTO                              
142500*                              RAD3-SUORDV-TOT                            
142600                               RAD3-KDVALISO                              
142700                                                                          
142800*    MOVE ZERO              TO RAD3-IDSTATNR                              
142900     MOVE ZERO              TO RAD3-KVART                                 
143000                               RAD3-VKORDNTO                              
143100                               RAD3-VKORDBTO                              
143200                               RAD3-SUORDV-TOT                            
143300     .                                                                    
143400     EJECT                                                                
143500                                                                          
143600 S89-MOVE-SPACE-RAD-HEAD SECTION.                                         
143700                                                                          
143800     MOVE SPACE             TO RAD1H-INVOICE-TEXT                         
143900                               RAD1H-INVOICE                              
144000     .                                                                    
144100     EJECT                                                                
144200                                                                          
144300 S90-PUT-DOC-LINE SECTION.                                                
144400     IF TRPD-FLSKRIV-ONDEM = YES                                          
144500       MOVE +1                            TO SEND-IDCOM                   
144600       MOVE 'PUT'                         TO SEND-KDFUNC                  
144700*      MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                  
144800       MOVE LENGTH OF SEND-RAD         TO SEND-KVDLEN                     
144900       CALL WZ01SEND USING SEND-CONTROL-AREA                              
145000                           SEND-KVDLEN                                    
145100                           SEND-RAD                                       
145200*                          SEND-RAD-STYRTECKEN                            
145300       IF SEND-KDRC > ZERO                                                
145400         MOVE SEND-KDRC                   TO KDRC-DISPLAY                 
145500         STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                     
145600         DELIMITED BY SIZE INTO ERRTEXT-STR                               
145700         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
145800       END-IF                                                             
145900     END-IF                                                               
146000     .                                                                    
146100     EJECT                                                                
146200                                                                          
146300* --- IMS SECTIONS  ---                                                   
146400                                                                          
146500 IMS-GU-WDE101-11  SECTION.                                               
146600                                                                          
146700     STRING 'WDE101  *PD(IDSHIPM  =' W-IDSHIPM-X ')'                      
146800          DELIMITED BY SIZE INTO SSA1                                     
146900     STRING 'WDE111  *D(WDE111KY>=' W-WDE111KY-MIN                        
147000                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
147100          DELIMITED BY SIZE INTO SSA2                                     
147200*    STRING 'WDE121  *D(WDE121KY>=' W-WDE121KY-MIN                        
147300*                   '&WDE121KY<=' W-WDE121KY-MAX ')'                      
147400*         DELIMITED BY SIZE INTO SSA3                                     
147500*    MOVE   'WDE131 ' TO SSA4                                             
147600     MOVE '  GE' TO GOOD-STATUSCODES                                      
147700     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101-11-21-31                
147800                                         SSA1 SSA2                        
147900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
148000     PERFORM IMS-STATUSCHECK                                              
148100     .                                                                    
148200     EJECT                                                                
148300                                                                          
148400*IMS-GNP-WDE131     SECTION.                                              
148500                                                                          
148600** READ ALL THE WDE131 SEGMENT UNDER THE SAME DISTRICT                    
148700                                                                          
148800*    STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
148900*                   '&WDE111KY<=' W-WDE111KY-MAX ')'                      
149000*         DELIMITED BY SIZE INTO SSA1                                     
149100*    STRING 'WDE121  (WDE121KY>=' W-WDE121KY-MIN                          
149200*                   '&WDE121KY<=' W-WDE121KY-MAX ')'                      
149300*         DELIMITED BY SIZE INTO SSA2                                     
149400*    MOVE   'WDE131  '  TO SSA3                                           
149500*    MOVE '  GEGB' TO GOOD-STATUSCODES                                    
149600*    CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
149700*                                                  SSA3                   
149800*    MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
149900*    PERFORM IMS-STATUSCHECK                                              
150000*    .                                                                    
150100*    EJECT                                                                
150200 IMS-GNP-WDE111 SECTION.                                                  
150300                                                                          
150400     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
150500          DELIMITED BY SIZE INTO SSA1                                     
150600     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
150700                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
150800          DELIMITED BY SIZE INTO SSA2                                     
150900     MOVE '  GE' TO GOOD-STATUSCODES                                      
151000     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
151100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
151200     PERFORM IMS-STATUSCHECK                                              
151300     .                                                                    
151400     EJECT                                                                
151500 IMS-GNP-WDE121  SECTION.                                                 
151600                                                                          
151700     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
151800          DELIMITED BY SIZE INTO SSA1                                     
151900     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
152000          DELIMITED BY SIZE INTO SSA2                                     
152100     MOVE 'WDE121  '          TO SSA3                                     
152200     MOVE '  GE' TO GOOD-STATUSCODES                                      
152300     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3         
152400     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
152500     PERFORM IMS-STATUSCHECK                                              
152600     .                                                                    
152700     EJECT                                                                
152800 IMS-GNP-WDE122 SECTION.                                                  
152900                                                                          
153000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
153100          DELIMITED BY SIZE INTO SSA1                                     
153200     MOVE 'WDE122  '          TO SSA2                                     
153300     MOVE '  GE' TO GOOD-STATUSCODES                                      
153400     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE122 SSA1 SSA2              
153500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
153600     PERFORM IMS-STATUSCHECK                                              
153700     .                                                                    
153800     EJECT                                                                
153900 IMS-GNP-WDE131  SECTION.                                                 
154000                                                                          
154100     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
154200          DELIMITED BY SIZE INTO SSA1                                     
154300     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
154400          DELIMITED BY SIZE INTO SSA2                                     
154500     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
154600          DELIMITED BY SIZE INTO SSA3                                     
154700     MOVE 'WDE131  '          TO SSA4                                     
154800     MOVE '  GE' TO GOOD-STATUSCODES                                      
154900     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
155000                                                   SSA3 SSA4              
155100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
155200     PERFORM IMS-STATUSCHECK                                              
155300     .                                                                    
155400     EJECT                                                                
155500 IMS-GU-WDB101 SECTION.                                                   
155600                                                                          
155700     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
155800          DELIMITED BY SIZE INTO SSA1                                     
155900     MOVE '  GE' TO GOOD-STATUSCODES                                      
156000     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
156100     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
156200     PERFORM IMS-STATUSCHECK                                              
156300     .                                                                    
156400     EJECT                                                                
156500 IMS-GU-WDB201 SECTION.                                                   
156600                                                                          
156700     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
156800          DELIMITED BY SIZE INTO SSA1                                     
156900     MOVE '  GE' TO GOOD-STATUSCODES                                      
157000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
157100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
157200     PERFORM IMS-STATUSCHECK                                              
157300     .                                                                    
157400     EJECT                                                                
157500 IMS-GU-WDD301           SECTION.                                         
157600                                                                          
157700     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
157800             DELIMITED BY SIZE INTO SSA1                                  
157900     MOVE '  ' TO GOOD-STATUSCODES                                        
158000     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
158100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
158200     PERFORM IMS-STATUSCHECK                                              
158300     SKIP2                                                                
158400     .                                                                    
158500 IMS-GNP-WDD311            SECTION.                                       
158600                                                                          
158700     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
158800             DELIMITED BY SIZE INTO SSA1                                  
158900     MOVE '  GE' TO GOOD-STATUSCODES                                      
159000     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
159100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
159200     PERFORM IMS-STATUSCHECK                                              
159300     .                                                                    
159400     EJECT                                                                
159500 IMS-GU-WDK601           SECTION.                                         
159600                                                                          
159700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
159800            DELIMITED BY SIZE INTO SSA1                                   
159900     MOVE '  GE' TO GOOD-STATUSCODES                                      
160000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
160100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
160200     PERFORM IMS-STATUSCHECK                                              
160300     .                                                                    
160400                                                                          
160500 IMS-GNP-WDK611      SECTION.                                             
160600                                                                          
160700     MOVE 'WDK611  ' TO SSA1                                              
160800     MOVE '  ' TO GOOD-STATUSCODES                                        
160900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
161000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
161100     PERFORM IMS-STATUSCHECK                                              
161200     .                                                                    
161300                                                                          
161400 IMS-STATUSCHECK SECTION.                                                 
161500                                                                          
161600     SET STATUS-IX TO 1                                                   
161700     SEARCH GOOD-STATUS                                                   
161800       AT END                                                             
161900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
162000           DELIMITED BY SIZE INTO ERRTEXT                                 
162100         DISPLAY ERRTEXT                                                  
162200         CALL FELLOG                                                      
162300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
162400         CONTINUE                                                         
162500     END-SEARCH                                                           
162600     .                                                                    
162700                                                                          
