000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2162000.                                    
000300*AUTHOR.         STEFAN KIHLBERG                                          
000400*DATE-WRITTEN.   FEBRUARI 1994.                                           
000500*    SKIP2                                                                
000600*    REMARKS.                                                             
000700*                                                                         
000800*    SYSTEM.                                                              
000900*        SKROTBEORDRING.                                                  
001000*                                                                         
001100*    FUNKTION.                                                            
001200*        EFTER INTERNSORTERING AV FILEN W21611 PÅ ANSKAFFARE              
001300*        SKAPAS FÖR VARJE POST (ARTIKEL) EN SKROTORDER.                   
001400*                                                                         
001500*    RETURKODER.                                                          
001600*        U0020   FEL I SORTERINGEN.                                       
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000 FILE-CONTROL.                                                            
002100     SKIP3                                                                
002200* INPUT                                                                   
002300* LISTPOSTER SKROTORDER                                                   
002400                                                                          
002500     SELECT W21611 ASSIGN W21620D1.                                       
002600                                                                          
002700* OUTPUT                                                                  
002800* SKROTORDERLISTA                                                         
002900                                                                          
003000     SELECT W21621  ASSIGN W21620D3.                                      
003100*QUAL                                                                     
003200     SELECT W21622  ASSIGN W21620D4.                                      
003300                                                                          
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 FILE SECTION.                                                            
003700     SKIP2                                                                
003800 FD  W21611                                                               
003900     RECORDING F                                                          
004000     BLOCK 0.                                                             
004100*01  POST -COPY W21611     -PRE W21611 -L                                 
004200                                                                          
004300 FD  W21621                                                               
004400     LABEL RECORD STANDARD                                                
004500     RECORDING      V                                                     
004600     BLOCK CONTAINS 0.                                                    
004700     SKIP3                                                                
004800 01  W21621-RAD         PIC X(121).                                       
004900                                                                          
005000                                                                          
005100 FD  W21622                                                               
005200     LABEL RECORD STANDARD                                                
005300     RECORDING      V                                                     
005400     BLOCK CONTAINS 0.                                                    
005500     SKIP3                                                                
005600 01  W21622-RAD         PIC X(121).                                       
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000                                                                          
006100*    -- CHECKED BY WY2000                                                 
006200 77  JA                  PIC X       VALUE 'J'.                           
006300 77  NEJ                 PIC X       VALUE 'N'.                           
006400                                                                          
006500 77  FELTEXT             PIC X(80)   VALUE SPACE.                         
006600                                                                          
006700 77  SIDA-SW             PIC 9       VALUE 1.                             
006800     88  SIDA1                       VALUE 1.                             
006900     88  SIDA2                       VALUE 2.                             
007000 77  WS-DASKROT9-BEORD           PIC 9(8)    VALUE ZERO.                  
007100                                                                          
007200 01  IX-DC               PIC 9(3) COMP-3  VALUE ZERO.                     
007300 01  IX-DISTR            PIC 9(3) COMP-3  VALUE ZERO.                     
007400 01  IX-KAT              PIC 9(3) COMP-3  VALUE ZERO.                     
007500 01  IX-KAT2             PIC 9(3) COMP-3  VALUE ZERO.                     
007600 01  IX-USER             PIC 9(3) COMP-3  VALUE ZERO.                     
007700 01  IX-NAMN             PIC 9(3) COMP-3  VALUE ZERO.                     
007800 01  TEXT-IX             PIC 9(3) COMP-3  VALUE ZERO.                     
007900     SKIP3                                                                
008000*--------------------------------------- EOF-SWITCHAR                     
008100 01  EOF-SWITCHAR.                                                        
008200     05  W21611-EOF          PIC X       VALUE 'N'.                       
008300     EJECT                                                                
008400*--------------------------------------- ALLMÄNNA ARBETSAREOR             
008500 01  WS.                                                                  
008600     05  WS-DDATUM.                                                       
008700         10  WS-DDATUMAA     PIC 9(2).                                    
008800         10  WS-DDATUMMM     PIC 9(2).                                    
008900         10  WS-DDATUMDD     PIC 9(2).                                    
009000     05  WS-CLAGER           OCCURS 2.                                    
009100         10  WS-LAGERTILLG   PIC S9(9)               COMP-3.              
009200         10  WS-AKTILLG      PIC S9(9)               COMP-3.              
009300         10  WS-SKROTANT-X.                                               
009400             15 WS-SKROTANT   PIC -(6)9.                                  
009500         10  WS-SKROTKVAR-X.                                              
009600             15 WS-SKROTKVAR  PIC -(6)9.                                  
009700     05  WS-SUTPO-TOT        PIC S9(9)               COMP-3.              
009800     05  WS-ANTAL            PIC S9(9)               COMP-3.              
009900                                                                          
010000     03  WS-ADBUFFOMR-1-X.                                                
010100        05  WS-ADBUFFOMR-1   PIC 9(2).                                    
010200     03  WS-ADBUFFGANG-1-X.                                               
010300        05  WS-ADBUFFGANG-1  PIC 9(2).                                    
010400     03  WS-ADBUFFPL-1-X.                                                 
010500       05  WS-ADBUFFPL-1     PIC 9(5).                                    
010600                                                                          
010700     03  WS-ADBUFFOMR-2-X.                                                
010800       05  WS-ADBUFFOMR-2    PIC 9(2).                                    
010900     03  WS-ADBUFFGANG-2-X.                                               
011000       05  WS-ADBUFFGANG-2   PIC 9(2).                                    
011100     03  WS-ADBUFFPL-2-X.                                                 
011200        05  WS-ADBUFFPL-2    PIC 9(5).                                    
011300                                                                          
011400     03  WS-ADBUFFOMR-3-X.                                                
011500       05  WS-ADBUFFOMR-3    PIC 9(2).                                    
011600     03  WS-ADBUFFGANG-3-X.                                               
011700       05  WS-ADBUFFGANG-3   PIC 9(2).                                    
011800     03  WS-ADBUFFPL-3-X.                                                 
011900        05  WS-ADBUFFPL-3    PIC 9(5).                                    
012000     03  IDDC-WS             PIC 9(2).                                    
012100     03  WS-IDPERSON         PIC 9(3).                                    
012200     03  DAGENS-DATUM        PIC 9(6)   VALUE ZERO.                       
012300                                                                          
012400*                                     HEADER RECORDS TO D&P               
012500 01  WS-HEADER-RECS.                                                      
012600     03 WS-HDR-1-IDOUTTYPE    PIC X(15).                                  
012700     03 WS-HDR-2-IDOUTREC     PIC X(30).                                  
012800                                                                          
012900 01  WS-DAP                   PIC X(05) VALUE ' ¤DAP'.                    
013000 01  WS-META                  PIC X(06) VALUE ' ¤META'.                   
013100                                                                          
013200 01  TEXTER.                                                              
013300     05  TEXT-FLER-BUFF      PIC X(80)  VALUE                             
013400            'FLER BUFFERTADRESSER FINNS'.                                 
013500     SKIP2                                                                
013600 01  PV-VAERDEN.                                                          
013700     03  WS-SKROTKONTO-PV    PIC X(10)   VALUE '5042000001'.              
013800     03  WS-PV-IDDISTR       PIC XX      VALUE '81'.                      
013900     SKIP2                                                                
014000 01  WS-IDDISTR              PIC XX.                                      
014100 01  WS-SKROTKONTO           PIC X(10).                                   
014200                                                                          
       01  WS-TIMESTAMP.                                                        
           03  FILLER                  PIC X       VALUE 'D'.                   
           03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
           03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
           03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
           03  FILLER                  PIC X       VALUE '_'.                   
           03  FILLER                  PIC X       VALUE 'T'.                   
           03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
           03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
           03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
014200                                                                          
014300 01  WS-IDANSK.                                                           
014400     03  WS-IDANSK-POS1      PIC X       VALUE SPACE.                     
014500     03  WS-IDANSK-POS2-3    PIC X(2)    VALUE SPACE.                     
014600 01  WS-IDANSK-NUM           PIC 9(3)    VALUE ZERO.                      
014700     EJECT                                                                
014800*--------------------------------------- GENERELLA SUBPROGRAM             
014900                                                                          
015000 01  DYNAMISKA-SUBPROGRAM.                                                
015100     05  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
015200     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
015300     05  ABEND               PIC X(8)    VALUE 'ABEND'.                   
015400     05  W00903              PIC X(8)    VALUE 'W00903  '.                
015500     05  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
015600     05  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
015700     SKIP3                                                                
015800*--------------------------------------- PARAMETRAR TILL DATKORT          
015900                                                                          
016000 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W21620'.                  
016100                                                                          
016200 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
016300                                                                          
016400*01  -COPY WDATKORT                                                       
016500     EJECT                                                                
016600*--------------------------------------- PARAMETRAR TILL POSTSUM          
016700                                                                          
016800*01  -COPY W0005      -PRE POSTSUM-                                       
016900     EJECT                                                                
017000*--------------------------------------- AREA FÖR W21611-POST             
017100                                                                          
017200*01  AREA -COPY W21611     -PRE W21611-                                   
017300     EJECT                                                                
017400                                                                          
017500*  LISTRADER                                                              
017600*******************SIDA1***************************                       
017700 01  DETRAD1.                                                             
017800     03  FILLER                PIC X(01).                                 
017900     03  UT-IDUSER             PIC X(08).                                 
018000     03  FILLER                PIC X(02)   VALUE SPACE.                   
018100     03  UT-BEANST             PIC X(25).                                 
018200     03  FILLER                PIC X(03)   VALUE SPACE.                   
018300     03  UT-IDANSK-AVD         PIC X(5).                                  
018400     03  FILLER                PIC X(15)   VALUE SPACE.                   
018500     03  UT-DDATUM             PIC X(6).                                  
018600     03  UT-SIDTYP             PIC X(2).                                  
018700                                                                          
018800                                                                          
018900 01  DETRAD2.                                                             
019000     03  FILLER                PIC X(01).                                 
019100     03  FILLER                PIC X(01)   VALUE SPACE.                   
019200     03  UT-IDDISTR            PIC Z(04).                                 
019300     03  FILLER                PIC X(02)   VALUE SPACE.                   
019400     03  UT-IDKUNDNR           PIC Z(06).                                 
019500     03  FILLER                PIC X(05)   VALUE SPACE.                   
019600     03  UT-IDDC               PIC X(02).                                 
019700     03  FILLER                PIC X(02)   VALUE SPACE.                   
019800     03  UT-SKROTKONTO         PIC Z(10).                                 
019900     03  FILLER                PIC X(03)   VALUE SPACE.                   
020000     03  UT-IDANALYS           PIC X(12)   VALUE SPACE.                   
020100     03  FILLER                PIC X(02)   VALUE SPACE.                   
020200     03  UT-KST                PIC X(10).                                 
020300                                                                          
020400                                                                          
020500 01  DETRAD3.                                                             
020600     03  FILLER                PIC X(01).                                 
020700     03  FILLER                PIC X(08)    VALUE SPACE.                  
020800     03  UT-IDARTNR            PIC Z(09).                                 
020900     03  FILLER                PIC X(02)    VALUE SPACE.                  
021000     03  UT-BEART              PIC X(25).                                 
021100                                                                          
021200                                                                          
021300 01  DETRAD4.                                                             
021400     03  FILLER                PIC X(01).                                 
021500     03  FILLER                PIC X(01)     VALUE SPACE.                 
021600     03  FILLER                PIC X(08)     VALUE SPACE.                 
021700     03  FILLER                PIC X(03)     VALUE SPACE.                 
021800     03  FILLER                PIC X(05)     VALUE SPACE.                 
021900     03  UT-TYP-SDC-1          PIC X(03).                                 
022000     03  FILLER                PIC X(03)     VALUE SPACE.                 
022100     03  FILLER                PIC X(08)     VALUE SPACE.                 
022200     03  FILLER                PIC X(03)     VALUE SPACE.                 
022300     03  FILLER                PIC X(05)     VALUE SPACE.                 
022400     03  UT-TYP-SDC-2          PIC X(03).                                 
022500                                                                          
022600                                                                          
022700 01  DETRADB.                                                             
022800     03  FILLER                PIC X(01).                                 
022900     03  FILLER                PIC X(01)     VALUE SPACE.                 
023000     03  UT-LAGERTILLG-CDC     PIC -(7)9.                                 
023100     03  FILLER                PIC X(03)     VALUE SPACE.                 
023200     03  UT-LAGERTILLG-SDC     PIC -(7)9.                                 
023300     03  FILLER                PIC X(03)     VALUE SPACE.                 
023400     03  UT-AKTILLG-CDC        PIC -(7)9.                                 
023500     03  FILLER                PIC X(03)     VALUE SPACE.                 
023600     03  UT-AKTILLG-SDC        PIC -(7)9.                                 
023700     03  FILLER                PIC X(05)     VALUE SPACE.                 
023800     03  UT-SUTPO-TOT          PIC -(7)9.                                 
023900     03  FILLER                PIC X(04)     VALUE SPACE.                 
024000     03  UT-KDERS-UTG          PIC Z99.                                   
024100                                                                          
024200                                                                          
024300 01  DETRAD5.                                                             
024400     03  FILLER                PIC X(01).                                 
024500     03  FILLER                PIC X(03)       VALUE SPACE.               
024600     03  UT-SKROTANT-1         PIC 9(07).                                 
024700     03  FILLER                PIC X(04)       VALUE SPACE.               
024800     03  UT-SKROTANT-2         PIC 9(07).                                 
024900     03  FILLER                PIC X(04)       VALUE SPACE.               
025000     03  UT-SKROTKVAR-1        PIC 9(07).                                 
025100     03  FILLER                PIC X(04)       VALUE SPACE.               
025200     03  UT-SKROTKVAR-2        PIC 9(07).                                 
025300     03  FILLER                PIC X(12)       VALUE SPACE.               
025400     03  UT-SKROTVARDE         PIC -(9)9.9(2).                            
025500                                                                          
025600                                                                          
025700 01  DETRAD6.                                                             
025800     03  FILLER                PIC X(01).                                 
025900     03  FILLER                PIC X(05)       VALUE SPACE.               
026000     03  UT-ADLAGOMR           PIC 9(02).                                 
026100     03  FILLER                PIC X(01)       VALUE SPACE.               
026200     03  UT-ADGANG             PIC 9(02).                                 
026300     03  FILLER                PIC X(01)       VALUE SPACE.               
026400     03  UT-ADPLATS            PIC Z9(4).                                 
026500     03  FILLER                PIC X(06)       VALUE SPACE.               
026600     03  UT-ADBUFFOMR-1        PIC 9(02).                                 
026700     03  FILLER                PIC X(01)       VALUE SPACE.               
026800     03  UT-ADBUFFGANG-1       PIC 9(02).                                 
026900     03  FILLER                PIC X(01)       VALUE SPACE.               
027000     03  UT-ADBUFFPL-1         PIC Z9(4).                                 
027100     03  FILLER                PIC X(07)       VALUE SPACE.               
027200     03  UT-ADBUFFOMR-2        PIC 9(02).                                 
027300     03  FILLER                PIC X(01)       VALUE SPACE.               
027400     03  UT-ADBUFFGANG-2       PIC 9(02).                                 
027500     03  FILLER                PIC X(01)       VALUE SPACE.               
027600     03  UT-ADBUFFPL-2         PIC Z9(4).                                 
027700     03  FILLER                PIC X(06)       VALUE SPACE.               
027800     03  UT-ADBUFFOMR-3        PIC 9(02).                                 
027900     03  FILLER                PIC X(01)       VALUE SPACE.               
028000     03  UT-ADBUFFGANG-3       PIC 9(02).                                 
028100     03  FILLER                PIC X(01)       VALUE SPACE.               
028200     03  UT-ADBUFFPL-3         PIC Z9(4).                                 
028300                                                                          
028400                                                                          
028500 01  DETRAD7.                                                             
028600     03  FILLER                PIC X(01).                                 
028700     03  FILLER                PIC X(05)       VALUE SPACE.               
028800     03  FILLER OCCURS 10.                                                
028900         05  UT-BEEMBLEM-7     PIC X(05).                                 
029000         05  FILLER            PIC X(01)       VALUE SPACE.               
029100                                                                          
029200                                                                          
029300 01  DETRAD8.                                                             
029400     03  FILLER                PIC X(01).                                 
029500     03  FILLER                PIC X(05)       VALUE SPACE.               
029600     03  FILLER OCCURS 10.                                                
029700         05  UT-BEEMBLEM-8     PIC X(05).                                 
029800         05  FILLER            PIC X(01)       VALUE SPACE.               
029900                                                                          
030000                                                                          
030100 01  DETRAD9.                                                             
030200     03  FILLER                PIC X(01).                                 
030300     03  UT-RAD9               PIC X(80).                                 
030400                                                                          
030500 01  DETRADN.                                                             
030600     03  FILLER                PIC X(01).                                 
030700     03  UT-RADN               PIC X(80).                                 
030800                                                                          
030900 01  DETRADX.                                                             
031000     03  FILLER                PIC X(01).                                 
031100     03  UT-IDUSER-GODK        PIC X(80).                                 
031200                                                                          
031300 01  DETRADZ.                                                             
031400     03  FILLER                PIC X(01).                                 
031500     03  UT-BEANST-GODK        PIC X(80).                                 
031600                                                                          
031700 01  DETRADD.                                                             
031800     03  FILLER                PIC X(01).                                 
031900     03  UT-TIDATE             PIC X(80).                                 
032000******************SIDA1****************************                       
032100                                                                          
032200                                                                          
032300******************SIDA2****************************                       
032400 01  DETRADTEXT1.                                                         
032500     03  FILLER                PIC X(01).                                 
032600     03  UT-TEMEMO1            PIC X(66).                                 
032700                                                                          
032800 01  DETRADTEXT2.                                                         
032900     03  FILLER                PIC X(01).                                 
033000     03  UT-TEMEMO2            PIC X(66).                                 
033100                                                                          
033200 01  DETRADTEXT3.                                                         
033300     03  FILLER                PIC X(01).                                 
033400     03  UT-TEMEMO3            PIC X(66).                                 
033500                                                                          
033600 01  DETRADTEXT4.                                                         
033700     03  FILLER                PIC X(01).                                 
033800     03  UT-TEMEMO4            PIC X(66).                                 
033900                                                                          
034000 01  DETRADTEXT5.                                                         
034100     03  FILLER                PIC X(01).                                 
034200     03  UT-TEMEMO5            PIC X(66).                                 
034300                                                                          
034400 01  DETRADTEXT6.                                                         
034500     03  FILLER                PIC X(01).                                 
034600     03  UT-TEMEMO6            PIC X(66).                                 
034700                                                                          
034800 01  DETRADTEXT7.                                                         
034900     03  FILLER                PIC X(01).                                 
035000     03  UT-TEMEMO7            PIC X(66).                                 
035100                                                                          
035200 01  DETRADTEXT8.                                                         
035300     03  FILLER                PIC X(01).                                 
035400     03  UT-TEMEMO8            PIC X(66).                                 
035500                                                                          
035600 01  DETRADTEXT9.                                                         
035700     03  FILLER                PIC X(01).                                 
035800     03  UT-TEMEMO9            PIC X(66).                                 
035900                                                                          
036000 01  DETRADTEXT10.                                                        
036100     03  FILLER                PIC X(01).                                 
036200     03  UT-TEMEMO10           PIC X(66).                                 
036300                                                                          
036400 01  DETRADTEXT11.                                                        
036500     03  FILLER                PIC X(01).                                 
036600     03  UT-TEMEMO11           PIC X(66).                                 
036700                                                                          
036800 01  DETRADX2.                                                            
036900     03  FILLER                PIC X(01).                                 
037000     03  UT-IDUSER-GODK2       PIC X(80).                                 
037100                                                                          
037200 01  DETRADZ2.                                                            
037300     03  FILLER                PIC X(01).                                 
037400     03  UT-BEANST-GODK2       PIC X(80).                                 
037500                                                                          
037600 01  DETRADD2.                                                            
037700     03  FILLER                PIC X(01).                                 
037800     03  UT-TIDATE2            PIC X(80).                                 
037900******************SIDA2****************************                       
038000                                                                          
038100     EJECT                                                                
038200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
038300*                                                                         
038400     EJECT                                                                
038500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
038600     SKIP3                                                                
038700                                                                          
038800 01  NYCKLAR-TILL-DLI.                                                    
038900     03  W-IDDC-B6-X.                                                     
039000         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
039100                                                                          
039200*    --- STATUS-KOD FRÅN IMS                                              
039300 01  STATUS-WS                   PIC XX.                                  
039400     88  SEGMENT-FINNS                       VALUE '  '.                  
039500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
039600                                                                          
039700 01  GODK-STATUSKODER.                                                    
039800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
039900                                                                          
040000 01  SSA1                        PIC X(128).                              
040100                                                                          
040200*    --- IMS FUNKTIONSKODER                                               
040300*01  -COPY W0003                                                          
040400                                                                          
040500*    ---  DLI INPUT-OUTPUT AREA                                           
040600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ART801'.             
040700                                                                          
040800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
040900 01   DLI-IO-AREA-B601.                                                   
041000*     03  -COPY WDB601                                                    
041100                                                                          
041200 LINKAGE SECTION.                                                         
041300                                                                          
041400*01  -COPY W0008  -PRE  WDB6-                                             
041500     05  FILLER                  PIC X.                                   
041600                                                                          
041700                                                                          
041800 PROCEDURE DIVISION USING WDB6-PCB.                                       
041900                                                                          
042000 MAIN SECTION.                                                            
042100     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
042200                                                                          
042300     PERFORM A-INITIERING                                                 
042400     PERFORM S11-LAS-W21611                                               
042500     PERFORM UNTIL W21611-EOF = JA                                        
042600        PERFORM C-VALJ-SIDA                                               
042700        PERFORM S11-LAS-W21611                                            
042800     END-PERFORM                                                          
042900     PERFORM Z-AVSLUTNING                                                 
043000     MOVE ZERO TO RETURN-CODE                                             
043100     GOBACK                                                               
043200     .                                                                    
043300     EJECT                                                                
043400                                                                          
043500                                                                          
043600 A-INITIERING SECTION.                                                    
043700                                                                          
043800     OPEN  INPUT W21611                                                   
043900          OUTPUT W21621                                                   
044000                 W21622                                                   
044100                                                                          
044200     ACCEPT DAGENS-DATUM FROM DATE                                        
                                                                                
           MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
           MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
           MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
           MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
           MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
           MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
                                                                                
044400     MOVE 001 TO POSTSUM-TRANSTYP                                         
044500     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
044600                                                                          
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000                                                                          
045100 C-VALJ-SIDA SECTION.                                                     
045200                                                                          
045300     PERFORM CA-FORSTA-SIDA                                               
045400     IF W21611-KDARBTYP = 'QUAL    '                                      
045500        PERFORM S90-SKRIV-DAP-SCRAP-B                                     
045600        PERFORM S91-SKRIV-META-SCRAP-B                                    
045700        PERFORM S22-SKRIV-DETRADER                                        
045800     ELSE                                                                 
045900        PERFORM S92-SKRIV-DAP-SCRAP-A                                     
046000        PERFORM S93-SKRIV-META-SCRAP-A                                    
046100        PERFORM S20-SKRIV-DETRADER                                        
046200     END-IF                                                               
046300     IF W21611-IDUSER-GODK(6) = LOW-VALUE OR SPACE                        
046400       MOVE +1         TO TEXT-IX                                         
046500       PERFORM UNTIL TEXT-IX > 11                                         
046600         IF W21611-TEMEMO(TEXT-IX) = SPACE                                
046700           CONTINUE                                                       
046800         ELSE                                                             
046900           PERFORM CB-ANDRA-SIDAN                                         
047000           IF W21611-KDARBTYP = 'QUAL    '                                
047300              PERFORM S22-SKRIV-DETRADER                                  
047400           ELSE                                                           
047700              PERFORM S20-SKRIV-DETRADER                                  
047800           END-IF                                                         
047900           MOVE +12    TO TEXT-IX                                         
048000         END-IF                                                           
048100         ADD +1        TO TEXT-IX                                         
048200       END-PERFORM                                                        
048300                                                                          
048400     ELSE                                                                 
048500       PERFORM CB-ANDRA-SIDAN                                             
048600       IF W21611-KDARBTYP = 'QUAL    '                                    
048700          PERFORM S22-SKRIV-DETRADER                                      
048800       ELSE                                                               
048900          PERFORM S20-SKRIV-DETRADER                                      
049000       END-IF                                                             
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400                                                                          
049500 CA-FORSTA-SIDA SECTION.                                                  
049600                                                                          
049700* DETALJRAD 1                                                             
049800                                                                          
049900     MOVE 'S1'          TO UT-SIDTYP                                      
050000     IF DCS-IDDC NOT = W21611-IDDC                                        
050100        MOVE W21611-IDDC    TO W-IDDC-B6                                  
050200        PERFORM IMS-GU-WDB601                                             
050300     END-IF                                                               
050400                                                                          
050500     IF W21611-IDDC NUMERIC                                               
050600        MOVE W21611-IDDC    TO WS-IDANSK-NUM                              
050700     ELSE                                                                 
050800        MOVE W21611-IDDC    TO WS-IDANSK-POS2-3                           
050900     END-IF                                                               
051000     MOVE W21611-IDUSER     TO UT-IDUSER                                  
051100     MOVE W21611-IDPERSON   TO WS-IDPERSON                                
051200     MOVE W21611-BEANST     TO UT-BEANST                                  
051300     MOVE W21611-IDAVD      TO UT-IDANSK-AVD                              
051400     COMPUTE WS-DASKROT9-BEORD = 99999999 - W21611-DASKROT9-BEORD         
051500     MOVE WS-DASKROT9-BEORD(3:6) TO UT-DDATUM                             
051600*    MOVE DAGENS-DATUM      TO UT-DDATUM                                  
051700                                                                          
051800                                                                          
051900* DETALJRAD 2                                                             
052000                                                                          
052100     MOVE W21611-IDDC       TO UT-IDDC                                    
052200     MOVE W21611-IDKONTO    TO UT-SKROTKONTO                              
052300     MOVE W21611-IDANALYS   TO UT-IDANALYS                                
052400     MOVE W21611-IDKST      TO UT-KST                                     
052500     MOVE W21611-IDDISTR    TO UT-IDDISTR                                 
052600                                                                          
052700*    MOVE W21611-IDDC    TO IDDC-WS                                       
052800     IF W21611-IDKUNDNR > ZERO                                            
052900       MOVE W21611-IDKUNDNR TO UT-IDKUNDNR                                
053000     ELSE                                                                 
053100       MOVE ZERO            TO UT-IDKUNDNR                                
053200     END-IF                                                               
053300                                                                          
053400                                                                          
053500                                                                          
053600* DETALJRAD 3                                                             
053700                                                                          
053800     MOVE W21611-IDARTNR    TO UT-IDARTNR                                 
053900     MOVE W21611-BEART      TO UT-BEART                                   
054000* DETALJRAD 4                                                             
054100                                                                          
054200     IF DCS-CDC                                                           
054300        MOVE 'TOT'          TO UT-TYP-SDC-1 UT-TYP-SDC-2                  
054400     ELSE                                                                 
054500        MOVE W21611-IDDC    TO UT-TYP-SDC-1 UT-TYP-SDC-2                  
054600     END-IF                                                               
054700     SKIP2                                                                
054800                                                                          
054900* DETALJRAD B                                                             
055000                                                                          
055100     MOVE W21611-KVTILLG-CDC TO UT-LAGERTILLG-CDC                         
055200     MOVE W21611-KVTILLG-SDC TO UT-LAGERTILLG-SDC                         
055300     MOVE W21611-KVAKS-CDC   TO UT-AKTILLG-CDC                            
055400     MOVE W21611-KVAKS-SDC   TO UT-AKTILLG-SDC                            
055500     MOVE W21611-KDERS-UTG   TO UT-KDERS-UTG                              
055600     MOVE W21611-SUTPO-TOT   TO UT-SUTPO-TOT                              
055700     EJECT                                                                
055800                                                                          
055900* DETALJRAD 5                                                             
056000                                                                          
056100     IF  DCS-CDC                                                          
056200       MOVE W21611-KVSKROT-BEORD TO WS-SKROTANT(1) WS-ANTAL               
056300       MOVE 'XXXXXXX' TO WS-SKROTANT-X(2)                                 
056400       MOVE W21611-KVSKROT-KVAR TO WS-SKROTKVAR(1)                        
056500       MOVE 'XXXXXXX' TO WS-SKROTKVAR-X(2)                                
056600     ELSE                                                                 
056700       MOVE 'XXXXXXX' TO WS-SKROTANT-X(1)                                 
056800       MOVE W21611-KVSKROT-BEORD TO WS-SKROTANT(2) WS-ANTAL               
056900       MOVE 'XXXXXXX' TO WS-SKROTKVAR-X(1)                                
057000       MOVE W21611-KVSKROT-KVAR TO WS-SKROTKVAR(2)                        
057100     END-IF                                                               
057200     MOVE WS-SKROTANT-X(1)     TO UT-SKROTANT-1                           
057300     MOVE WS-SKROTANT-X(2)     TO UT-SKROTANT-2                           
057400     MOVE WS-SKROTKVAR-X(1)    TO UT-SKROTKVAR-1                          
057500     MOVE WS-SKROTKVAR-X(2)    TO UT-SKROTKVAR-2                          
057600     COMPUTE UT-SKROTVARDE = W21611-PRARTSTD * WS-ANTAL                   
057700     EJECT                                                                
057800                                                                          
057900* DETALJRAD 6                                                             
058000                                                                          
058100                                                                          
058200*   ARTADDRESS                                                            
058300                                                                          
058400     MOVE W21611-ADLAGOMR  TO UT-ADLAGOMR                                 
058500     MOVE W21611-ADGANG    TO UT-ADGANG                                   
058600     MOVE W21611-ADPLATS   TO UT-ADPLATS                                  
058700                                                                          
058800                                                                          
058900*   BUFF1                                                                 
059000                                                                          
059100     IF W21611-ADBUFFOMR-1 > 0 OR                                         
059200        W21611-ADBUFFGANG-1 > 0 OR                                        
059300        W21611-ADBUFFPL-1 > 0                                             
059400        MOVE W21611-ADBUFFOMR-1     TO WS-ADBUFFOMR-1                     
059500        MOVE W21611-ADBUFFGANG-1    TO WS-ADBUFFGANG-1                    
059600        MOVE W21611-ADBUFFPL-1      TO WS-ADBUFFPL-1                      
059700     ELSE                                                                 
059800        MOVE SPACE                  TO WS-ADBUFFOMR-1-X                   
059900                                       WS-ADBUFFGANG-1-X                  
060000                                       WS-ADBUFFPL-1-X                    
060100     END-IF                                                               
060200                                                                          
060300*   BUFF2                                                                 
060400     IF W21611-ADBUFFOMR-2 > 0 OR                                         
060500        W21611-ADBUFFGANG-2 > 0 OR                                        
060600        W21611-ADBUFFPL-2 > 0                                             
060700        MOVE W21611-ADBUFFOMR-2     TO WS-ADBUFFOMR-2                     
060800        MOVE W21611-ADBUFFGANG-2    TO WS-ADBUFFGANG-2                    
060900        MOVE W21611-ADBUFFPL-2      TO WS-ADBUFFPL-2                      
061000     ELSE                                                                 
061100        MOVE SPACE                  TO WS-ADBUFFOMR-2-X                   
061200                                       WS-ADBUFFGANG-2-X                  
061300                                       WS-ADBUFFPL-2-X                    
061400     END-IF                                                               
061500                                                                          
061600     IF W21611-ADBUFFPL-3 > 0 OR                                          
061700        W21611-ADBUFFGANG-3 > 0 OR                                        
061800        W21611-ADBUFFPL-3 > 0                                             
061900        MOVE W21611-ADBUFFOMR-3     TO WS-ADBUFFOMR-3                     
062000        MOVE W21611-ADBUFFGANG-3    TO WS-ADBUFFGANG-3                    
062100        MOVE W21611-ADBUFFPL-3      TO WS-ADBUFFPL-3                      
062200     ELSE                                                                 
062300        MOVE SPACE                  TO WS-ADBUFFOMR-3-X                   
062400                                       WS-ADBUFFGANG-3-X                  
062500                                       WS-ADBUFFPL-3-X                    
062600     END-IF                                                               
062700                                                                          
062800     MOVE WS-ADBUFFOMR-1-X   TO UT-ADBUFFOMR-1                            
062900     MOVE WS-ADBUFFGANG-1-X  TO UT-ADBUFFGANG-1                           
063000     MOVE WS-ADBUFFPL-1-X    TO UT-ADBUFFPL-1                             
063100                                                                          
063200     MOVE WS-ADBUFFOMR-2-X   TO UT-ADBUFFOMR-2                            
063300     MOVE WS-ADBUFFGANG-2-X  TO UT-ADBUFFGANG-2                           
063400     MOVE WS-ADBUFFPL-2-X    TO UT-ADBUFFPL-2                             
063500                                                                          
063600     MOVE WS-ADBUFFOMR-3-X   TO UT-ADBUFFOMR-3                            
063700     MOVE WS-ADBUFFGANG-3-X  TO UT-ADBUFFGANG-3                           
063800     MOVE WS-ADBUFFPL-3-X    TO UT-ADBUFFPL-3                             
063900                                                                          
064000     EJECT                                                                
064100* DETALJRAD 7                                                             
064200                                                                          
064300                                                                          
064400*   KATALOG EMBLEM                                                        
064500                                                                          
064600     MOVE +1 TO IX-KAT                                                    
064700     PERFORM UNTIL IX-KAT > 10                                            
064800        MOVE W21611-BEEMBLEM (IX-KAT) TO UT-BEEMBLEM-7 (IX-KAT)           
064900        ADD +1 TO IX-KAT                                                  
065000     END-PERFORM                                                          
065100                                                                          
065200                                                                          
065300* DETALJRAD 8                                                             
065400                                                                          
065500                                                                          
065600*   KATALOG EMBLEM  FORTS.                                                
065700                                                                          
065800     MOVE +1 TO IX-KAT2                                                   
065900     PERFORM UNTIL IX-KAT > 20                                            
066000        MOVE W21611-BEEMBLEM (IX-KAT) TO UT-BEEMBLEM-8 (IX-KAT2)          
066100        ADD +1 TO IX-KAT IX-KAT2                                          
066200     END-PERFORM                                                          
066300                                                                          
066400                                                                          
066500* DETALJRAD 9                                                             
066600                                                                          
066700     IF W21611-ADBUFFOMR-4 > 0 OR                                         
066800        W21611-ADBUFFGANG-4 > 0 OR                                        
066900        W21611-ADBUFFPL-4 > 0                                             
067000        MOVE TEXT-FLER-BUFF    TO UT-RAD9                                 
067100     ELSE                                                                 
067200        MOVE SPACE             TO UT-RAD9                                 
067300     END-IF                                                               
067400                                                                          
067500* DETALJRAD N                                                             
067600                                                                          
067700     IF W21611-IDUSER-GODK(6) = LOW-VALUE OR SPACE                        
067800        MOVE SPACE             TO UT-RADN                                 
067900     ELSE                                                                 
068000        MOVE 'SEE MORE APPROVERS ON NEXT PAGE' TO UT-RADN                 
068100     END-IF                                                               
068200                                                                          
068300* DETALJRAD X                                                             
068400                                                                          
068500         MOVE W21611-IDUSER-GODK(1)                                       
068600                                      TO UT-IDUSER-GODK(1:8)              
068700         MOVE W21611-IDUSER-GODK(2)                                       
068800                                      TO UT-IDUSER-GODK(15:8)             
068900         MOVE W21611-IDUSER-GODK(3)                                       
069000                                      TO UT-IDUSER-GODK(29:8)             
069100         MOVE W21611-IDUSER-GODK(4)                                       
069200                                      TO UT-IDUSER-GODK(43:8)             
069300         MOVE W21611-IDUSER-GODK(5)                                       
069400                                      TO UT-IDUSER-GODK(57:8)             
069500                                                                          
069600     EJECT                                                                
069700                                                                          
069800                                                                          
069900* DETALJRAD Z                                                             
070000                                                                          
070100         MOVE W21611-BEANST-GODK(1)                                       
070200                                      TO UT-BEANST-GODK(1:13)             
070300         MOVE W21611-BEANST-GODK(2)                                       
070400                                      TO UT-BEANST-GODK(15:13)            
070500         MOVE W21611-BEANST-GODK(3)                                       
070600                                      TO UT-BEANST-GODK(29:13)            
070700         MOVE W21611-BEANST-GODK(4)                                       
070800                                      TO UT-BEANST-GODK(43:13)            
070900         MOVE W21611-BEANST-GODK(5)                                       
071000                                      TO UT-BEANST-GODK(57:13)            
071100                                                                          
071200     EJECT                                                                
071300                                                                          
071400* DETALJRAD D                                                             
071500                                                                          
071600         MOVE W21611-TIDATETIME(1)                                        
071700                                      TO UT-TIDATE     (1:8)              
071800         MOVE W21611-TIDATETIME(2)                                        
071900                                      TO UT-TIDATE     (15:8)             
072000         MOVE W21611-TIDATETIME(3)                                        
072100                                      TO UT-TIDATE     (29:8)             
072200         MOVE W21611-TIDATETIME(4)                                        
072300                                      TO UT-TIDATE     (43:8)             
072400         MOVE W21611-TIDATETIME(5)                                        
072500                                      TO UT-TIDATE     (57:8)             
072600     .                                                                    
072700     EJECT                                                                
072800                                                                          
072900 CB-ANDRA-SIDAN SECTION.                                                  
073000                                                                          
073100* DETALJRAD 1                                                             
073200                                                                          
073300     MOVE 'S2'          TO UT-SIDTYP                                      
073400     IF DCS-IDDC NOT = W21611-IDDC                                        
073500        MOVE W21611-IDDC    TO W-IDDC-B6                                  
073600        PERFORM IMS-GU-WDB601                                             
073700     END-IF                                                               
073800                                                                          
073900     IF W21611-IDDC NUMERIC                                               
074000        MOVE W21611-IDDC    TO WS-IDANSK-NUM                              
074100     ELSE                                                                 
074200        MOVE W21611-IDDC    TO WS-IDANSK-POS2-3                           
074300     END-IF                                                               
074400     MOVE W21611-IDUSER     TO UT-IDUSER                                  
074500     MOVE W21611-IDPERSON   TO WS-IDPERSON                                
074600     MOVE W21611-BEANST     TO UT-BEANST                                  
074700     MOVE W21611-IDAVD      TO UT-IDANSK-AVD                              
074800     COMPUTE WS-DASKROT9-BEORD = 99999999 - W21611-DASKROT9-BEORD         
074900     MOVE WS-DASKROT9-BEORD(3:6) TO UT-DDATUM                             
075000*    MOVE DAGENS-DATUM      TO UT-DDATUM                                  
075100                                                                          
075200                                                                          
075300* DETALJRAD 2                                                             
075400                                                                          
075500     MOVE W21611-IDDC       TO UT-IDDC                                    
075600     MOVE W21611-IDKONTO    TO UT-SKROTKONTO                              
075700     MOVE W21611-IDANALYS   TO UT-IDANALYS                                
075800     MOVE W21611-IDKST      TO UT-KST                                     
075900     MOVE W21611-IDDISTR    TO UT-IDDISTR                                 
076000                                                                          
076100*    MOVE W21611-IDDC    TO IDDC-WS                                       
076200     MOVE W21611-IDKUNDNR TO UT-IDKUNDNR                                  
076300                                                                          
076400                                                                          
076500* DETALJRAD 3                                                             
076600                                                                          
076700     MOVE W21611-IDARTNR    TO UT-IDARTNR                                 
076800     MOVE W21611-BEART      TO UT-BEART                                   
076900     EJECT                                                                
077000                                                                          
077100     MOVE W21611-TEMEMO(1)      TO UT-TEMEMO1                             
077200     MOVE W21611-TEMEMO(2)      TO UT-TEMEMO2                             
077300     MOVE W21611-TEMEMO(3)      TO UT-TEMEMO3                             
077400     MOVE W21611-TEMEMO(4)      TO UT-TEMEMO4                             
077500     MOVE W21611-TEMEMO(5)      TO UT-TEMEMO5                             
077600     MOVE W21611-TEMEMO(6)      TO UT-TEMEMO6                             
077700     MOVE W21611-TEMEMO(7)      TO UT-TEMEMO7                             
077800     MOVE W21611-TEMEMO(8)      TO UT-TEMEMO8                             
077900     MOVE W21611-TEMEMO(9)      TO UT-TEMEMO9                             
078000     MOVE W21611-TEMEMO(10)     TO UT-TEMEMO10                            
078100     MOVE W21611-TEMEMO(11)     TO UT-TEMEMO11                            
078200                                                                          
078300* DETALJRAD X                                                             
078400                                                                          
078500     MOVE W21611-IDUSER-GODK(6)                                           
078600                                  TO UT-IDUSER-GODK2(1:8)                 
078700     MOVE W21611-IDUSER-GODK(7)                                           
078800                                  TO UT-IDUSER-GODK2(15:8)                
078900     MOVE W21611-IDUSER-GODK(8)                                           
079000                                  TO UT-IDUSER-GODK2(29:8)                
079100     MOVE W21611-IDUSER-GODK(9)                                           
079200                                  TO UT-IDUSER-GODK2(43:8)                
079300     MOVE W21611-IDUSER-GODK(10)                                          
079400                                  TO UT-IDUSER-GODK2(57:8)                
079500                                                                          
079600 EJECT                                                                    
079700                                                                          
079800                                                                          
079900* DETALJRAD Z                                                             
080000                                                                          
080100     MOVE W21611-BEANST-GODK(6)                                           
080200                                  TO UT-BEANST-GODK2(1:13)                
080300     MOVE W21611-BEANST-GODK(7)                                           
080400                                  TO UT-BEANST-GODK2(15:13)               
080500     MOVE W21611-BEANST-GODK(8)                                           
080600                                  TO UT-BEANST-GODK2(29:13)               
080700     MOVE W21611-BEANST-GODK(9)                                           
080800                                  TO UT-BEANST-GODK2(43:13)               
080900     MOVE W21611-BEANST-GODK(10)                                          
081000                                  TO UT-BEANST-GODK2(57:13)               
081100                                                                          
081200 EJECT                                                                    
081300                                                                          
081400* DETALJRAD D                                                             
081500                                                                          
081600     MOVE W21611-TIDATETIME(6)                                            
081700                                  TO UT-TIDATE2(1:8)                      
081800     MOVE W21611-TIDATETIME(7)                                            
081900                                  TO UT-TIDATE2(15:8)                     
082000     MOVE W21611-TIDATETIME(8)                                            
082100                                  TO UT-TIDATE2(29:8)                     
082200     MOVE W21611-TIDATETIME(9)                                            
082300                                  TO UT-TIDATE2(43:8)                     
082400     MOVE W21611-TIDATETIME(10)                                           
082500                                  TO UT-TIDATE2(57:8)                     
082600     .                                                                    
082700     EJECT                                                                
082800                                                                          
082900                                                                          
083000 Z-AVSLUTNING SECTION.                                                    
083100                                                                          
083200     CLOSE W21611                                                         
083300           W21621                                                         
083400           W21622                                                         
083500                                                                          
083600     MOVE 'S' TO POSTSUM-OPKOD                                            
083700     CALL POSTSUM USING POSTSUM-PARM                                      
083800     .                                                                    
083900                                                                          
084000                                                                          
084100 S11-LAS-W21611 SECTION.                                                  
084200                                                                          
084300     READ W21611 INTO W21611-AREA                                         
084400          AT END MOVE JA TO W21611-EOF                                    
084500     END-READ                                                             
084600     SKIP2                                                                
084700     IF  W21611-EOF = NEJ                                                 
084800       MOVE 'W21611' TO POSTSUM-FDNAMN                                    
084900       MOVE 'W21620D1' TO POSTSUM-DDNAMN2                                 
085000       CALL POSTSUM USING POSTSUM-PARM                                    
085100     END-IF                                                               
085200     .                                                                    
085300     EJECT                                                                
085400                                                                          
085500                                                                          
085600 S20-SKRIV-DETRADER SECTION.                                              
085700     SKIP3                                                                
085800     WRITE  W21621-RAD               FROM DETRAD1 AFTER PAGE              
085900     MOVE SPACE                      TO W21621-RAD                        
086000     WRITE  W21621-RAD               FROM DETRAD2 AFTER 1                 
086100     MOVE SPACE                      TO W21621-RAD                        
086200     WRITE  W21621-RAD               FROM DETRAD3 AFTER 1                 
086300     MOVE SPACE                      TO W21621-RAD                        
086400     IF UT-SIDTYP = 'S1'                                                  
086500       WRITE W21621-RAD              FROM DETRAD4 AFTER 1                 
086600       MOVE SPACE                    TO W21621-RAD                        
086700       WRITE W21621-RAD              FROM DETRADB AFTER 1                 
086800       MOVE SPACE                    TO W21621-RAD                        
086900       WRITE W21621-RAD              FROM DETRAD5 AFTER 1                 
087000       MOVE SPACE                    TO W21621-RAD                        
087100       WRITE W21621-RAD              FROM DETRAD6 AFTER 1                 
087200       MOVE SPACE                    TO W21621-RAD                        
087300       WRITE W21621-RAD              FROM DETRAD7 AFTER 1                 
087400       MOVE SPACE                    TO W21621-RAD                        
087500       WRITE W21621-RAD              FROM DETRAD8 AFTER 1                 
087600       MOVE SPACE                    TO W21621-RAD                        
087700       WRITE W21621-RAD              FROM DETRAD9 AFTER 1                 
087800       MOVE SPACE                    TO W21621-RAD                        
087900       WRITE W21621-RAD              FROM DETRADN AFTER 1                 
088000       MOVE SPACE                    TO W21621-RAD                        
088100       WRITE W21621-RAD              FROM DETRADX AFTER 1                 
088200       MOVE SPACE                    TO W21621-RAD                        
088300       WRITE W21621-RAD              FROM DETRADZ AFTER 1                 
088400       MOVE SPACE                    TO W21621-RAD                        
088500       WRITE W21621-RAD              FROM DETRADD AFTER 1                 
088600       MOVE SPACE                    TO W21621-RAD                        
088700     END-IF                                                               
088800     IF UT-SIDTYP = 'S2'                                                  
088900       WRITE W21621-RAD              FROM DETRADTEXT1 AFTER 1             
089000       MOVE SPACE                    TO W21621-RAD                        
089100       WRITE W21621-RAD              FROM DETRADTEXT2 AFTER 1             
089200       MOVE SPACE                    TO W21621-RAD                        
089300       WRITE W21621-RAD              FROM DETRADTEXT3 AFTER 1             
089400       MOVE SPACE                    TO W21621-RAD                        
089500       WRITE W21621-RAD              FROM DETRADTEXT4 AFTER 1             
089600       MOVE SPACE                    TO W21621-RAD                        
089700       WRITE W21621-RAD              FROM DETRADTEXT5 AFTER 1             
089800       MOVE SPACE                    TO W21621-RAD                        
089900       WRITE W21621-RAD              FROM DETRADTEXT6 AFTER 1             
090000       MOVE SPACE                    TO W21621-RAD                        
090100       WRITE W21621-RAD              FROM DETRADTEXT7 AFTER 1             
090200       MOVE SPACE                    TO W21621-RAD                        
090300       WRITE W21621-RAD              FROM DETRADTEXT8 AFTER 1             
090400       MOVE SPACE                    TO W21621-RAD                        
090500       WRITE W21621-RAD              FROM DETRADTEXT9 AFTER 1             
090600       MOVE SPACE                    TO W21621-RAD                        
090700       WRITE W21621-RAD              FROM DETRADTEXT10 AFTER 1            
090800       MOVE SPACE                    TO W21621-RAD                        
090900       WRITE W21621-RAD              FROM DETRADTEXT11 AFTER 1            
091000       MOVE SPACE                    TO W21621-RAD                        
091100       WRITE W21621-RAD              FROM DETRADX2 AFTER 1                
091200       MOVE SPACE                    TO W21621-RAD                        
091300       WRITE W21621-RAD              FROM DETRADZ2 AFTER 1                
091400       MOVE SPACE                    TO W21621-RAD                        
091500       WRITE W21621-RAD              FROM DETRADD2 AFTER 1                
091600       MOVE SPACE                    TO W21621-RAD                        
091700     END-IF                                                               
091800     .                                                                    
091900     EJECT                                                                
092000                                                                          
092100                                                                          
092200 S22-SKRIV-DETRADER SECTION.                                              
092300     SKIP3                                                                
092400     WRITE  W21622-RAD               FROM DETRAD1 AFTER PAGE              
092500     MOVE SPACE                      TO W21622-RAD                        
092600     WRITE  W21622-RAD               FROM DETRAD2 AFTER 1                 
092700     MOVE SPACE                      TO W21622-RAD                        
092800     WRITE  W21622-RAD               FROM DETRAD3 AFTER 1                 
092900     MOVE SPACE                      TO W21622-RAD                        
093000     IF UT-SIDTYP = 'S1'                                                  
093100       WRITE W21622-RAD              FROM DETRAD4 AFTER 1                 
093200       MOVE SPACE                    TO W21622-RAD                        
093300       WRITE W21622-RAD              FROM DETRADB AFTER 1                 
093400       MOVE SPACE                    TO W21622-RAD                        
093500       WRITE W21622-RAD              FROM DETRAD5 AFTER 1                 
093600       MOVE SPACE                    TO W21622-RAD                        
093700       WRITE W21622-RAD              FROM DETRAD6 AFTER 1                 
093800       MOVE SPACE                    TO W21622-RAD                        
093900       WRITE W21622-RAD              FROM DETRAD7 AFTER 1                 
094000       MOVE SPACE                    TO W21622-RAD                        
094100       WRITE W21622-RAD              FROM DETRAD8 AFTER 1                 
094200       MOVE SPACE                    TO W21622-RAD                        
094300       WRITE W21622-RAD              FROM DETRAD9 AFTER 1                 
094400       MOVE SPACE                    TO W21622-RAD                        
094500       WRITE W21622-RAD              FROM DETRADN AFTER 1                 
094600       MOVE SPACE                    TO W21622-RAD                        
094700       WRITE W21622-RAD              FROM DETRADX AFTER 1                 
094800       MOVE SPACE                    TO W21622-RAD                        
094900       WRITE W21622-RAD              FROM DETRADZ AFTER 1                 
095000       MOVE SPACE                    TO W21622-RAD                        
095100       WRITE W21622-RAD              FROM DETRADD AFTER 1                 
095200       MOVE SPACE                    TO W21622-RAD                        
095300     END-IF                                                               
095400     IF UT-SIDTYP = 'S2'                                                  
095500       WRITE W21622-RAD              FROM DETRADTEXT1 AFTER 1             
095600       MOVE SPACE                    TO W21622-RAD                        
095700       WRITE W21622-RAD              FROM DETRADTEXT2 AFTER 1             
095800       MOVE SPACE                    TO W21622-RAD                        
095900       WRITE W21622-RAD              FROM DETRADTEXT3 AFTER 1             
096000       MOVE SPACE                    TO W21622-RAD                        
096100       WRITE W21622-RAD              FROM DETRADTEXT4 AFTER 1             
096200       MOVE SPACE                    TO W21622-RAD                        
096300       WRITE W21622-RAD              FROM DETRADTEXT5 AFTER 1             
096400       MOVE SPACE                    TO W21622-RAD                        
096500       WRITE W21622-RAD              FROM DETRADTEXT6 AFTER 1             
096600       MOVE SPACE                    TO W21622-RAD                        
096700       WRITE W21622-RAD              FROM DETRADTEXT7 AFTER 1             
096800       MOVE SPACE                    TO W21622-RAD                        
096900       WRITE W21622-RAD              FROM DETRADTEXT8 AFTER 1             
097000       MOVE SPACE                    TO W21622-RAD                        
097100       WRITE W21622-RAD              FROM DETRADTEXT9 AFTER 1             
097200       MOVE SPACE                    TO W21622-RAD                        
097300       WRITE W21622-RAD              FROM DETRADTEXT10 AFTER 1            
097400       MOVE SPACE                    TO W21622-RAD                        
097500       WRITE W21622-RAD              FROM DETRADTEXT11 AFTER 1            
097600       MOVE SPACE                    TO W21622-RAD                        
097700       WRITE W21622-RAD              FROM DETRADX2 AFTER 1                
097800       MOVE SPACE                    TO W21622-RAD                        
097900       WRITE W21622-RAD              FROM DETRADZ2 AFTER 1                
098000       MOVE SPACE                    TO W21622-RAD                        
098100       WRITE W21622-RAD              FROM DETRADD2 AFTER 1                
098200       MOVE SPACE                    TO W21622-RAD                        
098300     END-IF                                                               
098400     .                                                                    
098500 S90-SKRIV-DAP-SCRAP-B SECTION.                                           
098600                                                                          
098700     INITIALIZE WS-HEADER-RECS                                            
098800                W21622-RAD                                                
098900                                                                          
099000     MOVE 'SCRAP'             TO WS-HDR-1-IDOUTTYPE                       
099100                                                                          
099200     STRING WS-DAP WS-HDR-1-IDOUTTYPE DELIMITED BY SIZE                   
099300                              INTO W21622-RAD                             
099400                                                                          
099500     WRITE W21622-RAD                                                     
099600                                                                          
099700     MOVE 'B'                 TO WS-HDR-2-IDOUTREC                        
099800     MOVE SPACES              TO W21622-RAD                               
099900                                                                          
100000     STRING WS-DAP WS-HDR-2-IDOUTREC DELIMITED BY SIZE                    
100100                              INTO W21622-RAD                             
100200                                                                          
100300     WRITE W21622-RAD                                                     
100400     .                                                                    
100500                                                                          
100600 S91-SKRIV-META-SCRAP-B SECTION.                                          
100700                                                                          
100800     MOVE SPACES TO W21622-RAD                                            
100900     STRING WS-META 'DISTRICT='  UT-IDDISTR                               
101000     DELIMITED BY SIZE INTO W21622-RAD                                    
101100     WRITE W21622-RAD                                                     
101200                                                                          
101300     MOVE SPACES TO W21622-RAD                                            
101400     STRING WS-META 'CUSTOMER_NUMBER='  UT-IDKUNDNR                       
101410     DELIMITED BY SIZE INTO W21622-RAD                                    
101500     WRITE W21622-RAD                                                     
101600                                                                          
101700     MOVE SPACES TO W21622-RAD                                            
101800     STRING WS-META 'DC='  UT-IDDC                                        
101810     DELIMITED BY SIZE INTO W21622-RAD                                    
101900     WRITE W21622-RAD                                                     
102000                                                                          
102100     MOVE SPACES TO W21622-RAD                                            
102200     STRING WS-META 'SCRAPPING_DECISION_DATE='  UT-DDATUM                 
102210     DELIMITED BY SIZE INTO W21622-RAD                                    
102300     WRITE W21622-RAD                                                     
102400                                                                          
102500     MOVE SPACES TO W21622-RAD                                            
102600     STRING WS-META 'PART_NUMBER='  UT-IDARTNR                            
102610     DELIMITED BY SIZE INTO W21622-RAD                                    
102700     WRITE W21622-RAD                                                     
102800                                                                          
102900     MOVE SPACES TO W21622-RAD                                            
103000     STRING WS-META 'USER_ID='  UT-IDUSER                                 
103010     DELIMITED BY SIZE INTO W21622-RAD                                    
103100     WRITE W21622-RAD                                                     
103200                                                                          
103300     MOVE SPACES TO W21622-RAD                                            
103400     STRING WS-META 'EMPLOYED_NAME=' UT-BEANST                            
103410     DELIMITED BY SIZE INTO W21622-RAD                                    
103500     WRITE W21622-RAD                                                     
103200                                                                          
           MOVE SPACES TO W21622-RAD                                            
           STRING ' ¤METAFILE_NAME='                                            
                  DELIMITED BY SIZE                                             
                  'SCRAP'                                                       
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  FUNCTION TRIM (UT-IDARTNR)                                    
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  UT-IDDC                                                       
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  WS-TIMESTAMP                                                  
                  DELIMITED BY SIZE INTO W21622-RAD                             
                                                                                
103500     WRITE W21622-RAD                                                     
103600     .                                                                    
103700                                                                          
103800 S92-SKRIV-DAP-SCRAP-A SECTION.                                           
103900                                                                          
104000     INITIALIZE WS-HEADER-RECS                                            
104100                      W21621-RAD                                          
104200                                                                          
104300     MOVE 'SCRAP'             TO WS-HDR-1-IDOUTTYPE                       
104400                                                                          
104500     STRING WS-DAP WS-HDR-1-IDOUTTYPE DELIMITED BY SIZE                   
104600                              INTO W21621-RAD                             
104700                                                                          
104800     WRITE W21621-RAD                                                     
104900                                                                          
105000     MOVE 'A'                 TO WS-HDR-2-IDOUTREC                        
105100     MOVE SPACES              TO W21621-RAD                               
105200                                                                          
105300     STRING WS-DAP WS-HDR-2-IDOUTREC DELIMITED BY SIZE                    
105400                              INTO W21621-RAD                             
105500                                                                          
105600     WRITE W21621-RAD                                                     
105700     .                                                                    
105800                                                                          
105900 S93-SKRIV-META-SCRAP-A SECTION.                                          
106000                                                                          
106100     MOVE SPACES TO W21621-RAD                                            
106200     STRING WS-META 'DISTRICT='   UT-IDDISTR                              
106210     DELIMITED BY SIZE INTO W21621-RAD                                    
106300     WRITE W21621-RAD                                                     
106400                                                                          
106500     MOVE SPACES TO W21621-RAD                                            
106600     STRING WS-META 'CUSTOMER_NUMBER='   UT-IDKUNDNR                      
106610     DELIMITED BY SIZE INTO W21621-RAD                                    
106700     WRITE W21621-RAD                                                     
106800                                                                          
106900     MOVE SPACES TO W21621-RAD                                            
107000     STRING WS-META 'DC='  UT-IDDC                                        
107010     DELIMITED BY SIZE INTO W21621-RAD                                    
107100     WRITE W21621-RAD                                                     
107200                                                                          
107300     MOVE SPACES TO W21621-RAD                                            
107400     STRING WS-META 'SCRAPPING_DECISION_DATE='  UT-DDATUM                 
107410     DELIMITED BY SIZE INTO W21621-RAD                                    
107500     WRITE W21621-RAD                                                     
107600                                                                          
107700     MOVE SPACES TO W21621-RAD                                            
107800     STRING WS-META 'PART_NUMBER='     UT-IDARTNR                         
107810     DELIMITED BY SIZE INTO W21621-RAD                                    
107900     WRITE W21621-RAD                                                     
108000                                                                          
108100     MOVE SPACES TO W21621-RAD                                            
108200     STRING WS-META 'USER_ID='   UT-IDUSER                                
108210     DELIMITED BY SIZE INTO W21621-RAD                                    
108300     WRITE W21621-RAD                                                     
108400                                                                          
108500     MOVE SPACES TO W21621-RAD                                            
108600     STRING WS-META 'EMPLOYED_NAME=' UT-BEANST                            
108610     DELIMITED BY SIZE INTO W21621-RAD                                    
108700     WRITE W21621-RAD                                                     
                                                                                
           MOVE SPACES TO W21621-RAD                                            
           STRING ' ¤METAFILE_NAME='                                            
                  DELIMITED BY SIZE                                             
                  'SCRAP'                                                       
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  FUNCTION TRIM (UT-IDARTNR)                                    
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  UT-IDDC                                                       
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  WS-TIMESTAMP                                                  
                  DELIMITED BY SIZE INTO W21621-RAD                             
                                                                                
103500     WRITE W21621-RAD                                                     
108800     .                                                                    
108900                                                                          
109000 IMS-GU-WDB601    SECTION.                                                
109100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
109200          DELIMITED BY SIZE INTO SSA1                                     
109300     MOVE '  GE' TO GODK-STATUSKODER                                      
109400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
109500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
109600     PERFORM IMS-STATUSKONTROLL                                           
109700     IF SEGMENT-SAKNAS                                                    
109800        MOVE SPACE TO DCS-KDDC                                            
109900     END-IF                                                               
110000     .                                                                    
110100                                                                          
110200 IMS-STATUSKONTROLL SECTION.                                              
110300                                                                          
110400     SET STATUS-IX TO 1                                                   
110500     SEARCH GODK-STATUS                                                   
110600       AT END                                                             
110700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
110800         DELIMITED BY SIZE INTO FELTEXT                                   
110900         CALL FELLOG                                                      
111000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
111100         CONTINUE                                                         
111200     END-SEARCH                                                           
111300     .                                                                    
