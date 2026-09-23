000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5125100.                                                
000300 AUTHOR.         SARASWATHY.                                              
000400 DATE-WRITTEN.   APR 2017.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*                                                                         
000900*       -PROGRAM READS WDH5                                               
001000*       -CONVERTS CNY TO SEK                                              
001010*       -CONVERTS USD TO SEK                                              
001020*       -CONVERTS KRW TO SEK                                              
001100*    ABENDKODER:                                                          
001200*        U0016 -  . . . .                                                 
001300*        U1000 -  . . . .                                                 
001400*                                                                         
001500                                                                          
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100*          --- INPUT FROM W51250                                          
002200     SELECT W51250                     ASSIGN TO W51251D1.                
002210*          --- PRICE FROM WDK6                                            
002220     SELECT W01160                     ASSIGN TO W51251D2.                
002300                                                                          
002400*          --- OUTPUT WITH SWEDISH AND USD CURRENCY                       
002500     SELECT W51251                     ASSIGN TO W51251D3.                
002510*          --- CNY CONVERTED TO SEK                                       
002520     SELECT W51253                     ASSIGN TO W51251D4.                
002600                                                                          
002700     EJECT                                                                
002800                                                                          
002900 DATA DIVISION.                                                           
003000                                                                          
003100 FILE SECTION.                                                            
003200 FD  W51250                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500 01  IN-POST.                                                             
003600*    03  -COPY W51250    -PRE  IN-  -L.                                   
003700                                                                          
003710 FD  W01160                                                               
003720     RECORDING       F                                                    
003730     BLOCK CONTAINS  0.                                                   
003740 01  IN1-POST.                                                            
003750*    03  -COPY W01160    -PRE  IN1-  -L.                                  
003760                                                                          
003800 FD  W51251                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100*01  POST   -COPY W51251 -PRE  UT1-  -L.                                  
004200                                                                          
004210 FD  W51253                                                               
004220     RECORDING       V                                                    
004230     BLOCK CONTAINS  0.                                                   
004231*01  POST   -COPY W51253 -PRE  UT2-  -L.                                  
004250                                                                          
004300 WORKING-STORAGE SECTION.                                                 
004400 77  IDPGM                        PIC X(8)    VALUE 'W5125100'.           
004500 77  JA                           PIC X       VALUE 'J'.                  
004600 77  NEJ                          PIC X       VALUE 'N'.                  
004700 77  FELTEXT                      PIC X(80).                              
004710 77  WS-SEMICOLON                 PIC X(1)    VALUE ';'.                  
004720 77  WS-COUNT                     PIC 9(1)    VALUE 1.                    
004721 77  WS-IDARTNR-SPAR              PIC S9(9)   COMP-3 VALUE 0.             
004730 77  WS-PRARTSTD                  PIC S9(7)V9(2) COMP-3 VALUE 0.          
004740 77  W-DATE-AAMM                  PIC 9(4)       VALUE ZERO.              
004750 77  WS-KDVALISO-HUV              PIC X(3)       VALUE 'SEK'.             
004800 77  WS-ACTUAL-DATE               PIC 9(16)   VALUE 0.                    
004900 01  WS-ACTUAL-DATE-X             PIC X(16)   VALUE SPACES.               
004901 01  WS-ACTUAL-DATE-UT.                                                   
004910     03 W-DATE.                                                           
004920       05 W-YYYY                  PIC 9(4).                               
004930       05 W-MM                    PIC 9(2).                               
004940       05 W-DD                    PIC 9(2).                               
004950     03 W-TIME                    PIC 9(8).                               
004960 01  WS-HEADER-AREA.                                                      
004970     03 WS-IDARTNR          PIC X(7)    VALUE 'PART NO'.                  
004980     03 WS-SEMICOLON1       PIC X       VALUE ';'.                        
004990     03 WS-IDDC             PIC X(2)    VALUE 'DC'.                       
004991     03 WS-SEMICOLON2       PIC X       VALUE ';'.                        
004994     03 WS-DATE             PIC X(4)    VALUE 'DATE'.                     
004995     03 WS-SEMICOLON3       PIC X       VALUE ';'.                        
004996     03 WS-KVAVIS           PIC X(8)    VALUE 'QUANTITY'.                 
004997     03 WS-SEMICOLON4       PIC X       VALUE ';'.                        
004998     03 WS-PRARTNTO-NET     PIC X(9)    VALUE 'NET PRICE'.                
004999     03 WS-SEMICOLON5       PIC X       VALUE ';'.                        
005002     03 WS-KDVALISO         PIC X(3)    VALUE  'CUR'.                     
005003     03 WS-SEMICOLON6       PIC X       VALUE ';'.                        
005004     03 WS-IDFAKT           PIC X(10)   VALUE  'INVOICE NO'.              
005005     03 WS-SEMICOLON7       PIC X       VALUE ';'.                        
005006     03 WS-PRKURS           PIC X(7)    VALUE  'EX RATE'.                 
005007     03 WS-SEMICOLON8       PIC X       VALUE ';'.                        
005008     03 WS-PRARTNTO-NETSEK  PIC X(10)   VALUE  'NET PR SEK'.              
005009     03 WS-SEMICOLON9       PIC X       VALUE ';'.                        
005010     03 WS-PRARTNTO-TOTSEK  PIC X(13)   VALUE  'TOT NET PRICE'.           
005011     03 WS-SEMICOLON10      PIC X       VALUE ';'.                        
005012     03 WS-PRARTSTD-SEK     PIC X(9)    VALUE 'STD PRICE'.                
005013     03 WS-SEMICOLON5       PIC X       VALUE ';'.                        
005014     03 WS-PRARTSTD-TOTSEK  PIC X(13)   VALUE  'TOT STD PRICE'.           
005015     03 WS-SEMICOLON11      PIC X       VALUE ';'.                        
005020     03 WS-DIST             PIC X(4)    VALUE 'DIST'.                     
005040 77  W51250-EOF-SW                PIC X       VALUE 'N'.                  
005100     88  END-OF-W51250                        VALUE 'J'.                  
005200                                                                          
005210 77  W01160-EOF-SW                PIC X       VALUE 'N'.                  
005220     88  END-OF-W01160                        VALUE 'J'.                  
005230                                                                          
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
005600     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
005700     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
005710     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
005800                                                                          
006900*    --- PARAMETRAR TILL ABEND                                            
007000 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
007100 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
007200 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
007300     EJECT                                                                
007400                                                                          
007500*    --- PARAMETRAR TILL POSTSUM                                          
007600*01  -COPY W0005   -PRE  POSTSUM-                                         
007700     EJECT                                                                
007800                                                                          
007810*01  -COPY W510CURR                                                       
007820     EJECT                                                                
007830                                                                          
007900 01  IN-AREA-START               PIC X(24)   VALUE                        
008000                                 'IN-AREA-START  '.                       
008100                                                                          
008200*01  AREA -COPY W51250     -PRE IN-                                       
008300     EJECT                                                                
008310 01  IN-AREA1-START              PIC X(24)   VALUE                        
008320                                 'IN-AREA1-START  '.                      
008330                                                                          
008340*01  AREA -COPY W01160     -PRE IN1-                                      
008350     EJECT                                                                
008400                                                                          
008500 01  UT-AREA-START               PIC X(24)   VALUE                        
008600                                 'UT1-AREA-START  '.                      
008700                                                                          
008800*01  AREA -COPY W51251     -PRE UT1-                                      
008900     EJECT                                                                
008901                                                                          
008902 01  UT2-AREA-START               PIC X(24)   VALUE                       
008903                                 'UT2-AREA-START  '.                      
008904                                                                          
008905*01  AREA -COPY W51253     -PRE UT2-                                      
008906     EJECT                                                                
008907                                                                          
009000                                                                          
011800 LINKAGE SECTION.                                                         
011900*01  -COPY W0008  -PRE WDG2-                                              
012000     05  FILLER                  PIC X.                                   
012100                                                                          
012200     EJECT                                                                
012300                                                                          
012400 PROCEDURE DIVISION  USING WDG2-PCB.                                      
012500                                                                          
012600 MAIN SECTION.                                                            
012700     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
012800                                                                          
012900     PERFORM A-INIT                                                       
013000                                                                          
013100     PERFORM S01-READ-W51250                                              
013110     PERFORM S02-READ-W01160                                              
013200     PERFORM UNTIL END-OF-W51250                                          
013210       PERFORM B-READ-W01160-STDPRICE                                     
013300       PERFORM C-CONVERT-CURRENCY-TO-SEK                                  
013400       PERFORM S01-READ-W51250                                            
013500     END-PERFORM                                                          
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100                                                                          
014200 A-INIT SECTION.                                                          
014300     OPEN INPUT  W51250                                                   
014310                 W01160                                                   
014400     OPEN OUTPUT W51251                                                   
014410                 W51253                                                   
014420     MOVE ZERO           TO WS-IDARTNR-SPAR                               
014500     .                                                                    
014600     EJECT                                                                
014610 B-READ-W01160-STDPRICE SECTION.                                          
014611                                                                          
014612     IF IN-IDARTNR NOT = WS-IDARTNR-SPAR                                  
014613        MOVE ZERO               TO WS-PRARTSTD                            
014614        MOVE IN-IDARTNR         TO WS-IDARTNR-SPAR                        
014630        PERFORM UNTIL  END-OF-W01160                                      
014640                   OR  IN1-CLAG-IDARTNR > IN-IDARTNR                      
014650             IF  IN1-CLAG-IDARTNR = IN-IDARTNR                            
014660                 MOVE IN1-CLAG-PRARTSTD TO WS-PRARTSTD                    
014670             END-IF                                                       
014680             PERFORM S02-READ-W01160                                      
014690        END-PERFORM                                                       
014691     END-IF                                                               
014692     .                                                                    
014693     EJECT                                                                
014700                                                                          
014800 C-CONVERT-CURRENCY-TO-SEK SECTION.                                       
014900     COMPUTE WS-ACTUAL-DATE    = 9999999999999999                         
015000                                - IN-DAINLEV                              
015100     MOVE WS-ACTUAL-DATE          TO WS-ACTUAL-DATE-X                     
015110                                     WS-ACTUAL-DATE-UT                    
015200     MOVE WS-ACTUAL-DATE-X(3:2)   TO W-DATE-AAMM(1:2)                     
015300     MOVE WS-ACTUAL-DATE-X(5:2)   TO W-DATE-AAMM(3:2)                     
015400     MOVE W-DATE-AAMM             TO CURR-TIAAMM                          
015500     MOVE WS-KDVALISO-HUV         TO CURR-KDVALISO-HUV                    
015600     MOVE 'M'                     TO CURR-KDVALTYP                        
016200**** VI SKAPAR INTE INTERNAL TRANSFER                                     
016310       IF IN-IDDISTR = 9111                                               
016320       OR IN-IDDISTR = 9161                                               
016330       OR IN-IDDISTR = 9162                                               
016500           MOVE 'CNY'               TO CURR-KDVALISO-ROW                  
016510                                       UT1-KDVALISO                       
016520                                       UT2-KDVALISO                       
016530                                                                          
016600           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
016700           IF CURR-KDSVAR = ' '                                           
016800             COMPUTE UT2-PRARTNTO-NSEK ROUNDED =                          
016900                   IN-PRARTNTO  * CURR-PRKURS-NEW                         
016910             COMPUTE UT2-SUNTO-TOT     ROUNDED =                          
016920                  (IN-PRARTNTO * IN-KVAVIS) * CURR-PRKURS-NEW             
016930             COMPUTE UT2-SUSTDTOT      ROUNDED =                          
016940                  (WS-PRARTSTD * IN-KVAVIS)                               
017000           END-IF                                                         
019310       END-IF                                                             
019320       IF  IN-IDDISTR = 9211                                              
019330       OR  IN-IDDISTR = 9261                                              
019331       OR  IN-IDDISTR = 9262                                              
019340           MOVE 'USD'               TO CURR-KDVALISO-ROW                  
019341                                       UT1-KDVALISO                       
019342                                       UT2-KDVALISO                       
019343                                                                          
019350           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
019360           IF CURR-KDSVAR = ' '                                           
019361             COMPUTE UT2-PRARTNTO-NSEK ROUNDED =                          
019362                   IN-PRARTNTO  * CURR-PRKURS-NEW                         
019363             COMPUTE UT2-SUNTO-TOT     ROUNDED =                          
019364                  (IN-PRARTNTO * IN-KVAVIS) * CURR-PRKURS-NEW             
019365             COMPUTE UT2-SUSTDTOT      ROUNDED =                          
019366                  (WS-PRARTSTD * IN-KVAVIS)                               
019390           END-IF                                                         
019414       END-IF                                                             
019415       IF  IN-IDDISTR = 9361                                              
019416       OR  IN-IDDISTR = 9362                                              
019418           MOVE 'KRW'               TO CURR-KDVALISO-ROW                  
019419                                       UT1-KDVALISO                       
019420                                       UT2-KDVALISO                       
019421                                                                          
019422           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
019423           IF CURR-KDSVAR = ' '                                           
019424             COMPUTE UT2-PRARTNTO-NSEK ROUNDED =                          
019425                   IN-PRARTNTO  * CURR-PRKURS-NEW                         
019426             COMPUTE UT2-SUNTO-TOT     ROUNDED =                          
019427                  (IN-PRARTNTO * IN-KVAVIS) * CURR-PRKURS-NEW             
019428             COMPUTE UT2-SUSTDTOT      ROUNDED =                          
019429                  (WS-PRARTSTD * IN-KVAVIS)                               
019430           END-IF                                                         
019431       END-IF                                                             
019432       MOVE IN-IDARTNR              TO UT1-IDARTNR                        
019440                                       UT2-IDARTNR                        
019500       MOVE IN-IDDC                 TO UT1-IDDC                           
019510                                       UT2-IDDC                           
019511       MOVE IN-PRARTNTO             TO UT2-PRARTNTO-NET                   
019512       MOVE WS-PRARTSTD             TO UT2-PRARTSTD-SEK                   
019520       MOVE IN-IDDISTR              TO UT2-IDDISTR                        
019600       MOVE IN-DAINLEV              TO UT1-DAINLEV                        
019610       MOVE W-DATE                  TO UT2-DATUM                          
019700       MOVE IN-KVAVIS               TO UT1-KVAVIS                         
019710                                       UT2-KVAVIS                         
019800       MOVE UT2-PRARTNTO-NET        TO UT1-SUNTO-TOT                      
019920       MOVE IN-IDFAKT               TO UT2-IDFAKT                         
019930       MOVE CURR-PRKURS-NEW         TO UT2-PRKURS                         
019940       MOVE WS-SEMICOLON            TO UT2-SEMICOLON1                     
019950                                       UT2-SEMICOLON2                     
019960                                       UT2-SEMICOLON3                     
019970                                       UT2-SEMICOLON4                     
019980                                       UT2-SEMICOLON5                     
019990                                       UT2-SEMICOLON6                     
019991                                       UT2-SEMICOLON7                     
019992                                       UT2-SEMICOLON8                     
019993                                       UT2-SEMICOLON9                     
019994                                       UT2-SEMICOLON10                    
019995                                       UT2-SEMICOLON11                    
019996                                       UT2-SEMICOLON12                    
020000       PERFORM S03-WRITE-W51251                                           
020010       PERFORM S03-WRITE-W51253                                           
020200     .                                                                    
020300     EJECT                                                                
020400                                                                          
020700 S01-READ-W51250  SECTION.                                                
020800     READ W51250 INTO IN-AREA                                             
020900     AT END                                                               
021000        SET END-OF-W51250 TO TRUE                                         
021100                                                                          
021200     NOT AT END                                                           
021300        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
021400        MOVE 'W51250'     TO POSTSUM-FDNAMN                               
021500        MOVE 'W51251D1'   TO POSTSUM-DDNAMN2                              
021600        CALL POSTSUM USING POSTSUM-PARM                                   
021700     END-READ                                                             
021800     .                                                                    
021900                                                                          
021910 S02-READ-W01160  SECTION.                                                
021920     READ W01160 INTO IN1-AREA                                            
021930     AT END                                                               
021940        SET END-OF-W01160 TO TRUE                                         
021950                                                                          
021960     NOT AT END                                                           
021970        MOVE 'IN1'        TO POSTSUM-TRANSTYP                             
021980        MOVE 'W01160'     TO POSTSUM-FDNAMN                               
021990        MOVE 'W51251D2'   TO POSTSUM-DDNAMN2                              
021991        CALL POSTSUM USING POSTSUM-PARM                                   
021992     END-READ                                                             
021993     .                                                                    
021994                                                                          
022000 S03-WRITE-W51251 SECTION.                                                
022100     WRITE UT1-POST              FROM UT1-AREA                            
022200                                                                          
022300     MOVE 'W51251'              TO POSTSUM-FDNAMN                         
022400     MOVE 'W51251D3'            TO POSTSUM-DDNAMN2                        
022500     CALL POSTSUM USING POSTSUM-PARM                                      
022600     .                                                                    
022601                                                                          
022610 S03-WRITE-W51253 SECTION.                                                
022611     IF WS-COUNT = 1                                                      
022612        WRITE UT2-POST           FROM WS-HEADER-AREA                      
022613        MOVE ZERO TO WS-COUNT                                             
022614     END-IF                                                               
022620     WRITE UT2-POST              FROM UT2-AREA                            
022630                                                                          
022640     MOVE 'W51253'              TO POSTSUM-FDNAMN                         
022650     MOVE 'W51251D4'            TO POSTSUM-DDNAMN2                        
022660     CALL POSTSUM USING POSTSUM-PARM                                      
022670     .                                                                    
022680                                                                          
