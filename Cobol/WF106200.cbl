000100 PROCESS DYNAM                                                            
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     WF106200.                                                
000600 AUTHOR.         ANDERS HENRIKSSON                                        
000700 DATE-WRITTEN.   JUN 2006.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*   PGM                                                                   
001100*   -SELECTS ROWS FROM YESTERDAYS INVOICING READING                       
001200*      T01DHEA                                                            
001300*      T01DLIN                                                            
001500                                                                          
001600*   -CREATES FILE CONTAINING                                              
001700*      DOCUMENTS  WITHOUT PAYMENT TERMS                                   
001800*      DOCUMENTS  WITHOUT DELIVERY TERMS                                  
001900*      DISCOUNTS  > 75%                                                   
002000*      NET PRICES < 0.11 SEK AND >  99000 SEK (LOCAL CURRENCY)            
002100*      NET VALUES < 0.11 SEK AND > 300000 SEK (LOCAL CURRENCY)            
002200*                                                                         
002300*   - SENDS DOCUMENT DATA RECORDS FOR VCCS TO                             
002400*      DISTRIBUTION & PRINT BY USING WZ01SEND                             
002500*                                                                         
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000 FILE-CONTROL.                                                            
003100 DATA DIVISION.                                                           
003200 FILE SECTION.                                                            
003300                                                                          
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                     PIC X(8)    VALUE 'WF106200'.              
003600                                                                          
003900 01  WS-CURRENT-DATE           PIC X(8)  VALUE SPACE.                     
004000 01  WS-KDVALISO               PIC X(3)  VALUE SPACE.                     
004100 01  WS-RUNDATUM-START         PIC X(8)  VALUE SPACE.                     
004110 01  WS-RUNDATUM-END           PIC X(8)  VALUE SPACE.                     
004200 01  WS-DASTADAT               PIC X(8)  VALUE SPACE.                     
004310 01  IX                             PIC S9(9)  VALUE ZERO BINARY.         
004400 77  WS-VALUE-MIN              PIC S9(11)V9(5) COMP-3 VALUE ZERO.         
004500 77  WS-VALUE-MAX              PIC S9(11)V9(5) COMP-3 VALUE ZERO.         
004610 01  WS-BETEXT-A                    PIC X(20)   VALUE SPACE.              
004620 01  WS-BETEXT-B                    PIC X(20)   VALUE SPACE.              
004630 01  WS-BETEXT-C                    PIC X(20)   VALUE SPACE.              
004640 01  WS-BETEXT-D                    PIC X(20)   VALUE SPACE.              
004650 01  WS-BETEXT-E                    PIC X(20)   VALUE SPACE.              
004660 01  WS-BETEXT-F                    PIC X(20)   VALUE SPACE.              
004670 01  WS-BETEXT-G                    PIC X(20)   VALUE SPACE.              
004671 01  WS-BETEXT-H                    PIC X(20)   VALUE SPACE.              
004680 01  WS-REARTRAB                    PIC S9(2)V9(2) COMP-3.                
004690 01  WS-PRARTNTO-MIN                PIC S9(7)V9(2) COMP-3.                
004691 01  WS-PRARTNTO-MAX                PIC S9(7)V9(2) COMP-3.                
004692 01  WS-SUNTO-MIN                   PIC S9(11)V9(2) COMP-3.               
004693 01  WS-SUNTO-MAX                   PIC S9(11)V9(2) COMP-3.               
004694 01  WS-FLSOFT                      PIC X(1)    VALUE SPACE.              
004695 01  WS-FLFREE                      PIC X(1)    VALUE SPACE.              
004700                                                                          
005000 77  WS-ADRESS-RETURN          PIC X(50)                                  
005100                           VALUE 'CARPARTS.BILLIT.DAPERROR'.              
005110 77  WS-ADRESS2                  PIC X(50)                                
005120                           VALUE 'CARPARTS.DAP.DISTRDOC'.                 
005200                                                                          
005300 77  WZ04-SEND-IDCOM           PIC S9(9)   COMP VALUE +0.                 
005400     EJECT                                                                
005500                                                                          
005510 01  ERROR-TEXT                PIC X(80)   VALUE SPACE.                   
005600 01  ERRTEXT.                                                             
005700     03  FILLER                PIC X(8)    VALUE 'ERRTEXT'.               
005800     03  ERRTEXT-STR           PIC X(72)   VALUE SPACE.                   
005900 01  KDRC-DISPLAY              PIC Z(5).                                  
006000     EJECT                                                                
006100                                                                          
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300     03  ABEND                 PIC X(8)    VALUE 'ABEND   '.              
006400     03  WZ01SEND              PIC X(8)    VALUE 'WZ01SEND'.              
006410     03  WZ20DATE              PIC X(8)    VALUE 'WZ20DATE'.              
006500     EJECT                                                                
006600                                                                          
007200*    --- PARAMETRAR TILL ABEND                                            
007300*                                                                         
007400 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
007500 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
007600     EJECT                                                                
007700                                                                          
007800*    --- AREOR FÖR KOMMUNIKATION                                          
007900 01  FILLER                    PIC X(16)   VALUE 'SEND-CONTROL'.          
008000 01  -COPY WZ01SEND                                                       
008100     EJECT                                                                
008110 01  FILLER                    PIC X(16)   VALUE 'DATE-CONTROL'.          
008120 01  -COPY WZ20DATE                                                       
008200     EJECT                                                                
008210 01  WZ20DAYS                  PIC X(8)    VALUE 'WZ20DAYS'.              
008230*    -COPY WZ20DAYS                                                       
008240     EJECT                                                                
008250                                                                          
008300 01  UT-AREA-START             PIC X(24)   VALUE 'UT-AREA-START'.         
008400 01  HDR-AREA.                                                            
008500*    03  -COPY WZ01REQU -PRE MAIL-                                        
008600*    03  -COPY WZ04HDR                                                    
008700     EJECT                                                                
008800                                                                          
008900 01  DOC-HEAD-AREA1.                                                      
009000*    03  -COPY WF029201                                                   
009100 01  DOC-HEAD-AREA2.                                                      
009110*    03  -COPY WF029202                                                   
009120     EJECT                                                                
009130                                                                          
009140 01  DOC-LINE-AREA1.                                                      
009150*    03  -COPY WF029203                                                   
009160 01  DOC-LINE-AREA2.                                                      
009170*    03  -COPY WF029204                                                   
009180     EJECT                                                                
009181                                                                          
009182 01  DOC-LINE-AREA3.                                                      
009183*    03  -COPY WF029205                                                   
009184 01  DOC-LINE-AREA4.                                                      
009185*    03  -COPY WF029206                                                   
009186     EJECT                                                                
009190                                                                          
009200*                                                                         
009300*        WORK-AREAS FOR DB2-SECTIONS                                      
009400*                                                                         
011800 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
011900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
012000*                        **** STATUS-CODE FROM DB2                        
012100                                                                          
012200 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
012300 01  DB2-WS.                                                              
012310   03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                    
012320       88  CURSOR-OK                       VALUE 000.                     
012330       88  LINES-FOUND                     VALUE 000.                     
012340       88  LINES-MISSING                   VALUE 100.                     
012350       88  NULL-VALUE                      VALUE 305.                     
012360       88  RESOURCE-WRONG                  VALUE 904.                     
012370   03  GOOD-SQLCODES.                                                     
012380       05  GOOD-SQLCODE OCCURS 5                                          
012390           INDEXED BY SQLCODE-IX PIC 9(3).                                
013000     EJECT                                                                
013001                                                                          
013010 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
013020*01  -COPY T01LSEL -PRE T01LSEL-                                          
013030     EJECT                                                                
013040 01  FILLER                      PIC X(16)   VALUE 'T01PDEV-AREA'.        
013050*01  -COPY T01PDEV -PRE T01PDEV-                                          
013060     EJECT                                                                
013070 01  FILLER                      PIC X(16)   VALUE 'SELECT-AREA '.        
013080*01  -COPY T01PDEV -PRE SELECT-                                           
013090     EJECT                                                                
013091 01  FILLER                      PIC X(16)   VALUE 'T01SDEV-AREA'.        
013092*01  -COPY T01SDEV -PRE T01SDEV-                                          
013093     EJECT                                                                
013094     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
013095     EJECT                                                                
013096     EXEC SQL INCLUDE T01PDEV END-EXEC.                                   
013097     EJECT                                                                
013098     EXEC SQL INCLUDE T01SDEV END-EXEC.                                   
013099     EJECT                                                                
013100                                                                          
013200 LINKAGE SECTION.                                                         
013600 PROCEDURE DIVISION.                                                      
013700 MAIN SECTION.                                                            
014000     PERFORM A-INIT                                                       
014100                                                                          
014200     PERFORM B-EXECUTE                                                    
014300                                                                          
014400     PERFORM Z-FINISH                                                     
014500     MOVE ZERO TO RETURN-CODE                                             
014600     GOBACK                                                               
014700     .                                                                    
014800     EJECT                                                                
014900                                                                          
015000 A-INIT SECTION.                                                          
015100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
015200     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
015300     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
015400     MOVE 25                          TO DAYS-KVDAYS                      
015500     MOVE ' '                         TO DAYS-IDCALEND                    
015600     MOVE SPACE                       TO DAYS-TIDATE1                     
015700     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
015800     CALL WZ20DAYS USING                                                  
015900          DAYS-WZ20DAYS                                                   
016000     IF DAYS-KDRC = ZERO                                                  
016010*** IT'S PREVIUS MONTH THAT SHOULD BE PROCESSED                           
016100       MOVE DAYS-TIDATE1(1:6)         TO WS-RUNDATUM-START(1:6)           
016101       MOVE '01'                      TO WS-RUNDATUM-START(7:2)           
016110       MOVE DAYS-TIDATE1(1:6)         TO WS-RUNDATUM-END                  
016120       MOVE '31'                      TO WS-RUNDATUM-END(7:2)             
016200     END-IF                                                               
016300     .                                                                    
016400     EJECT                                                                
016500                                                                          
016600 B-EXECUTE SECTION.                                                       
016701     PERFORM DB2-SELECT-T01PDEV-SELECT                                    
016800     IF LINES-FOUND                                                       
016900       PERFORM DB2-SELECT-T01PDEV-MONTHLY                                 
017000       IF LINES-FOUND                                                     
017100         PERFORM S09-MOVE-TO-RESPOND                                      
017200         PERFORM S10-MOVE-LINE-TO-RESPOND                                 
017400       ELSE                                                               
017500         CONTINUE                                                         
017800       END-IF                                                             
017900     ELSE                                                                 
018000       CONTINUE                                                           
018100     END-IF                                                               
018110**** RUN KINA SOFTWARE                                                    
018200     PERFORM S12-MOVE-TO-RESPOND                                          
018300     PERFORM S13-MOVE-LINE-TO-RESPOND                                     
026400     .                                                                    
026500     EJECT                                                                
026600                                                                          
034500 Z-FINISH SECTION.                                                        
034600     CONTINUE                                                             
034700     .                                                                    
034800     EJECT                                                                
034900                                                                          
034910 S09-MOVE-TO-RESPOND SECTION.                                             
034920     MOVE 1                          TO MAIL-REQU-IDMSGVER                
034930     MOVE 'R'                        TO MAIL-REQU-KDPGMACT                
034940     MOVE IDPGM                      TO MAIL-REQU-IDUSER                  
034950                                                                          
034960     MOVE 'WF1062-001'               TO HDR-IDOUTTYPE                     
034970     MOVE SPACE                      TO HDR-IDOUTREC                      
034980     MOVE 'VCCS'                     TO HDR-IDOUTREC(1:4)                 
034990     MOVE WS-CURRENT-DATE            TO HDR-IDLIST                        
034991                                                                          
034992     IF T01PDEV-FLPAYTE  = 'J'                                            
034993       MOVE 'Y'                   TO MAIL-FLPAYTE                         
034994       IF SELECT-FLPAYTE = 'N'                                            
034995         MOVE 'N'                 TO MAIL-FLPAYTE                         
034996       END-IF                                                             
034997     ELSE                                                                 
034998       MOVE T01PDEV-FLPAYTE       TO MAIL-FLPAYTE                         
034999     END-IF                                                               
035000     IF T01PDEV-FLDELTE  = 'J'                                            
035001       MOVE 'Y'                   TO MAIL-FLDELTE                         
035002       IF SELECT-FLDELTE = 'N'                                            
035003         MOVE 'N'                 TO MAIL-FLDELTE                         
035004       END-IF                                                             
035005     ELSE                                                                 
035006       MOVE T01PDEV-FLDELTE       TO MAIL-FLDELTE                         
035007     END-IF                                                               
035008     MOVE T01PDEV-REARTRAB        TO MAIL-REARTRAB                        
035009     IF SELECT-REARTRAB > T01PDEV-REARTRAB                                
035010       MOVE SELECT-REARTRAB       TO MAIL-REARTRAB                        
035011     END-IF                                                               
035012     IF SELECT-REARTRAB = ZERO                                            
035013       MOVE SELECT-REARTRAB       TO MAIL-REARTRAB                        
035014     END-IF                                                               
035015     MOVE T01PDEV-PRARTNTO-MIN    TO MAIL-PRARTNTO-MIN                    
035016     IF SELECT-PRARTNTO-MIN < T01PDEV-PRARTNTO-MIN                        
035017       MOVE SELECT-PRARTNTO-MIN   TO MAIL-PRARTNTO-MIN                    
035018     END-IF                                                               
035019     IF SELECT-PRARTNTO-MIN = ZERO                                        
035020       MOVE SELECT-PRARTNTO-MIN   TO MAIL-PRARTNTO-MIN                    
035021     END-IF                                                               
035022     MOVE T01PDEV-PRARTNTO-MAX    TO MAIL-PRARTNTO-MAX                    
035023     IF SELECT-PRARTNTO-MAX > T01PDEV-PRARTNTO-MAX                        
035024       MOVE SELECT-PRARTNTO-MAX   TO MAIL-PRARTNTO-MAX                    
035025     END-IF                                                               
035026     IF SELECT-PRARTNTO-MAX = ZERO                                        
035027       MOVE SELECT-PRARTNTO-MAX   TO MAIL-PRARTNTO-MAX                    
035028     END-IF                                                               
035029     MOVE T01PDEV-SUNTO-MIN       TO MAIL-SUNTO-MIN                       
035030     IF SELECT-SUNTO-MIN < T01PDEV-SUNTO-MIN                              
035031       MOVE SELECT-SUNTO-MIN      TO MAIL-SUNTO-MIN                       
035032     END-IF                                                               
035033     IF SELECT-SUNTO-MIN = ZERO                                           
035034       MOVE SELECT-SUNTO-MIN      TO MAIL-SUNTO-MIN                       
035035     END-IF                                                               
035036     MOVE T01PDEV-SUNTO-MAX       TO MAIL-SUNTO-MAX                       
035037     IF SELECT-SUNTO-MAX > T01PDEV-SUNTO-MAX                              
035038       MOVE SELECT-SUNTO-MAX      TO MAIL-SUNTO-MAX                       
035039     END-IF                                                               
035040     IF SELECT-SUNTO-MAX = ZERO                                           
035041       MOVE SELECT-SUNTO-MAX      TO MAIL-SUNTO-MAX                       
035042     END-IF                                                               
035043     IF T01PDEV-FLSOFT   = 'J'                                            
035044       MOVE 'Y'                   TO MAIL-FLSOFT                          
035045       IF SELECT-FLSOFT  = 'N'                                            
035046         MOVE 'N'                 TO MAIL-FLSOFT                          
035047       END-IF                                                             
035048     ELSE                                                                 
035049       MOVE T01PDEV-FLSOFT        TO MAIL-FLSOFT                          
035050     END-IF                                                               
035051     IF T01PDEV-FLFREE   = 'J'                                            
035052       MOVE 'Y'                   TO MAIL-FLFREE                          
035053       IF SELECT-FLFREE  = 'N'                                            
035054         MOVE 'N'                 TO MAIL-FLFREE                          
035055       END-IF                                                             
035056     ELSE                                                                 
035057       MOVE T01PDEV-FLFREE        TO MAIL-FLFREE                          
035058     END-IF                                                               
035059     IF T01PDEV-FLSERV   = 'J'                                            
035060       MOVE 'Y'                   TO MAIL-FLSERV                          
035061       IF SELECT-FLSERV  = 'N'                                            
035062         MOVE 'N'                 TO MAIL-FLSERV                          
035063       END-IF                                                             
035064     ELSE                                                                 
035065       MOVE T01PDEV-FLSERV        TO MAIL-FLSERV                          
035066     END-IF                                                               
035067     IF T01PDEV-FLINVOIC = 'J'                                            
035068       MOVE 'Y'                   TO MAIL-FLINVOIC                        
035069     ELSE                                                                 
035070       MOVE T01PDEV-FLINVOIC      TO MAIL-FLINVOIC                        
035071     END-IF                                                               
035072     MOVE WS-RUNDATUM-START       TO MAIL-DAREGDAT                        
035073     MOVE WS-RUNDATUM-END         TO MAIL-DAUPPDAT                        
035074     MOVE 'PAYMENT TERMS'         TO MAIL-BETEXT-01                       
035075     MOVE 'DELIVERY TERMS'        TO MAIL-BETEXT-02                       
035076     MOVE 'HIGH DISCOUNT (%)'     TO MAIL-BETEXT-03                       
035077     MOVE 'LOW NET PRICE (SEK)'   TO MAIL-BETEXT-04                       
035078     MOVE 'HIGH NET PRICE (SEK)'  TO MAIL-BETEXT-05                       
035079     MOVE 'LOW NET VALUE (SEK)'   TO MAIL-BETEXT-06                       
035080     MOVE 'HIGH NET VALUE (SEK)'  TO MAIL-BETEXT-07                       
035081     MOVE 'SOFTWARE'              TO MAIL-BETEXT-08                       
035082     MOVE 'FREEWARE'              TO MAIL-BETEXT-09                       
035083     MOVE 'SERVICES'              TO MAIL-BETEXT-10                       
035084     MOVE 'PULS INVOICES'         TO MAIL-BETEXT-11                       
035085     MOVE 'DATE FROM'             TO MAIL-BETEXT-12                       
035086     MOVE 'DATE TO'               TO MAIL-BETEXT-13                       
035087                                                                          
035088     MOVE 'LEGAL SELLER'          TO LINE-BETEXT-01                       
035089     MOVE 'ABNORMAL VALUE CAUSE'  TO LINE-BETEXT-02                       
035090     MOVE 'INVOICE DATE'          TO LINE-BETEXT-03                       
035091     MOVE 'INVOICE NO'            TO LINE-BETEXT-04                       
035092     MOVE 'PART NUMBER'           TO LINE-BETEXT-05                       
035093     MOVE 'PART DESCRIPTION'      TO LINE-BETEXT-06                       
035094     MOVE 'FINANCIAL CUSTOMER'    TO LINE-BETEXT-07                       
035095     MOVE 'CUSTOMER INFO 1'       TO LINE-BETEXT-08                       
035096     MOVE 'CUSTOMER INFO 2'       TO LINE-BETEXT-09                       
035097     MOVE 'ORDER REFERENCE'       TO LINE-BETEXT-10                       
035098     MOVE 'FINANCIAL DOCUMENT'    TO LINE-BETEXT-11                       
035099     MOVE 'SOFTWARE'              TO LINE-BETEXT-12                       
035100     MOVE 'FREEWARE'              TO LINE-BETEXT-13                       
035101     MOVE 'DISCOUNT (%)'          TO LINE-BETEXT-14                       
035102     MOVE 'NET PRICE (SEK)'       TO LINE-BETEXT-15                       
035103     MOVE 'NET VALUE (SEK)'       TO LINE-BETEXT-16                       
035104     MOVE 'CURRENCY CODE'         TO LINE-BETEXT-17                       
035105     MOVE 'NET PRICE'             TO LINE-BETEXT-18                       
035106     MOVE 'NET VALUE'             TO LINE-BETEXT-19                       
035107                                                                          
035108     PERFORM S90-SEND-OPEN                                                
035109     PERFORM S90-PUT-HEADER                                               
035110     PERFORM S90-PUT-DOC-HEAD1                                            
035111     PERFORM S90-PUT-DOC-HEAD2                                            
035112     PERFORM S90-PUT-DOC-LINE1                                            
035113     .                                                                    
035114                                                                          
035115 S10-MOVE-LINE-TO-RESPOND SECTION.                                        
035116     IF T01PDEV-FLPAYTE = 'J'                                             
035117       MOVE 'PAYMENT TERMS MISS'      TO WS-BETEXT-A                      
035118     ELSE                                                                 
035119       MOVE '?'                       TO WS-BETEXT-A                      
035120     END-IF                                                               
035121     IF T01PDEV-FLDELTE = 'J'                                             
035122       MOVE 'DELIVERY TERMS MISS'     TO WS-BETEXT-B                      
035123     ELSE                                                                 
035124       MOVE '?'                       TO WS-BETEXT-B                      
035125     END-IF                                                               
035126     IF T01PDEV-REARTRAB > ZERO                                           
035127       MOVE 'DISCOUNT IS HIGH'        TO WS-BETEXT-C                      
035128       MOVE T01PDEV-REARTRAB          TO WS-REARTRAB                      
035129     ELSE                                                                 
035130       MOVE '?'                       TO WS-BETEXT-C                      
035131       MOVE T01PDEV-REARTRAB          TO WS-REARTRAB                      
035132     END-IF                                                               
035133     IF T01PDEV-PRARTNTO-MIN > ZERO                                       
035134       MOVE 'NET PRICE IS LOW'        TO WS-BETEXT-D                      
035135       MOVE T01PDEV-PRARTNTO-MIN      TO WS-PRARTNTO-MIN                  
035136     ELSE                                                                 
035137       MOVE '?'                       TO WS-BETEXT-D                      
035138       MOVE T01PDEV-PRARTNTO-MIN      TO WS-PRARTNTO-MIN                  
035139     END-IF                                                               
035140     IF T01PDEV-PRARTNTO-MAX > ZERO                                       
035141       MOVE 'NET PRICE IS HIGH'       TO WS-BETEXT-E                      
035142       MOVE T01PDEV-PRARTNTO-MAX      TO WS-PRARTNTO-MAX                  
035143     ELSE                                                                 
035144       MOVE '?'                       TO WS-BETEXT-E                      
035145       MOVE T01PDEV-PRARTNTO-MAX      TO WS-PRARTNTO-MAX                  
035146     END-IF                                                               
035147     IF T01PDEV-SUNTO-MIN        > ZERO                                   
035148       MOVE 'NET VALUE IS LOW'        TO WS-BETEXT-F                      
035149       MOVE T01PDEV-SUNTO-MIN         TO WS-SUNTO-MIN                     
035150     ELSE                                                                 
035151       MOVE '?'                       TO WS-BETEXT-F                      
035152       MOVE T01PDEV-SUNTO-MIN         TO WS-SUNTO-MIN                     
035153     END-IF                                                               
035154     IF T01PDEV-SUNTO-MAX        > ZERO                                   
035155       MOVE 'NET VALUE IS HIGH'       TO WS-BETEXT-G                      
035156       MOVE T01PDEV-SUNTO-MAX         TO WS-SUNTO-MAX                     
035157     ELSE                                                                 
035158       MOVE '?'                       TO WS-BETEXT-G                      
035159       MOVE T01PDEV-SUNTO-MAX         TO WS-SUNTO-MAX                     
035160     END-IF                                                               
035161     IF T01PDEV-FLSOFT = 'J'                                              
035162       MOVE 'J'                       TO WS-FLSOFT                        
035163     ELSE                                                                 
035164       MOVE 'N'                       TO WS-FLSOFT                        
035165     END-IF                                                               
035166     IF T01PDEV-FLFREE = 'J'                                              
035167       MOVE 'J'                       TO WS-FLFREE                        
035168     ELSE                                                                 
035169       MOVE 'N'                       TO WS-FLFREE                        
035170     END-IF                                                               
035171     IF T01PDEV-FLSERV = 'J'                                              
035172       IF T01PDEV-FLINVOIC = 'J'                                          
035181         PERFORM DB2-OPEN-T01SDEV-4                                       
035182         PERFORM DB2-FETCH-T01SDEV-4                                      
035183         MOVE +1 TO IX                                                    
035184         PERFORM UNTIL LINES-MISSING                                      
035185           PERFORM S11-MOVE-TO-LINES                                      
035191           PERFORM DB2-FETCH-T01SDEV-4                                    
035192           ADD +1 TO IX                                                   
035193         END-PERFORM                                                      
035194         PERFORM DB2-CLOSE-T01SDEV-4                                      
035195       ELSE                                                               
035204         PERFORM DB2-OPEN-T01SDEV-2                                       
035205         PERFORM DB2-FETCH-T01SDEV-2                                      
035206         MOVE +1 TO IX                                                    
035207         PERFORM UNTIL LINES-MISSING                                      
035208           PERFORM S11-MOVE-TO-LINES                                      
035214           PERFORM DB2-FETCH-T01SDEV-2                                    
035215           ADD +1 TO IX                                                   
035216         END-PERFORM                                                      
035217         PERFORM DB2-CLOSE-T01SDEV-2                                      
035218       END-IF                                                             
035219     ELSE                                                                 
035220       IF T01PDEV-FLINVOIC = 'J'                                          
035229         PERFORM DB2-OPEN-T01SDEV-3                                       
035230         PERFORM DB2-FETCH-T01SDEV-3                                      
035231         MOVE +1 TO IX                                                    
035232         PERFORM UNTIL LINES-MISSING                                      
035233           PERFORM S11-MOVE-TO-LINES                                      
035239           PERFORM DB2-FETCH-T01SDEV-3                                    
035240           ADD +1 TO IX                                                   
035241         END-PERFORM                                                      
035242         PERFORM DB2-CLOSE-T01SDEV-3                                      
035243       ELSE                                                               
035252         PERFORM DB2-OPEN-T01SDEV-1                                       
035253         PERFORM DB2-FETCH-T01SDEV-1                                      
035254         MOVE +1 TO IX                                                    
035255         PERFORM UNTIL LINES-MISSING                                      
035256           PERFORM S11-MOVE-TO-LINES                                      
035262           PERFORM DB2-FETCH-T01SDEV-1                                    
035263           ADD +1 TO IX                                                   
035264         END-PERFORM                                                      
035265         PERFORM DB2-CLOSE-T01SDEV-1                                      
035266       END-IF                                                             
035267     END-IF                                                               
035268                                                                          
035269     PERFORM S90-SEND-CLOSE                                               
035270     .                                                                    
035271                                                                          
035272 S11-MOVE-TO-LINES        SECTION.                                        
035273     MOVE T01SDEV-IDLEGSEL             TO LINE-IDLEGSEL                   
035274     MOVE T01SDEV-BETEXT               TO LINE-BETEXT                     
035275     MOVE T01SDEV-DAREGDAT             TO LINE-DAREGDAT                   
035276     MOVE T01SDEV-IDFINDOC             TO LINE-IDFINDOC                   
035277     MOVE T01SDEV-IDARTNR-FINANCE(1:9) TO LINE-IDARTNR                    
035278     MOVE T01SDEV-BEART                TO LINE-BEART                      
035279     MOVE T01SDEV-IDPARTNR             TO LINE-IDPARTNR                   
035280     MOVE T01SDEV-IDEXCUST-1           TO LINE-IDEXCUST-1                 
035281     MOVE T01SDEV-IDEXCUST-2           TO LINE-IDEXCUST-2                 
035282     MOVE T01SDEV-IDREF                TO LINE-IDREF                      
035283     MOVE T01SDEV-KDFINDOC             TO LINE-KDFINDOC                   
035284     MOVE T01SDEV-FLSOFT               TO LINE-FLSOFT                     
035285     MOVE T01SDEV-FLFREE               TO LINE-FLFREE                     
035286     MOVE T01SDEV-REARTRAB             TO LINE-REARTRAB                   
035287     MOVE T01SDEV-PRARTNTO-SEK         TO LINE-PRARTNTO-SEK               
035288     MOVE T01SDEV-SUNTO-SEK            TO LINE-SUNTO-SEK                  
035289     MOVE T01SDEV-KDVALISO             TO LINE-KDVALISO                   
035290     MOVE T01SDEV-PRARTNTO             TO LINE-PRARTNTO                   
035291     MOVE T01SDEV-SUNTO                TO LINE-SUNTO                      
035292                                                                          
035293     PERFORM S90-PUT-DOC-LINE2                                            
035294     .                                                                    
035295                                                                          
035296 S12-MOVE-TO-RESPOND SECTION.                                             
035297     MOVE 1                          TO MAIL-REQU-IDMSGVER                
035298     MOVE 'R'                        TO MAIL-REQU-KDPGMACT                
035299     MOVE IDPGM                      TO MAIL-REQU-IDUSER                  
035300                                                                          
035301     MOVE 'WF1062-0CN'               TO HDR-IDOUTTYPE                     
035302     MOVE SPACE                      TO HDR-IDOUTREC                      
035303     MOVE 'VCCS'                     TO HDR-IDOUTREC(1:4)                 
035304     MOVE WS-CURRENT-DATE            TO HDR-IDLIST                        
035305                                                                          
035402     MOVE 'LEGAL SELLER'          TO LINESW-BETEXT-01                     
035403     MOVE 'ABNORMAL VALUE CAUSE'  TO LINESW-BETEXT-02                     
035404     MOVE 'INVOICE DATE'          TO LINESW-BETEXT-03                     
035405     MOVE 'INVOICE NO'            TO LINESW-BETEXT-04                     
035406     MOVE 'PART NUMBER'           TO LINESW-BETEXT-05                     
035407     MOVE 'PART DESCRIPTION'      TO LINESW-BETEXT-06                     
035408     MOVE 'FINANCIAL CUSTOMER'    TO LINESW-BETEXT-07                     
035409     MOVE 'CUSTOMER INFO 1'       TO LINESW-BETEXT-08                     
035410     MOVE 'CUSTOMER INFO 2'       TO LINESW-BETEXT-09                     
035411     MOVE 'ORDER REFERENCE'       TO LINESW-BETEXT-10                     
035412     MOVE 'FINANCIAL DOCUMENT'    TO LINESW-BETEXT-11                     
035413     MOVE 'SOFTWARE'              TO LINESW-BETEXT-12                     
035414     MOVE 'FREEWARE'              TO LINESW-BETEXT-13                     
035415     MOVE 'DISCOUNT (%)'          TO LINESW-BETEXT-14                     
035416     MOVE 'NET PRICE (SEK)'       TO LINESW-BETEXT-15                     
035417     MOVE 'NET VALUE (SEK)'       TO LINESW-BETEXT-16                     
035418     MOVE 'CURRENCY CODE'         TO LINESW-BETEXT-17                     
035419     MOVE 'NET PRICE'             TO LINESW-BETEXT-18                     
035420     MOVE 'NET VALUE'             TO LINESW-BETEXT-19                     
035421     MOVE 'DELIVERED QTY.'        TO LINESW-BETEXT-20                     
035422     MOVE 'STATISTIC NUMBER'      TO LINESW-BETEXT-21                     
035423     MOVE 'SUPPLIER'              TO LINESW-BETEXT-22                     
035424     MOVE 'DOCUMENT DATE'         TO LINESW-BETEXT-23                     
035425                                                                          
035426     PERFORM S90-SEND-OPEN                                                
035427     PERFORM S90-PUT-HEADER                                               
035430     PERFORM S90-PUT-DOC-LINE3                                            
035431     .                                                                    
035432                                                                          
035433 S13-MOVE-LINE-TO-RESPOND SECTION.                                        
035434     MOVE 'SOFTWARE TO CHINA'       TO WS-BETEXT-H                        
035491     PERFORM DB2-OPEN-T01SDEV-SW                                          
035492     PERFORM DB2-FETCH-T01SDEV-SW                                         
035493     MOVE +1 TO IX                                                        
035494     PERFORM UNTIL LINES-MISSING                                          
035495       PERFORM S14-MOVE-TO-LINES                                          
035496       PERFORM DB2-FETCH-T01SDEV-SW                                       
035497       ADD +1 TO IX                                                       
035498     END-PERFORM                                                          
035499     PERFORM DB2-CLOSE-T01SDEV-SW                                         
035534                                                                          
035535     PERFORM S90-SEND-CLOSE                                               
035536     .                                                                    
035537                                                                          
035538 S14-MOVE-TO-LINES        SECTION.                                        
035539     MOVE T01SDEV-IDLEGSEL             TO LINESW-IDLEGSEL                 
035540     MOVE T01SDEV-BETEXT               TO LINESW-BETEXT                   
035541     MOVE T01SDEV-DAREGDAT             TO LINESW-DAREGDAT                 
035542     MOVE T01SDEV-IDFINDOC             TO LINESW-IDFINDOC                 
035543     MOVE T01SDEV-IDARTNR-FINANCE(1:9) TO LINESW-IDARTNR                  
035544     MOVE T01SDEV-BEART                TO LINESW-BEART                    
035545     MOVE T01SDEV-IDPARTNR             TO LINESW-IDPARTNR                 
035546     MOVE T01SDEV-IDEXCUST-1           TO LINESW-IDEXCUST-1               
035547     MOVE T01SDEV-IDEXCUST-2           TO LINESW-IDEXCUST-2               
035548     MOVE T01SDEV-IDREF                TO LINESW-IDREF                    
035549     MOVE T01SDEV-KDFINDOC             TO LINESW-KDFINDOC                 
035550     MOVE T01SDEV-FLSOFT               TO LINESW-FLSOFT                   
035551     MOVE T01SDEV-FLFREE               TO LINESW-FLFREE                   
035552     MOVE T01SDEV-REARTRAB             TO LINESW-REARTRAB                 
035553     MOVE T01SDEV-PRARTNTO-SEK         TO LINESW-PRARTNTO-SEK             
035554     MOVE T01SDEV-SUNTO-SEK            TO LINESW-SUNTO-SEK                
035555     MOVE T01SDEV-KDVALISO             TO LINESW-KDVALISO                 
035556     MOVE T01SDEV-PRARTNTO             TO LINESW-PRARTNTO                 
035557     MOVE T01SDEV-SUNTO                TO LINESW-SUNTO                    
035558     MOVE T01SDEV-KVLEVART             TO LINESW-KVLEVART                 
035559     MOVE T01SDEV-IDSTATNR             TO LINESW-IDSTATNR                 
035560     MOVE T01SDEV-IDLEVNR              TO LINESW-IDLEVNR                  
035561     MOVE T01SDEV-DAFINDOC             TO LINESW-DAFINDOC                 
035562                                                                          
035563     PERFORM S90-PUT-DOC-LINE4                                            
035564     .                                                                    
035565                                                                          
035566 S90-SEND-OPEN SECTION.                                                   
035567     MOVE WS-ADRESS2                      TO SEND-ADDISPABS               
035568     MOVE 'OPEN'                          TO SEND-KDFUNC                  
035569     CALL WZ01SEND USING SEND-CONTROL-AREA                                
035570                         SEND-OPEN-AREA                                   
035571     IF SEND-KDRC > ZERO                                                  
035580       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
035700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
035800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035900     END-IF                                                               
036000     .                                                                    
036100                                                                          
036200 S90-PUT-HEADER SECTION.                                                  
036300     MOVE 'PUT'                           TO SEND-KDFUNC                  
036500     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
036600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036700                         SEND-KVDLEN                                      
036800                         HDR-AREA                                         
036900     IF SEND-KDRC > ZERO                                                  
037000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
037100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
037200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
037300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037400     END-IF                                                               
037500     .                                                                    
037600                                                                          
037700 S90-PUT-DOC-HEAD1 SECTION.                                               
037800     MOVE 'PUT'                           TO SEND-KDFUNC                  
038000     MOVE LENGTH OF DOC-HEAD-AREA1        TO SEND-KVDLEN                  
038100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
038200                         SEND-KVDLEN                                      
038300                         DOC-HEAD-AREA1                                   
038400     IF SEND-KDRC > ZERO                                                  
038500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
038600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
038700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
038800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
038900     END-IF                                                               
039000     .                                                                    
039100                                                                          
039200 S90-PUT-DOC-HEAD2 SECTION.                                               
039300     MOVE 'PUT'                           TO SEND-KDFUNC                  
039500     MOVE LENGTH OF DOC-HEAD-AREA2        TO SEND-KVDLEN                  
039600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039700                         SEND-KVDLEN                                      
039800                         DOC-HEAD-AREA2                                   
039810     IF SEND-KDRC > ZERO                                                  
039820       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
039830       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
039840       DELIMITED BY SIZE INTO ERROR-TEXT                                  
039850       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039860     END-IF                                                               
039870     .                                                                    
039880                                                                          
039890 S90-PUT-DOC-LINE1 SECTION.                                               
039891     MOVE 'PUT'                           TO SEND-KDFUNC                  
039893     MOVE LENGTH OF DOC-LINE-AREA1        TO SEND-KVDLEN                  
039894     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039895                         SEND-KVDLEN                                      
039896                         DOC-LINE-AREA1                                   
039897     IF SEND-KDRC > ZERO                                                  
039898       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
039899       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
039900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
039901       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039902     END-IF                                                               
039903     .                                                                    
039904                                                                          
039905 S90-PUT-DOC-LINE2 SECTION.                                               
039906     MOVE 'PUT'                           TO SEND-KDFUNC                  
039908     MOVE LENGTH OF DOC-LINE-AREA2        TO SEND-KVDLEN                  
039909     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039910                         SEND-KVDLEN                                      
039911                         DOC-LINE-AREA2                                   
039912     IF SEND-KDRC > ZERO                                                  
039913       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
039914       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
039915       DELIMITED BY SIZE INTO ERROR-TEXT                                  
039916       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039917     END-IF                                                               
039918     .                                                                    
039919                                                                          
039920 S90-PUT-DOC-LINE3 SECTION.                                               
039921     MOVE 'PUT'                           TO SEND-KDFUNC                  
039922     MOVE LENGTH OF DOC-LINE-AREA3        TO SEND-KVDLEN                  
039923     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039924                         SEND-KVDLEN                                      
039925                         DOC-LINE-AREA3                                   
039926     IF SEND-KDRC > ZERO                                                  
039927       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
039928       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
039929       DELIMITED BY SIZE INTO ERROR-TEXT                                  
039930       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039931     END-IF                                                               
039932     .                                                                    
039933                                                                          
039934 S90-PUT-DOC-LINE4 SECTION.                                               
039935     MOVE 'PUT'                           TO SEND-KDFUNC                  
039936     MOVE LENGTH OF DOC-LINE-AREA4        TO SEND-KVDLEN                  
039937     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039938                         SEND-KVDLEN                                      
039939                         DOC-LINE-AREA4                                   
039940     IF SEND-KDRC > ZERO                                                  
039941       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
039942       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
039943       DELIMITED BY SIZE INTO ERROR-TEXT                                  
039944       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039945     END-IF                                                               
039946     .                                                                    
039947                                                                          
039976 S90-SEND-CLOSE SECTION.                                                  
039977     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
039978     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039979     .                                                                    
039980                                                                          
039990* --- DB2 SECTIONS  ---                                                   
040000*                                                                         
045100 DB2-SELECT-T01PDEV-SELECT SECTION.                                       
045200     MOVE 000100305  TO GOOD-SQLCODES                                     
045300                                                                          
045400     EXEC SQL                                                             
045500         SELECT  IDLEGSEL                                                 
045600                ,KDBEHX                                                   
045700                ,FLPAYTE                                                  
045800                ,FLDELTE                                                  
045900                ,REARTRAB                                                 
046000                ,PRARTNTO_MIN                                             
046100                ,PRARTNTO_MAX                                             
046200                ,SUNTO_MIN                                                
046300                ,SUNTO_MAX                                                
046400                ,FLSOFT                                                   
046500                ,FLFREE                                                   
046600                ,FLSERV                                                   
046700                ,FLINVOIC                                                 
046800                ,KVMANAD                                                  
046900                ,DAREGDAT                                                 
047000                ,DAUPPDAT                                                 
047100                ,IDUSER                                                   
047200                                                                          
047300         INTO  :SELECT-IDLEGSEL                                           
047400             , :SELECT-KDBEHX                                             
047500             , :SELECT-FLPAYTE                                            
047600             , :SELECT-FLDELTE                                            
047700             , :SELECT-REARTRAB                                           
047800             , :SELECT-PRARTNTO-MIN                                       
047900             , :SELECT-PRARTNTO-MAX                                       
048000             , :SELECT-SUNTO-MIN                                          
048100             , :SELECT-SUNTO-MAX                                          
048200             , :SELECT-FLSOFT                                             
048300             , :SELECT-FLFREE                                             
048400             , :SELECT-FLSERV                                             
048500             , :SELECT-FLINVOIC                                           
048600             , :SELECT-KVMANAD                                            
048700             , :SELECT-DAREGDAT                                           
048800             , :SELECT-DAUPPDAT                                           
048900             , :SELECT-IDUSER                                             
049000                                                                          
049100         FROM  T01PDEV                                                    
049200                                                                          
049300         WHERE IDLEGSEL = 'VCCS'                                          
049400         AND   KDBEHX   = 'S'                                             
049500     END-EXEC                                                             
049600                                                                          
049700     MOVE SQLCODE TO SQLCODE-WS                                           
049800     PERFORM DB2-STATUS-CHECK                                             
049900     .                                                                    
050000                                                                          
050010 DB2-SELECT-T01PDEV-MONTHLY SECTION.                                      
050020     MOVE 000100305  TO GOOD-SQLCODES                                     
050030                                                                          
050040     EXEC SQL                                                             
050050         SELECT  IDLEGSEL                                                 
050060                ,KDBEHX                                                   
050070                ,FLPAYTE                                                  
050080                ,FLDELTE                                                  
050090                ,REARTRAB                                                 
050091                ,PRARTNTO_MIN                                             
050092                ,PRARTNTO_MAX                                             
050093                ,SUNTO_MIN                                                
050094                ,SUNTO_MAX                                                
050095                ,FLSOFT                                                   
050096                ,FLFREE                                                   
050097                ,FLSERV                                                   
050098                ,FLINVOIC                                                 
050099                ,KVMANAD                                                  
050100                ,DAREGDAT                                                 
050110                ,DAUPPDAT                                                 
050120                ,IDUSER                                                   
050130                                                                          
050140         INTO  :T01PDEV-IDLEGSEL                                          
050150             , :T01PDEV-KDBEHX                                            
050160             , :T01PDEV-FLPAYTE                                           
050170             , :T01PDEV-FLDELTE                                           
050180             , :T01PDEV-REARTRAB                                          
050190             , :T01PDEV-PRARTNTO-MIN                                      
050191             , :T01PDEV-PRARTNTO-MAX                                      
050192             , :T01PDEV-SUNTO-MIN                                         
050193             , :T01PDEV-SUNTO-MAX                                         
050194             , :T01PDEV-FLSOFT                                            
050195             , :T01PDEV-FLFREE                                            
050196             , :T01PDEV-FLSERV                                            
050197             , :T01PDEV-FLINVOIC                                          
050198             , :T01PDEV-KVMANAD                                           
050199             , :T01PDEV-DAREGDAT                                          
050200             , :T01PDEV-DAUPPDAT                                          
050210             , :T01PDEV-IDUSER                                            
050220                                                                          
050230         FROM  T01PDEV                                                    
050240                                                                          
050250         WHERE IDLEGSEL = 'VCCS'                                          
050260         AND   KDBEHX   = 'M'                                             
050270     END-EXEC                                                             
050280                                                                          
050290     MOVE SQLCODE TO SQLCODE-WS                                           
050291     PERFORM DB2-STATUS-CHECK                                             
050292     .                                                                    
050293                                                                          
050300* * * * * * * * * *   - CURSOR-1 -   * * * * * * * * * * * * * * *        
053800 DB2-OPEN-T01SDEV-1 SECTION.                                              
053900     MOVE 000100 TO GOOD-SQLCODES                                         
054000                                                                          
054100     EXEC SQL                                                             
054200         DECLARE T01SDEV-1 CURSOR WITH HOLD FOR                           
054300                                                                          
054400           SELECT  IDLEGSEL                                               
054500                  ,DAREGDAT                                               
054600                  ,BETEXT                                                 
054700                  ,IDFINDOC                                               
054800                  ,IDARTNR_FINANCE                                        
054900                  ,BEART                                                  
055000                  ,IDPARTNR                                               
055100                  ,IDEXCUST_1                                             
055200                  ,IDEXCUST_2                                             
055300                  ,IDREF                                                  
055400                  ,KDFINDOC                                               
055500                  ,FLSOFT                                                 
055600                  ,FLFREE                                                 
055700                  ,REARTRAB                                               
055800                  ,PRARTNTO                                               
055900                  ,SUNTO                                                  
056000                  ,KDVALISO                                               
056100                  ,PRARTNTO_SEK                                           
056200                  ,SUNTO_SEK                                              
056300                                                                          
056400           FROM    T01SDEV                                                
056500                                                                          
056600           WHERE    IDLEGSEL = 'VCCS'                                     
056700           AND     (FLSOFT =    :WS-FLSOFT                                
056800           OR       FLSOFT LIKE 'N%')                                     
056900           AND     (FLFREE =    :WS-FLFREE                                
057000           OR       FLFREE LIKE 'N%')                                     
057100           AND      BEART > ' '                                           
057200           AND      DAREGDAT >= :WS-RUNDATUM-START                        
057300           AND      DAREGDAT <= :WS-RUNDATUM-END                          
057400           AND    ((BETEXT   = :WS-BETEXT-A)                              
057500           OR      (BETEXT   = :WS-BETEXT-B)                              
057600           OR      (BETEXT   = :WS-BETEXT-C                               
057700           AND      REARTRAB > :WS-REARTRAB)                              
057800           OR      (BETEXT   = :WS-BETEXT-D                               
057900           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
058000           OR      (BETEXT   = :WS-BETEXT-E                               
058100           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
058200           OR      (BETEXT   = :WS-BETEXT-F                               
058300           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
058400           OR      (BETEXT   = :WS-BETEXT-G                               
058500           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
058600                                                                          
058700           ORDER BY IDLEGSEL                                              
058800     END-EXEC                                                             
058900                                                                          
059000     MOVE 000100  TO GOOD-SQLCODES                                        
059100                                                                          
059200     EXEC SQL                                                             
059300        OPEN T01SDEV-1                                                    
059400     END-EXEC                                                             
059500                                                                          
059600     MOVE SQLCODE TO SQLCODE-WS                                           
059700     PERFORM DB2-STATUS-CHECK                                             
059800     .                                                                    
059900                                                                          
060000 DB2-FETCH-T01SDEV-1 SECTION.                                             
060100     MOVE 000100  TO GOOD-SQLCODES                                        
060200                                                                          
060300     EXEC SQL                                                             
060400         FETCH T01SDEV-1                                                  
060500                                                                          
060600         INTO :T01SDEV-IDLEGSEL                                           
060700             ,:T01SDEV-DAREGDAT                                           
060800             ,:T01SDEV-BETEXT                                             
060900             ,:T01SDEV-IDFINDOC                                           
061000             ,:T01SDEV-IDARTNR-FINANCE                                    
061100             ,:T01SDEV-BEART                                              
061200             ,:T01SDEV-IDPARTNR                                           
061300             ,:T01SDEV-IDEXCUST-1                                         
061400             ,:T01SDEV-IDEXCUST-2                                         
061500             ,:T01SDEV-IDREF                                              
061600             ,:T01SDEV-KDFINDOC                                           
061700             ,:T01SDEV-FLSOFT                                             
061800             ,:T01SDEV-FLFREE                                             
061900             ,:T01SDEV-REARTRAB                                           
062000             ,:T01SDEV-PRARTNTO                                           
062100             ,:T01SDEV-SUNTO                                              
062200             ,:T01SDEV-KDVALISO                                           
062300             ,:T01SDEV-PRARTNTO-SEK                                       
062400             ,:T01SDEV-SUNTO-SEK                                          
062500                                                                          
062600     END-EXEC                                                             
062700                                                                          
062800     MOVE SQLCODE TO SQLCODE-WS                                           
062900     PERFORM DB2-STATUS-CHECK                                             
063000     .                                                                    
063100                                                                          
063200 DB2-CLOSE-T01SDEV-1 SECTION.                                             
063300     EXEC SQL                                                             
063400        CLOSE T01SDEV-1                                                   
063500     END-EXEC                                                             
063600     .                                                                    
063700                                                                          
063800* * * * * * * * * *   - CURSOR-2 -   * * * * * * * * * * * * * * *        
067400 DB2-OPEN-T01SDEV-2 SECTION.                                              
067500     MOVE 000100 TO GOOD-SQLCODES                                         
067600                                                                          
067700     EXEC SQL                                                             
067800         DECLARE T01SDEV-2 CURSOR WITH HOLD FOR                           
067900                                                                          
068000           SELECT  IDLEGSEL                                               
068100                  ,DAREGDAT                                               
068200                  ,BETEXT                                                 
068300                  ,IDFINDOC                                               
068400                  ,IDARTNR_FINANCE                                        
068500                  ,BEART                                                  
068600                  ,IDPARTNR                                               
068700                  ,IDEXCUST_1                                             
068800                  ,IDEXCUST_2                                             
068900                  ,IDREF                                                  
069000                  ,KDFINDOC                                               
069100                  ,FLSOFT                                                 
069200                  ,FLFREE                                                 
069300                  ,REARTRAB                                               
069400                  ,PRARTNTO                                               
069500                  ,SUNTO                                                  
069600                  ,KDVALISO                                               
069700                  ,PRARTNTO_SEK                                           
069800                  ,SUNTO_SEK                                              
069900                                                                          
070000           FROM    T01SDEV                                                
070100                                                                          
070200           WHERE    IDLEGSEL = 'VCCS'                                     
070300           AND     (FLSOFT =    :WS-FLSOFT                                
070400           OR       FLSOFT LIKE 'N%')                                     
070500           AND     (FLFREE =    :WS-FLFREE                                
070600           OR       FLFREE LIKE 'N%')                                     
070700           AND      DAREGDAT >= :WS-RUNDATUM-START                        
070800           AND      DAREGDAT <= :WS-RUNDATUM-END                          
070900           AND    ((BETEXT   = :WS-BETEXT-A)                              
071000           OR      (BETEXT   = :WS-BETEXT-B)                              
071100           OR      (BETEXT   = :WS-BETEXT-C                               
071200           AND      REARTRAB > :WS-REARTRAB)                              
071300           OR      (BETEXT   = :WS-BETEXT-D                               
071400           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
071500           OR      (BETEXT   = :WS-BETEXT-E                               
071600           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
071700           OR      (BETEXT   = :WS-BETEXT-F                               
071800           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
071900           OR      (BETEXT   = :WS-BETEXT-G                               
072000           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
072100                                                                          
072200           ORDER BY IDLEGSEL                                              
072300     END-EXEC                                                             
072400                                                                          
072500     MOVE 000100  TO GOOD-SQLCODES                                        
072600                                                                          
072700     EXEC SQL                                                             
072800        OPEN T01SDEV-2                                                    
072900     END-EXEC                                                             
073000                                                                          
073100     MOVE SQLCODE TO SQLCODE-WS                                           
073200     PERFORM DB2-STATUS-CHECK                                             
073300     .                                                                    
073400                                                                          
073500 DB2-FETCH-T01SDEV-2 SECTION.                                             
073600     MOVE 000100  TO GOOD-SQLCODES                                        
073700                                                                          
073800     EXEC SQL                                                             
073900         FETCH T01SDEV-2                                                  
074000                                                                          
074100         INTO :T01SDEV-IDLEGSEL                                           
074200             ,:T01SDEV-DAREGDAT                                           
074300             ,:T01SDEV-BETEXT                                             
074400             ,:T01SDEV-IDFINDOC                                           
074500             ,:T01SDEV-IDARTNR-FINANCE                                    
074600             ,:T01SDEV-BEART                                              
074700             ,:T01SDEV-IDPARTNR                                           
074800             ,:T01SDEV-IDEXCUST-1                                         
074900             ,:T01SDEV-IDEXCUST-2                                         
075000             ,:T01SDEV-IDREF                                              
075100             ,:T01SDEV-KDFINDOC                                           
075200             ,:T01SDEV-FLSOFT                                             
075300             ,:T01SDEV-FLFREE                                             
075400             ,:T01SDEV-REARTRAB                                           
075500             ,:T01SDEV-PRARTNTO                                           
075600             ,:T01SDEV-SUNTO                                              
075700             ,:T01SDEV-KDVALISO                                           
075800             ,:T01SDEV-PRARTNTO-SEK                                       
075900             ,:T01SDEV-SUNTO-SEK                                          
076000     END-EXEC                                                             
076100                                                                          
076200     MOVE SQLCODE TO SQLCODE-WS                                           
076300     PERFORM DB2-STATUS-CHECK                                             
076400     .                                                                    
076500                                                                          
076600 DB2-CLOSE-T01SDEV-2 SECTION.                                             
076700     EXEC SQL                                                             
076800        CLOSE T01SDEV-2                                                   
076900     END-EXEC                                                             
077000     .                                                                    
077100                                                                          
077200* * * * * * * * * *   - CURSOR-3 -   * * * * * * * * * * * * * * *        
078606 DB2-OPEN-T01SDEV-3 SECTION.                                              
078607     MOVE 000100 TO GOOD-SQLCODES                                         
078608                                                                          
078609     EXEC SQL                                                             
078610         DECLARE T01SDEV-3 CURSOR WITH HOLD FOR                           
078611                                                                          
078612           SELECT  IDLEGSEL                                               
078613                  ,DAREGDAT                                               
078614                  ,BETEXT                                                 
078615                  ,IDFINDOC                                               
078616                  ,IDARTNR_FINANCE                                        
078617                  ,BEART                                                  
078618                  ,IDPARTNR                                               
078619                  ,IDEXCUST_1                                             
078620                  ,IDEXCUST_2                                             
078621                  ,IDREF                                                  
078622                  ,KDFINDOC                                               
078623                  ,FLSOFT                                                 
078624                  ,FLFREE                                                 
078625                  ,REARTRAB                                               
078626                  ,PRARTNTO                                               
078627                  ,SUNTO                                                  
078628                  ,KDVALISO                                               
078629                  ,PRARTNTO_SEK                                           
078630                  ,SUNTO_SEK                                              
078631                                                                          
078632           FROM    T01SDEV                                                
078633                                                                          
078634           WHERE    IDLEGSEL = 'VCCS'                                     
078635           AND     (FLSOFT =    :WS-FLSOFT                                
078636           OR       FLSOFT LIKE 'N%')                                     
078637           AND     (FLFREE =    :WS-FLFREE                                
078638           OR       FLFREE LIKE 'N%')                                     
078639           AND      KDFINDOC = 'INV'                                      
078640           AND      BEART > ' '                                           
078641           AND      DAREGDAT >= :WS-RUNDATUM-START                        
078642           AND      DAREGDAT <= :WS-RUNDATUM-END                          
078643           AND    ((BETEXT   = :WS-BETEXT-A)                              
078644           OR      (BETEXT   = :WS-BETEXT-B)                              
078645           OR      (BETEXT   = :WS-BETEXT-C                               
078646           AND      REARTRAB > :WS-REARTRAB)                              
078647           OR      (BETEXT   = :WS-BETEXT-D                               
078648           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
078649           OR      (BETEXT   = :WS-BETEXT-E                               
078650           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
078651           OR      (BETEXT   = :WS-BETEXT-F                               
078652           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
078653           OR      (BETEXT   = :WS-BETEXT-G                               
078654           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
078655                                                                          
078656           ORDER BY IDLEGSEL                                              
078657     END-EXEC                                                             
078658                                                                          
078659     MOVE 000100  TO GOOD-SQLCODES                                        
078660                                                                          
078661     EXEC SQL                                                             
078662        OPEN T01SDEV-3                                                    
078663     END-EXEC                                                             
078664                                                                          
078665     MOVE SQLCODE TO SQLCODE-WS                                           
078666     PERFORM DB2-STATUS-CHECK                                             
078667     .                                                                    
078668                                                                          
078669 DB2-FETCH-T01SDEV-3 SECTION.                                             
078670     MOVE 000100  TO GOOD-SQLCODES                                        
078671                                                                          
078672     EXEC SQL                                                             
078673         FETCH T01SDEV-3                                                  
078674                                                                          
078675         INTO :T01SDEV-IDLEGSEL                                           
078676             ,:T01SDEV-DAREGDAT                                           
078677             ,:T01SDEV-BETEXT                                             
078678             ,:T01SDEV-IDFINDOC                                           
078679             ,:T01SDEV-IDARTNR-FINANCE                                    
078680             ,:T01SDEV-BEART                                              
078681             ,:T01SDEV-IDPARTNR                                           
078682             ,:T01SDEV-IDEXCUST-1                                         
078683             ,:T01SDEV-IDEXCUST-2                                         
078684             ,:T01SDEV-IDREF                                              
078685             ,:T01SDEV-KDFINDOC                                           
078686             ,:T01SDEV-FLSOFT                                             
078687             ,:T01SDEV-FLFREE                                             
078688             ,:T01SDEV-REARTRAB                                           
078689             ,:T01SDEV-PRARTNTO                                           
078690             ,:T01SDEV-SUNTO                                              
078691             ,:T01SDEV-KDVALISO                                           
078692             ,:T01SDEV-PRARTNTO-SEK                                       
078693             ,:T01SDEV-SUNTO-SEK                                          
078694     END-EXEC                                                             
078695                                                                          
078696     MOVE SQLCODE TO SQLCODE-WS                                           
078697     PERFORM DB2-STATUS-CHECK                                             
078698     .                                                                    
078699                                                                          
078700 DB2-CLOSE-T01SDEV-3 SECTION.                                             
078701     EXEC SQL                                                             
078702        CLOSE T01SDEV-3                                                   
078703     END-EXEC                                                             
078704     .                                                                    
078705                                                                          
078706* * * * * * * * * *   - CURSOR-4 -   * * * * * * * * * * * * * * *        
078743 DB2-OPEN-T01SDEV-4 SECTION.                                              
078744     MOVE 000100 TO GOOD-SQLCODES                                         
078745                                                                          
078746     EXEC SQL                                                             
078747         DECLARE T01SDEV-4 CURSOR WITH HOLD FOR                           
078748                                                                          
078749           SELECT  IDLEGSEL                                               
078750                  ,DAREGDAT                                               
078751                  ,BETEXT                                                 
078752                  ,IDFINDOC                                               
078753                  ,IDARTNR_FINANCE                                        
078754                  ,BEART                                                  
078755                  ,IDPARTNR                                               
078756                  ,IDEXCUST_1                                             
078757                  ,IDEXCUST_2                                             
078758                  ,IDREF                                                  
078759                  ,KDFINDOC                                               
078760                  ,FLSOFT                                                 
078761                  ,FLFREE                                                 
078762                  ,REARTRAB                                               
078763                  ,PRARTNTO                                               
078764                  ,SUNTO                                                  
078765                  ,KDVALISO                                               
078766                  ,PRARTNTO_SEK                                           
078767                  ,SUNTO_SEK                                              
078768                                                                          
078769           FROM    T01SDEV                                                
078770                                                                          
078771           WHERE    IDLEGSEL = 'VCCS'                                     
078772           AND     (FLSOFT =    :WS-FLSOFT                                
078773           OR       FLSOFT LIKE 'N%')                                     
078774           AND     (FLFREE =    :WS-FLFREE                                
078775           OR       FLFREE LIKE 'N%')                                     
078776           AND      KDFINDOC = 'INV'                                      
078777           AND      DAREGDAT >= :WS-RUNDATUM-START                        
078778           AND      DAREGDAT <= :WS-RUNDATUM-END                          
078779           AND    ((BETEXT   = :WS-BETEXT-A)                              
078780           OR      (BETEXT   = :WS-BETEXT-B)                              
078781           OR      (BETEXT   = :WS-BETEXT-C                               
078782           AND      REARTRAB > :WS-REARTRAB)                              
078783           OR      (BETEXT   = :WS-BETEXT-D                               
078784           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
078785           OR      (BETEXT   = :WS-BETEXT-E                               
078786           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
078787           OR      (BETEXT   = :WS-BETEXT-F                               
078788           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
078789           OR      (BETEXT   = :WS-BETEXT-G                               
078790           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
078791                                                                          
078792           ORDER BY IDLEGSEL                                              
078793     END-EXEC                                                             
078794                                                                          
078795     MOVE 000100  TO GOOD-SQLCODES                                        
078796                                                                          
078797     EXEC SQL                                                             
078798        OPEN T01SDEV-4                                                    
078799     END-EXEC                                                             
078800                                                                          
078801     MOVE SQLCODE TO SQLCODE-WS                                           
078802     PERFORM DB2-STATUS-CHECK                                             
078803     .                                                                    
078804                                                                          
078805 DB2-FETCH-T01SDEV-4 SECTION.                                             
078806     MOVE 000100  TO GOOD-SQLCODES                                        
078807                                                                          
078808     EXEC SQL                                                             
078809         FETCH T01SDEV-4                                                  
078810                                                                          
078811         INTO :T01SDEV-IDLEGSEL                                           
078812             ,:T01SDEV-DAREGDAT                                           
078813             ,:T01SDEV-BETEXT                                             
078814             ,:T01SDEV-IDFINDOC                                           
078815             ,:T01SDEV-IDARTNR-FINANCE                                    
078816             ,:T01SDEV-BEART                                              
078817             ,:T01SDEV-IDPARTNR                                           
078818             ,:T01SDEV-IDEXCUST-1                                         
078819             ,:T01SDEV-IDEXCUST-2                                         
078820             ,:T01SDEV-IDREF                                              
078821             ,:T01SDEV-KDFINDOC                                           
078822             ,:T01SDEV-FLSOFT                                             
078823             ,:T01SDEV-FLFREE                                             
078824             ,:T01SDEV-REARTRAB                                           
078825             ,:T01SDEV-PRARTNTO                                           
078826             ,:T01SDEV-SUNTO                                              
078827             ,:T01SDEV-KDVALISO                                           
078828             ,:T01SDEV-PRARTNTO-SEK                                       
078829             ,:T01SDEV-SUNTO-SEK                                          
078830     END-EXEC                                                             
078831                                                                          
078832     MOVE SQLCODE TO SQLCODE-WS                                           
078833     PERFORM DB2-STATUS-CHECK                                             
078834     .                                                                    
078835                                                                          
078836 DB2-CLOSE-T01SDEV-4 SECTION.                                             
078837     EXEC SQL                                                             
078838        CLOSE T01SDEV-4                                                   
078839     END-EXEC                                                             
078840     .                                                                    
078841                                                                          
078842 DB2-OPEN-T01SDEV-SW SECTION.                                             
078843     MOVE 000100 TO GOOD-SQLCODES                                         
078844                                                                          
078845     EXEC SQL                                                             
078846         DECLARE T01SDEV-SW CURSOR WITH HOLD FOR                          
078847                                                                          
078848           SELECT  IDLEGSEL                                               
078849                  ,DAREGDAT                                               
078850                  ,BETEXT                                                 
078851                  ,IDFINDOC                                               
078852                  ,IDARTNR_FINANCE                                        
078853                  ,BEART                                                  
078854                  ,IDPARTNR                                               
078855                  ,IDEXCUST_1                                             
078856                  ,IDEXCUST_2                                             
078857                  ,IDREF                                                  
078858                  ,KDFINDOC                                               
078859                  ,FLSOFT                                                 
078860                  ,FLFREE                                                 
078861                  ,REARTRAB                                               
078862                  ,PRARTNTO                                               
078863                  ,SUNTO                                                  
078864                  ,KDVALISO                                               
078865                  ,PRARTNTO_SEK                                           
078866                  ,SUNTO_SEK                                              
078867                  ,KVLEVART                                               
078868                  ,IDSTATNR                                               
078869                  ,IDLEVNR                                                
078870                  ,DAFINDOC                                               
078871                                                                          
078872           FROM    T01SDEV                                                
078873                                                                          
078874           WHERE    IDLEGSEL = 'VCCS'                                     
078875           AND     (FLSOFT =   'J'                                        
078876           OR       FLSOFT =   'Y')                                       
078877           AND      IDPARTNR = '352343'                                   
078880           AND      DAREGDAT >= :WS-RUNDATUM-START                        
078881           AND      DAREGDAT <= :WS-RUNDATUM-END                          
078892           AND      BETEXT   = :WS-BETEXT-H                               
078894                                                                          
078895           ORDER BY IDLEGSEL                                              
078896     END-EXEC                                                             
078897                                                                          
078898     MOVE 000100  TO GOOD-SQLCODES                                        
078899                                                                          
078900     EXEC SQL                                                             
078901        OPEN T01SDEV-SW                                                   
078902     END-EXEC                                                             
078903                                                                          
078904     MOVE SQLCODE TO SQLCODE-WS                                           
078905     PERFORM DB2-STATUS-CHECK                                             
078906     .                                                                    
078907                                                                          
078908 DB2-FETCH-T01SDEV-SW SECTION.                                            
078909     MOVE 000100  TO GOOD-SQLCODES                                        
078910                                                                          
078911     EXEC SQL                                                             
078912         FETCH T01SDEV-SW                                                 
078913                                                                          
078914         INTO :T01SDEV-IDLEGSEL                                           
078915             ,:T01SDEV-DAREGDAT                                           
078916             ,:T01SDEV-BETEXT                                             
078917             ,:T01SDEV-IDFINDOC                                           
078918             ,:T01SDEV-IDARTNR-FINANCE                                    
078919             ,:T01SDEV-BEART                                              
078920             ,:T01SDEV-IDPARTNR                                           
078921             ,:T01SDEV-IDEXCUST-1                                         
078922             ,:T01SDEV-IDEXCUST-2                                         
078923             ,:T01SDEV-IDREF                                              
078924             ,:T01SDEV-KDFINDOC                                           
078925             ,:T01SDEV-FLSOFT                                             
078926             ,:T01SDEV-FLFREE                                             
078927             ,:T01SDEV-REARTRAB                                           
078928             ,:T01SDEV-PRARTNTO                                           
078929             ,:T01SDEV-SUNTO                                              
078930             ,:T01SDEV-KDVALISO                                           
078931             ,:T01SDEV-PRARTNTO-SEK                                       
078932             ,:T01SDEV-SUNTO-SEK                                          
078933             ,:T01SDEV-KVLEVART                                           
078934             ,:T01SDEV-IDSTATNR                                           
078935             ,:T01SDEV-IDLEVNR                                            
078936             ,:T01SDEV-DAFINDOC                                           
078937     END-EXEC                                                             
078938                                                                          
078939     MOVE SQLCODE TO SQLCODE-WS                                           
078940     PERFORM DB2-STATUS-CHECK                                             
078941     .                                                                    
078942                                                                          
078943 DB2-CLOSE-T01SDEV-SW SECTION.                                            
078944     EXEC SQL                                                             
078945        CLOSE T01SDEV-SW                                                  
078946     END-EXEC                                                             
078947     .                                                                    
078948                                                                          
078949 DB2-STATUS-CHECK SECTION.                                                
078950     SET SQLCODE-IX         TO 1                                          
078951     SEARCH GOOD-SQLCODE AT END                                           
078960           CALL ABEND USING RKOD-ABEND-DB2                                
079000        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
079100           CONTINUE                                                       
079200     END-SEARCH                                                           
079300     .                                                                    
