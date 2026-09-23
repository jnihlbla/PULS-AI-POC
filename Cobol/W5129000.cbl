010002 ID DIVISION.                                                             
020002 PROGRAM-ID.     W5129000.                                                
030002 AUTHOR.         SARASWATHY S.                                            
040002 DATE-WRITTEN.   20/01/20.                                                
050002 DATE-COMPILED.                                                           
060002                                                                          
070002*                                                                         
080002*    FUNCTION:                                                            
090002*        CREATES OBSOLESCENCE DETAILED REPORT                             
100002*                                                                         
110002*                                                                         
120002*    ABENDCODES:                                                          
130002*        U0016 -  . . . .                                                 
140002*        U1000 -  . . . .                                                 
150002*                                                                         
160002                                                                          
170002     SKIP3                                                                
180002 ENVIRONMENT DIVISION.                                                    
190002     SKIP2                                                                
200002 INPUT-OUTPUT SECTION.                                                    
210002                                                                          
220002 FILE-CONTROL.                                                            
230002     SKIP2                                                                
240002*          --- W51284                                                     
250002     SELECT W51284                     ASSIGN TO W51290D1.                
260002     SKIP2                                                                
270002*          --- OBSOLESCENCE DETAILED REPORT                               
280002     SELECT W51290K                    ASSIGN TO W51290D2.                
290002     EJECT                                                                
300002 DATA DIVISION.                                                           
310002     SKIP3                                                                
320002 FILE SECTION.                                                            
330002     SKIP3                                                                
340002 FD  W51284                                                               
350002     RECORDING       F                                                    
360002     BLOCK CONTAINS  0.                                                   
370002                                                                          
380002*01  -COPY W51284      -L.                                                
390002     SKIP3                                                                
400002 FD  W51290K                                                              
410002     RECORDING       V                                                    
420002     BLOCK CONTAINS  0.                                                   
430002     EJECT                                                                
440002 01  W51290-K-REC            PIC X(250).                                  
450002     SKIP2                                                                
460002 WORKING-STORAGE SECTION.                                                 
460102                                                                          
460200 77  IDPGM                   PIC X(8)      VALUE 'W5129000'.              
460300 77  JA                      PIC X         VALUE 'J'.                     
460400 77  NEJ                     PIC X         VALUE 'N'.                     
460500 77  EOF-W51284              PIC X         VALUE 'N'.                     
460600                                                                          
460900 01  WS-KDPSLLOC             PIC S9(2)     VALUE +0  COMP-3.              
460901 01  WS-STOCK-KURANS         PIC S9(11)    VALUE +0  COMP-3.              
460910 01  WS-IDDC                 PIC X(2)      VALUE SPACES.                  
460920 01  WS-IDARTNR              PIC S9(9)     VALUE +0  COMP-3.              
461000 01  TOT-INKRES              PIC S9(11)V99 VALUE +0  COMP-3.              
461100 01  RAD-INKRES              PIC S9(11)V99 VALUE +0  COMP-3.              
461200                                                                          
461300 01  SUBPROGRAM.                                                          
461400     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
461500     03  WISOLAND            PIC X(8)    VALUE 'WISOLAND'.                
461600                                                                          
461700 01  W51290-TRANSID.                                                      
461800     03  FILLER              PIC X(6) VALUE 'W51290'.                     
461900     03  FILLER              PIC X(8) VALUE 'W51290D1'.                   
462000     03  FILLER              PIC X(4) VALUE '4311'.                       
462100                                                                          
462200     EJECT                                                                
462300*   -COPY W0005  -PRE POSTSUM-                                            
462400     EJECT                                                                
462500*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
462600*                                                                         
462700 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
462800     SKIP3                                                                
462900*01 -COPY WISOLAND                                                        
463000                                                                          
463100 01  W-IDLAND-CURR           PIC X(2)    VALUE SPACE.                     
463200 01  W-IDLAND-X.                                                          
463300     03  W-IDLAND            PIC X(2)    VALUE SPACE.                     
463400*01  -COPY WWDCLAND                                                       
463500                                                                          
463600 01  FILLER                  PIC X(16)   VALUE 'IN-AREA'.                 
463700 01  INAREA.                                                              
463800*    03  -COPY W51284     -PRE IN-                                        
463900     EJECT                                                                
464000                                                                          
464100 01  W001-DAP.                                                            
464200     03  FILLER                  PIC X(165)  VALUE SPACE.                 
464300                                                                          
473400*FOR KGR                                                                  
473500 01  TEXT-AREA2.                                                          
473510     03  MAIN-LINE1.                                                      
473520         05  FILLER          PIC X(7)    VALUE                            
473530             'W51290-'.                                                   
473540         05  RUB1-IDLAND     PIC XX.                                      
473550         05  FILLER          PIC X(1)    VALUE '	'.                       
473560         05  FILLER          PIC X(17)   VALUE                            
473570             'STOCK TURNOVER BY'.                                         
473580         05  FILLER          PIC X(1)    VALUE '	'.                       
473590         05  FILLER          PIC X(16)   VALUE                            
473591             'SHELF-LIFE METOD'.                                          
473592         05  FILLER          PIC X(1)    VALUE '	'.                       
473593         05  FILLER          PIC X(5)    VALUE 'DATE '.                   
473594         05  RUB1-YYYY       PIC 9999.                                    
473595         05  FILLER          PIC X(1)    VALUE '-'.                       
473596         05  RUB1-MM         PIC 99.                                      
473597         05  FILLER          PIC X(1)    VALUE '-'.                       
473598         05  RUB1-DD         PIC 99.                                      
473599         05  FILLER          PIC X(1)    VALUE '	'.                       
473600         05  FILLER          PIC X(9)    VALUE 'CURRENCY '.               
473601         05  RUB1-KDVALISO   PIC X(3).                                    
473602         05  FILLER          PIC X(1)    VALUE '	'.                       
473603         05  FILLER          PIC X(1)    VALUE '	'.                       
473604                                                                          
473605     03  FILLER-LINE.                                                     
473606         05  FILLER          PIC X(1)    VALUE '	'.                       
473607         05  FILLER          PIC X(1)    VALUE '	'.                       
473608         05  FILLER          PIC X(1)    VALUE '	'.                       
473609         05  FILLER          PIC X(1)    VALUE '	'.                       
473610         05  FILLER          PIC X(1)    VALUE '	'.                       
473611         05  FILLER          PIC X(1)    VALUE '	'.                       
473612                                                                          
473631     03  ROW-LINE2.                                                       
473632         05  FILLER          PIC X(7)   VALUE                             
473633            'PART NO'.                                                    
473634         05  FILLER          PIC X(1)    VALUE ';'.                       
473640         05  FILLER          PIC X(2)    VALUE                            
473650            'DC'.                                                         
473651         05  FILLER          PIC X(1)    VALUE ';'.                       
473700         05  FILLER          PIC X(13)   VALUE                            
473800            'PRODUCT GROUP'.                                              
473810         05  FILLER          PIC X(1)    VALUE ';'.                       
473901         05  FILLER          PIC X(16)   VALUE                            
473902            'PUBLICATION WEEK'.                                           
473903         05  FILLER          PIC X(1)    VALUE ';'.                       
473905         05  FILLER          PIC X(17)   VALUE                            
473906            'SUPERSESSION CODE'.                                          
473907         05  FILLER          PIC X(1)    VALUE ';'.                       
473910         05  FILLER          PIC X(20)   VALUE                            
473911            'PERIOD REQUIREMENTS'.                                        
473912         05  FILLER          PIC X(1)    VALUE ';'.                       
473914         05  FILLER          PIC X(13)   VALUE                            
473915            'TURNOVER CODE'.                                              
473916         05  FILLER          PIC X(1)    VALUE ';'.                       
473918         05  FILLER          PIC X(8)    VALUE                            
473919            'QUANTITY'.                                                   
473920         05  FILLER          PIC X(1)    VALUE ';'.                       
473921         05  FILLER          PIC X(14)   VALUE                            
473922            'ORDERING TOTAL'.                                             
473923         05  FILLER          PIC X(1)    VALUE ';'.                       
473924         05  FILLER          PIC X(12)   VALUE                            
473925            'AVERAGE COST'.                                               
473926         05  FILLER          PIC X(1)    VALUE ';'.                       
474000         05  FILLER          PIC X(17)   VALUE                            
474100            'STOCKVALUE T.GR 1'.                                          
474110         05  FILLER          PIC X(1)    VALUE ';'.                       
474300         05  FILLER          PIC X(17)   VALUE                            
474400            'STOCKVALUE T.GR 2'.                                          
474410         05  FILLER          PIC X(1)    VALUE ';'.                       
474600         05  FILLER          PIC X(17)   VALUE                            
474700            'STOCKVALUE T.GR 3'.                                          
474710         05  FILLER          PIC X(1)    VALUE ';'.                       
474900         05  FILLER          PIC X(17)   VALUE                            
475000            'STOCKVALUE T.GR 4'.                                          
475010         05  FILLER          PIC X(1)    VALUE ';'.                       
475200         05  FILLER          PIC X(17)   VALUE                            
475300            'STOCKVALUE T.GR 5'.                                          
475310         05  FILLER          PIC X(1)    VALUE ';'.                       
475500         05  FILLER          PIC X(17)   VALUE                            
475600            'STOCKVALUE T.GR 6'.                                          
475700                                                                          
475800     03  ROW2.                                                            
475810         05  ROW2-IDARTNR    PIC 9(9).                                    
475811         05  FILLER          PIC X(1)    VALUE ';'.                       
475830         05  ROW2-IDDC       PIC X(2).                                    
475831         05  FILLER          PIC X(1)    VALUE ';'.                       
475900         05  ROW2-PKOD       PIC 99.                                      
475910         05  FILLER          PIC X(1)    VALUE ';'.                       
476001         05  ROW2-TIFINLV    PIC 9(5).                                    
476002         05  FILLER          PIC X(1)    VALUE ';'.                       
476004         05  ROW2-KDERS      PIC 9(2).                                    
476005         05  FILLER          PIC X(1)    VALUE ';'.                       
476007         05  ROW2-KVPB       PIC Z(7)9.9-.                                
476008         05  FILLER          PIC X(1)    VALUE ';'.                       
476010         05  ROW2-KDKG       PIC 9(1).                                    
476011         05  FILLER          PIC X(1)    VALUE ';'.                       
476013         05  ROW2-QTY        PIC Z(10)9-.                                 
476014         05  FILLER          PIC X(1)    VALUE ';'.                       
476015         05  ROW2-SULEVANT-TOT   PIC Z(10)9-.                             
476016         05  FILLER          PIC X(1)    VALUE ';'.                       
476017         05  ROW2-PRAVCOST   PIC Z(9)9.99-.                               
476018         05  FILLER          PIC X(1)    VALUE ';'.                       
476100         05  ROW2-LVALUE1    PIC Z(9)9.99-.                               
476110         05  FILLER          PIC X(1)    VALUE ';'.                       
476300         05  ROW2-LVALUE2    PIC Z(9)9.99-.                               
476310         05  FILLER          PIC X(1)    VALUE ';'.                       
476500         05  ROW2-LVALUE3    PIC Z(9)9.99-.                               
476510         05  FILLER          PIC X(1)    VALUE ';'.                       
476700         05  ROW2-LVALUE4    PIC Z(9)9.99-.                               
476710         05  FILLER          PIC X(1)    VALUE ';'.                       
476900         05  ROW2-LVALUE5    PIC Z(9)9.99-.                               
476910         05  FILLER          PIC X(1)    VALUE ';'.                       
477100         05  ROW2-LVALUE6    PIC Z(9)9.99-.                               
477200                                                                          
477300     03  ROW2-TOTAL.                                                      
477400         05  FILLER          PIC X(5)    VALUE                            
477500            'TOTAL'.                                                      
477501         05  FILLER          PIC X(9)    VALUE ' '.                       
477502         05  FILLER          PIC X(1)    VALUE ';'.                       
477503         05  FILLER          PIC X(2)    VALUE ' '.                       
477504         05  FILLER          PIC X(1)    VALUE ';'.                       
477505         05  FILLER          PIC X(2)    VALUE ' '.                       
477506         05  FILLER          PIC X(1)    VALUE ';'.                       
477507         05  FILLER          PIC X(5)    VALUE ' '.                       
477508         05  FILLER          PIC X(1)    VALUE ';'.                       
477509         05  FILLER          PIC X(2)    VALUE ' '.                       
477510         05  FILLER          PIC X(1)    VALUE ';'.                       
477511         05  FILLER          PIC X(10)   VALUE ' '.                       
477512         05  FILLER          PIC X(1)    VALUE ';'.                       
477513         05  FILLER          PIC X(1)    VALUE ' '.                       
477514         05  FILLER          PIC X(1)    VALUE ';'.                       
477515         05  FILLER          PIC X(1)    VALUE ' '.                       
477516         05  FILLER          PIC X(1)    VALUE ';'.                       
477517         05  FILLER          PIC X(10)   VALUE ' '.                       
477518         05  FILLER          PIC X(1)    VALUE ';'.                       
477519         05  FILLER          PIC X(10)   VALUE ' '.                       
477520         05  FILLER          PIC X(1)    VALUE ';'.                       
477700         05  ROW2T-LVALUE1   PIC Z(12)9.99-.                              
477710         05  FILLER          PIC X(1)    VALUE ';'.                       
477900         05  ROW2T-LVALUE2   PIC Z(12)9.99-.                              
477910         05  FILLER          PIC X(1)    VALUE ';'.                       
478100         05  ROW2T-LVALUE3   PIC Z(12)9.99-.                              
478110         05  FILLER          PIC X(1)    VALUE ';'.                       
478300         05  ROW2T-LVALUE4   PIC Z(12)9.99-.                              
478310         05  FILLER          PIC X(1)    VALUE ';'.                       
478500         05  ROW2T-LVALUE5   PIC Z(12)9.99-.                              
478510         05  FILLER          PIC X(1)    VALUE ';'.                       
478700         05  ROW2T-LVALUE6   PIC Z(12)9.99-.                              
478800                                                                          
478900     03  ROW-OBSOL.                                                       
479000         05  FILLER          PIC X(12)   VALUE                            
479100            'OBSOLESCENCE'.                                               
479110         05  FILLER          PIC X(1)    VALUE ';'.                       
479300         05  ROW3T-LVALUE1   PIC Z(12)9.99-.                              
479310         05  FILLER          PIC X(1)    VALUE ';'.                       
479500         05  ROW3T-LVALUE2   PIC Z(12)9.99-.                              
479510         05  FILLER          PIC X(1)    VALUE ';'.                       
479700         05  ROW3T-LVALUE3   PIC Z(12)9.99-.                              
479710         05  FILLER          PIC X(1)    VALUE ';'.                       
479900         05  ROW3T-LVALUE4   PIC Z(12)9.99-.                              
479910         05  FILLER          PIC X(1)    VALUE ';'.                       
480100         05  ROW3T-LVALUE5   PIC Z(12)9.99-.                              
480110         05  FILLER          PIC X(1)    VALUE ';'.                       
480300         05  ROW3T-LVALUE6   PIC Z(12)9.99-.                              
480400                                                                          
480500     03  ROWT-OBSOL.                                                      
480600         05  FILLER          PIC X(9)   VALUE                             
480700            'TOTAL OBS'.                                                  
480710         05  FILLER          PIC X(1)    VALUE ';'.                       
480900         05  ROW4T-LVALUE1   PIC Z(12)9.99-.                              
480910         05  FILLER          PIC X(1)    VALUE ';'.                       
481100         05  ROW4T-PROC      PIC Z(2)9.9-.                                
481110         05  FILLER          PIC X(1)    VALUE ';'.                       
481300         05  FILLER          PIC X(10)   VALUE                            
481400            '% OF TOTAL'.                                                 
481410         05  FILLER          PIC X(1)    VALUE ';'.                       
481420         05  ROW4T-TVALUE    PIC Z(12)9.99-.                              
481430         05  FILLER          PIC X(1)    VALUE ';'.                       
481701                                                                          
481710     03  ROW-SUPERTOTAL.                                                  
481720         05  FILLER          PIC X(17)   VALUE                            
481730            'TOTAL IN TURNOVER'.                                          
481741         05  FILLER          PIC X(1)    VALUE ';'.                       
481750         05  ROWS-LVALUE     PIC Z(12)9.99-.                              
481760         05  FILLER          PIC X(1)    VALUE ';'.                       
481770         05  FILLER          PIC X(1)    VALUE '	'.                       
481780         05  FILLER          PIC X(1)    VALUE '	'.                       
481790         05  FILLER          PIC X(1)    VALUE '	'.                       
481791         05  FILLER          PIC X(1)    VALUE '	'.                       
481793                                                                          
481800     EJECT                                                                
481900                                                                          
483100 01  FILLER                  PIC X(16)   VALUE 'W-KURTAB   '.             
483200 01  W-KURTAB.                                                            
483300     03  W-KUR1              PIC S9(11)V99  COMP-3 VALUE ZERO.            
483400     03  W-KUR2              PIC S9(11)V99  COMP-3 VALUE ZERO.            
483500     03  W-KUR3              PIC S9(11)V99  COMP-3 VALUE ZERO.            
483600     03  W-KUR4              PIC S9(11)V99  COMP-3 VALUE ZERO.            
483700     03  W-KUR5              PIC S9(11)V99  COMP-3 VALUE ZERO.            
483800     03  W-KUR6              PIC S9(11)V99  COMP-3 VALUE ZERO.            
483900     03  W-KURSUM            PIC S9(11)V99  COMP-3 VALUE ZERO.            
484000     03  WS-KURSUM2          PIC S9(11)V99  COMP-3 VALUE ZERO.            
484100                                                                          
484200 01  FILLER                  PIC X(16)   VALUE 'WS-KURTAB  '.             
484300 01  WS-KURTAB.                                                           
484400     03  WS-KUR1             PIC S9(11)V99  COMP-3 VALUE ZERO.            
484500     03  WS-KUR2             PIC S9(11)V99  COMP-3 VALUE ZERO.            
484600     03  WS-KUR3             PIC S9(11)V99  COMP-3 VALUE ZERO.            
484700     03  WS-KUR4             PIC S9(11)V99  COMP-3 VALUE ZERO.            
484800     03  WS-KUR5             PIC S9(11)V99  COMP-3 VALUE ZERO.            
484900     03  WS-KUR6             PIC S9(11)V99  COMP-3 VALUE ZERO.            
485000     03  WS-KURSUM           PIC S9(11)V99  COMP-3 VALUE ZERO.            
485100                                                                          
485200 01  FILLER                  PIC X(16)   VALUE 'WO-KURTAB  '.             
485300 01  WO-KURTAB.                                                           
485400     03  WO-KUR1             PIC S9(11)V99 COMP-3 VALUE ZERO.             
485500     03  WO-KUR2             PIC S9(11)V99 COMP-3 VALUE ZERO.             
485600     03  WO-KUR3             PIC S9(11)V99 COMP-3 VALUE ZERO.             
485700     03  WO-KUR4             PIC S9(11)V99 COMP-3 VALUE ZERO.             
485800     03  WO-KUR5             PIC S9(11)V99 COMP-3 VALUE ZERO.             
485900     03  WO-KUR6             PIC S9(11)V99 COMP-3 VALUE ZERO.             
486000     03  WO-KURSUM           PIC S9(11)V99 COMP-3 VALUE ZERO.             
486100                                                                          
486200 PROCEDURE DIVISION.                                                      
486300                                                                          
486400 MAIN SECTION.                                                            
486500                                                                          
486600     PERFORM A-INIT                                                       
486700                                                                          
486800     PERFORM S01-READ-W51284-POST                                         
486900     IF EOF-W51284 = JA                                                   
487000        CONTINUE                                                          
487100     ELSE                                                                 
487200        PERFORM B-CREATE-SH-O-KGR-POST                                    
487300     END-IF                                                               
487400                                                                          
487500     PERFORM Z-END                                                        
487600                                                                          
487700     MOVE ZERO TO RETURN-CODE                                             
487800     GOBACK                                                               
487900     .                                                                    
488000     EJECT                                                                
488100                                                                          
488200 A-INIT SECTION.                                                          
488300     OPEN INPUT  W51284                                                   
488400          OUTPUT W51290K                                                  
488500                                                                          
488600     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
488700                                                                          
488800     MOVE FUNCTION CURRENT-DATE(1:4) TO RUB1-YYYY                         
488900     MOVE FUNCTION CURRENT-DATE(5:2) TO RUB1-MM                           
489000     MOVE FUNCTION CURRENT-DATE(7:2) TO RUB1-DD                           
489100     .                                                                    
489200     EJECT                                                                
489300                                                                          
489400 B-CREATE-SH-O-KGR-POST SECTION.                                          
489500                                                                          
489602     MOVE SPACE        TO W-IDLAND-CURR                                   
489603                          WS-IDDC                                         
489800     MOVE ZEROS        TO WS-IDARTNR                                      
490000     MOVE ZERO         TO WS-KDPSLLOC                                     
490100                                                                          
490200     PERFORM UNTIL EOF-W51284 = JA                                        
490300                                                                          
490302       IF IN-IDDC NOT = WS-IDDC                                           
490402          PERFORM S10-SOK-IDLAND                                          
490502       END-IF                                                             
490506                                                                          
490602       IF W-IDLAND NOT = W-IDLAND-CURR                                    
490603        IF WS-IDDC NOT = SPACE                                            
490802          PERFORM BG-WRITE-TOTAL                                          
490902          PERFORM BH-COMPUTE-OBSOLITE                                     
491002          PERFORM BI-WRITE-SUPERTOTAL                                     
491100        END-IF                                                            
491202          MOVE IN-IDDC     TO WS-IDDC                                     
491302          MOVE W-IDLAND    TO W-IDLAND-CURR                               
491400          MOVE IN-KDPSLLOC TO WS-KDPSLLOC                                 
491410          MOVE IN-IDARTNR  TO WS-IDARTNR                                  
491500          PERFORM S04-SKRIV-DAP1-K                                        
491600          PERFORM S05-SKRIV-DAP2-K                                        
491700          PERFORM BD-HEADER-KGR                                           
491804       END-IF                                                             
491805       MOVE IN-IDDC     TO WS-IDDC                                        
491806       MOVE W-IDLAND    TO W-IDLAND-CURR                                  
491808                                                                          
491900       IF WS-KDPSLLOC = IN-KDPSLLOC AND                                   
491901          WS-IDARTNR  = IN-IDARTNR AND                                    
491902          W-IDLAND    = W-IDLAND-CURR                                     
492100         PERFORM BE-COMPUTE-STOCKVALUE                                    
492110         PERFORM BF-WRITE-KGR-POST                                        
492200       ELSE                                                               
492300         PERFORM BE-COMPUTE-STOCKVALUE                                    
492400         PERFORM BF-WRITE-KGR-POST                                        
492500         MOVE IN-KDPSLLOC  TO WS-KDPSLLOC                                 
492501         MOVE IN-IDARTNR   TO WS-IDARTNR                                  
492502         MOVE W-IDLAND     TO W-IDLAND-CURR                               
492700       END-IF                                                             
492800       PERFORM S01-READ-W51284-POST                                       
492900     END-PERFORM                                                          
493000                                                                          
493200     PERFORM BG-WRITE-TOTAL                                               
493300     PERFORM BH-COMPUTE-OBSOLITE                                          
493400     PERFORM BI-WRITE-SUPERTOTAL                                          
493500     .                                                                    
493600     EJECT                                                                
493700                                                                          
495600 BD-HEADER-KGR SECTION.                                                   
495700                                                                          
495800     MOVE W-IDLAND-CURR TO RUB1-IDLAND                                    
495900                           LAND-IDLANDX2                                  
496000     MOVE SPACE         TO LAND-IDLANDX3                                  
496100     CALL WISOLAND USING LAND-WISOLAND                                    
496200     IF LAND-KDSVAR = SPACE                                               
496300        MOVE LAND-KDVALISO(1)  TO RUB1-KDVALISO                           
496400     ELSE                                                                 
496500        MOVE SPACE             TO RUB1-KDVALISO                           
496600     END-IF                                                               
496700                                                                          
496800     WRITE W51290-K-REC FROM MAIN-LINE1                                   
496900     WRITE W51290-K-REC FROM FILLER-LINE                                  
497000     WRITE W51290-K-REC FROM ROW-LINE2                                    
497102     WRITE W51290-K-REC FROM FILLER-LINE                                  
497200     .                                                                    
497300                                                                          
497400                                                                          
497500 BE-COMPUTE-STOCKVALUE SECTION.                                           
497600     COMPUTE W-KUR1 = (IN-PRAVCOST * IN-STOCK-KURANS1)                    
497800     COMPUTE W-KUR2 = (IN-PRAVCOST * IN-STOCK-KURANS2)                    
498000     COMPUTE W-KUR3 = (IN-PRAVCOST * IN-STOCK-KURANS3)                    
498200     COMPUTE W-KUR4 = (IN-PRAVCOST * IN-STOCK-KURANS4)                    
498400     COMPUTE W-KUR5 = (IN-PRAVCOST * IN-STOCK-KURANS5)                    
498600     COMPUTE W-KUR6 = (IN-PRAVCOST * IN-STOCK-KURANS6)                    
498610     COMPUTE WS-STOCK-KURANS = IN-STOCK-KURANS1 +                         
498620                               IN-STOCK-KURANS2 +                         
498630                               IN-STOCK-KURANS3 +                         
498640                               IN-STOCK-KURANS4 +                         
498650                               IN-STOCK-KURANS5 +                         
498660                               IN-STOCK-KURANS6                           
498800     .                                                                    
498900     EJECT                                                                
499000                                                                          
499100 BF-WRITE-KGR-POST  SECTION.                                              
499200*TOTAL TURNOVERVALUE PER PRODUCT GROUP                                    
499300     MOVE IN-IDARTNR      TO ROW2-IDARTNR                                 
499310     MOVE IN-KDPSLLOC     TO ROW2-PKOD                                    
499320     MOVE IN-IDDC         TO ROW2-IDDC                                    
499321     MOVE IN-TIFINLV      TO ROW2-TIFINLV                                 
499322     MOVE IN-KDERS        TO ROW2-KDERS                                   
499323     MOVE IN-KVPB         TO ROW2-KVPB                                    
499324     MOVE IN-KDKG         TO ROW2-KDKG                                    
499325     MOVE WS-STOCK-KURANS TO ROW2-QTY                                     
499326     MOVE IN-SULEVANT-TOT TO ROW2-SULEVANT-TOT                            
499330     MOVE IN-PRAVCOST     TO ROW2-PRAVCOST                                
499400     MOVE W-KUR1 TO ROW2-LVALUE1                                          
499500     MOVE W-KUR2 TO ROW2-LVALUE2                                          
499600     MOVE W-KUR3 TO ROW2-LVALUE3                                          
499700     MOVE W-KUR4 TO ROW2-LVALUE4                                          
499800     MOVE W-KUR5 TO ROW2-LVALUE5                                          
499900     MOVE W-KUR6 TO ROW2-LVALUE6                                          
500000     COMPUTE W-KURSUM = W-KUR1 + W-KUR2 + W-KUR3 + W-KUR4 + W-KUR5        
500100                      + W-KUR6                                            
500120     IF W-KURSUM NOT = 0                                                  
500200      WRITE W51290-K-REC FROM ROW2                                        
500300      COMPUTE WS-KUR1 = WS-KUR1 + W-KUR1                                  
500400      COMPUTE WS-KUR2 = WS-KUR2 + W-KUR2                                  
500500      COMPUTE WS-KUR3 = WS-KUR3 + W-KUR3                                  
500600      COMPUTE WS-KUR4 = WS-KUR4 + W-KUR4                                  
500700      COMPUTE WS-KUR5 = WS-KUR5 + W-KUR5                                  
500800      COMPUTE WS-KUR6 = WS-KUR6 + W-KUR6                                  
501010     END-IF                                                               
501900     .                                                                    
502000 BG-WRITE-TOTAL   SECTION.                                                
502100                                                                          
502200     MOVE WS-KUR1 TO ROW2T-LVALUE1                                        
502300     MOVE WS-KUR2 TO ROW2T-LVALUE2                                        
502400     MOVE WS-KUR3 TO ROW2T-LVALUE3                                        
502500     MOVE WS-KUR4 TO ROW2T-LVALUE4                                        
502600     MOVE WS-KUR5 TO ROW2T-LVALUE5                                        
502700     MOVE WS-KUR6 TO ROW2T-LVALUE6                                        
502710     COMPUTE WS-KURSUM2 = WS-KURSUM2 + WS-KUR1 + WS-KUR2 +                
502720                          WS-KUR3 +  WS-KUR4 + WS-KUR5 + WS-KUR6          
502800     WRITE W51290-K-REC FROM ROW2-TOTAL                                   
502900     WRITE W51290-K-REC FROM FILLER-LINE                                  
503000     .                                                                    
503100     SKIP3                                                                
503200                                                                          
503300 BH-COMPUTE-OBSOLITE   SECTION.                                           
503400                                                                          
503500     COMPUTE WO-KUR1   = WS-KUR1 * 0 / 100                                
503600     COMPUTE WO-KUR2   = WS-KUR2 * 0   / 100                              
503700     COMPUTE WO-KUR3   = WS-KUR3 * 18  / 100                              
503800     COMPUTE WO-KUR4   = WS-KUR4 * 53  / 100                              
503900     COMPUTE WO-KUR5   = WS-KUR5 * 81  / 100                              
504000     COMPUTE WO-KUR6   = WS-KUR6 * 100 / 100                              
504100     COMPUTE WO-KURSUM = WO-KUR1 + WO-KUR2 + WO-KUR3 + WO-KUR4 +          
504200                         WO-KUR5 + WO-KUR6                                
504300     MOVE WO-KUR1   TO ROW3T-LVALUE1                                      
504400     MOVE WO-KUR2   TO ROW3T-LVALUE2                                      
504500     MOVE WO-KUR3   TO ROW3T-LVALUE3                                      
504600     MOVE WO-KUR4   TO ROW3T-LVALUE4                                      
504700     MOVE WO-KUR5   TO ROW3T-LVALUE5                                      
504800     MOVE WO-KUR6   TO ROW3T-LVALUE6                                      
504900     MOVE WO-KURSUM TO ROW4T-LVALUE1                                      
505001     MOVE WS-KURSUM2 TO ROW4T-TVALUE                                      
505100     IF WS-KURSUM2 = ZERO                                                 
505200     OR WO-KURSUM = ZERO                                                  
505300       MOVE ZERO TO ROW4T-PROC                                            
505400     ELSE                                                                 
505500       COMPUTE ROW4T-PROC = 100 * WO-KURSUM / WS-KURSUM2                  
505600     END-IF                                                               
505700     .                                                                    
505800     EJECT                                                                
505900                                                                          
506000 BI-WRITE-SUPERTOTAL SECTION.                                             
506100                                                                          
506200     WRITE W51290-K-REC FROM ROW-OBSOL                                    
506300     WRITE W51290-K-REC FROM FILLER-LINE                                  
506400     WRITE W51290-K-REC FROM ROWT-OBSOL                                   
506500     WRITE W51290-K-REC FROM FILLER-LINE                                  
506600     MOVE WS-KURSUM2 TO ROWS-LVALUE                                       
506700     WRITE W51290-K-REC FROM ROW-SUPERTOTAL                               
506720     MOVE ZEROS   TO ROW2T-LVALUE1                                        
506730                     ROW2T-LVALUE2                                        
506740                     ROW2T-LVALUE3                                        
506750                     ROW2T-LVALUE4                                        
506760                     ROW2T-LVALUE5                                        
506770                     ROW2T-LVALUE6                                        
506771                     ROW2-LVALUE1                                         
506772                     ROW2-LVALUE2                                         
506773                     ROW2-LVALUE3                                         
506774                     ROW2-LVALUE4                                         
506775                     ROW2-LVALUE5                                         
506776                     ROW2-LVALUE6                                         
506777                     ROW3T-LVALUE1                                        
506778                     ROW3T-LVALUE2                                        
506779                     ROW3T-LVALUE3                                        
506780                     ROW3T-LVALUE4                                        
506781                     ROW3T-LVALUE5                                        
506782                     ROW3T-LVALUE6                                        
506783                     ROW4T-LVALUE1                                        
506784                     ROW4T-TVALUE                                         
506790                     WS-KUR1                                              
506791                     WS-KUR2                                              
506792                     WS-KUR3                                              
506793                     WS-KUR4                                              
506794                     WS-KUR5                                              
506795                     WS-KUR6                                              
506796                     WO-KUR1                                              
506797                     WO-KUR2                                              
506798                     WO-KUR3                                              
506799                     WO-KUR4                                              
506800                     WO-KUR5                                              
506801                     WO-KUR6                                              
506802                     WS-KURSUM2                                           
506803                     WO-KURSUM                                            
506804                     WS-KURSUM2                                           
506805                     W-KUR1                                               
506806                     W-KUR2                                               
506807                     W-KUR3                                               
506808                     W-KUR4                                               
506809                     W-KUR5                                               
506810                     W-KUR6                                               
506811                     WS-STOCK-KURANS                                      
506820     .                                                                    
506900     EJECT                                                                
507000                                                                          
507100 Z-END    SECTION.                                                        
507200     CLOSE W51284                                                         
507300           W51290K                                                        
507400                                                                          
507500     MOVE 'S'        TO POSTSUM-OPKOD                                     
507600     CALL POSTSUM USING POSTSUM-PARM                                      
507700     .                                                                    
507800     EJECT                                                                
507900                                                                          
508000 S01-READ-W51284-POST SECTION.                                            
508100     READ W51284 INTO INAREA                                              
508200     AT END                                                               
508300       MOVE JA TO EOF-W51284                                              
508400     NOT AT END                                                           
508500       MOVE W51290-TRANSID TO POSTSUM-TRANSID                             
508600       CALL POSTSUM USING POSTSUM-PARM                                    
508700     END-READ                                                             
508800     .                                                                    
508900     EJECT                                                                
509000 S04-SKRIV-DAP1-K SECTION.                                                
509100                                                                          
509200     MOVE ' ¤DAPW51290-001' TO W001-DAP                                   
509300     WRITE W51290-K-REC  FROM W001-DAP                                    
509400                                                                          
509500     MOVE SPACE TO W001-DAP                                               
509600     .                                                                    
509700 S05-SKRIV-DAP2-K SECTION.                                                
509800                                                                          
509900     STRING ' ¤DAP' W-IDLAND-CURR                                         
510000            DELIMITED BY SIZE INTO W001-DAP                               
510100     WRITE W51290-K-REC  FROM W001-DAP                                    
510200                                                                          
510300     MOVE SPACE TO W001-DAP                                               
510400     .                                                                    
510500 S10-SOK-IDLAND SECTION.                                                  
510600                                                                          
510700     SEARCH ALL DC-LAND                                                   
510800        AT END                                                            
510900           MOVE SPACE          TO W-IDLAND                                
511000        WHEN DCLAND-IDDC (DCLAND-IX) = IN-IDDC                            
511100           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
511200                               TO W-IDLAND                                
511300     END-SEARCH                                                           
511400     .                                                                    
