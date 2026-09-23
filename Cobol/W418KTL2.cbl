000010*COMPOPT STDSUB=YES                                                       
000020 ID DIVISION.                                                             
000040 PROGRAM-ID.     W418KTL2.                                                
000050 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000060 DATE-WRITTEN.   95/07/07.                                                
000061 DATE-COMPILED.                                                           
000070                                                                          
000080*                                                                         
000090*    FUNKTION:                                                            
000100*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W41810                         
000200*                                                                         
000300*        KONTROLL AV BEKRÄFTELSEPOSTER FRÅN VIPS                          
000500*                                                                         
000900                                                                          
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200                                                                          
001300 DATA DIVISION.                                                           
001400                                                                          
001500 WORKING-STORAGE SECTION.                                                 
001600                                                                          
001601*    -COPY WY2000W1                                                       
001610     SKIP3                                                                
001700 77  FELTEXT-STR                 PIC X(75)   VALUE SPACE.                 
001710 77  IDPGM                       PIC X(08)   VALUE 'W418KTL2'.            
001711 77  JA                          PIC X       VALUE 'J'.                   
001712 77  NEJ                         PIC X       VALUE 'N'.                   
001713 77  DAGENS-DATUM                PIC  9(6)   VALUE ZERO.                  
001714 77  INDX                        PIC S9(3)   VALUE +0    COMP-3.          
001715 77  MAX-INDX                    PIC S9(3)   VALUE +50   COMP-3.          
001720                                                                          
001721 77  ALLT-SW                     PIC X       VALUE 'J'.                   
001722     88  ALLT-OK                             VALUE 'J'.                   
001724                                                                          
001725 77  WDB201-SW                 PIC X       VALUE 'J'.                     
001726     88  WDB201-FINNS                      VALUE 'J'.                     
001727     88  WDB201-SAKNAS                     VALUE 'N'.                     
001728                                                                          
001733 77  WLKREE11-SW                 PIC X       VALUE 'J'.                   
001734     88  WLKREE11-FINNS                      VALUE 'J'.                   
001735     88  WLKREE11-SAKNAS                     VALUE 'N'.                   
001736                                                                          
001800     EJECT                                                                
001801                                                                          
001808 01  DYNAMISKA-SUBPROGRAM.                                                
001809*                                                                         
001810     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
001811     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
001812     SKIP2                                                                
001813                                                                          
001820 01  NYCKLAR-TILL-DLI.                                                    
001822                                                                          
001835   03  W-IDGMT-X.                                                         
001836     05  W-IDDISTR-WDB2          PIC S9(5)    COMP-3.                     
001837     05  W-IDKUNDNR-WDB2         PIC S9(7)    COMP-3.                     
001844                                                                          
001845    03 W-IDGMT-MIN-X.                                                     
001846       05 W-IDDISTR-WDB2-MIN     PIC S9(5)    COMP-3.                     
001847       05 W-IDKUNDNR-WDB2-MIN    PIC S9(7)    COMP-3.                     
001848                                                                          
001849    03 W-IDGMT-MAX-X.                                                     
001850       05 W-IDDISTR-WDB2-MAX     PIC S9(5)    COMP-3.                     
001851       05 W-IDKUNDNR-WDB2-MAX    PIC S9(7)    COMP-3.                     
001852                                                                          
001853   03 W-IDLEVANM-X.                                                       
001854     05 W-IDDISTR-WDA2           PIC S9(5)    COMP-3.                     
001855     05 W-IDKUNDNR-WDA2          PIC S9(7)    COMP-3.                     
001856     05 W-IDRAPPNR-WDA2          PIC  X(7).                               
001857                                                                          
001858   03 W-WDA211KY-X.                                                       
001859     05 W-IDARTNR                PIC S9(9)    COMP-3.                     
001860     05 W-IDRADNR                PIC S9(5)    COMP-3.                     
001861                                                                          
001862     EJECT                                                                
001863                                                                          
001864*    --- STATUS-KOD FRÅN IMS                                              
001865 01  STATUS-WS                    PIC XX.                                 
001866     88  SEGMENT-FINNS                       VALUE '  '.                  
001867     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
001868     SKIP2                                                                
001869 01  GODK-STATUSKODER.                                                    
001870     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
001871     SKIP3                                                                
001872                                                                          
001873 01  SSA1                        PIC X(64).                               
001874 01  SSA2                        PIC X(64).                               
001875     EJECT                                                                
001880                                                                          
001888*    --- IMS FUNKTIONSKODER                                               
001889*01  -COPY W0003                                                          
001890     EJECT                                                                
001891                                                                          
001892*    ---  DLI INPUT-OUTPUT AREA                                           
001893 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
001894   SKIP3                                                                  
001895 01  DLI-IO-AREA-WDA201.                                                  
001896     03  WLKREE11.                                                        
001897*       05 -COPY WDA201.                                                  
001898     EJECT                                                                
001899                                                                          
001900 01  DLI-IO-AREA-WDA211.                                                  
001901     03  WLKREE11.                                                        
001902*       05 -COPY WDA211.                                                  
001903     EJECT                                                                
001904                                                                          
001905 01  DLI-IO-AREA-WDB201.                                                  
001906     03  WDB201.                                                          
001907*       05 -COPY WDB201.                                                  
001908     EJECT                                                                
001909                                                                          
001938 LINKAGE SECTION.                                                         
001939                                                                          
001940*   -COPY W418KTL2               -PRE LINK-.                              
001941                                                                          
001942     EJECT                                                                
001943                                                                          
001952*01  -COPY W0008                 -PRE KREE-.                              
001953         05  WLKREE-KONKAT-NKL   PIC X.                                   
001954     EJECT                                                                
001955                                                                          
001956*01  -COPY W0008                 -PRE WDB2-.                              
001957         05  WDB2-KONKAT-NKL   PIC X.                                     
001958     EJECT                                                                
001960                                                                          
001961 PROCEDURE DIVISION  USING LINK-W418KTL2 KREE-PCB WDB2-PCB.               
001962 STYR SECTION.                                                            
001963                                                                          
001964*    DISPLAY '*** INNE I W418KTL2 !!'                                     
001965                                                                          
001966     PERFORM A-INIT                                                       
001967                                                                          
001968     PERFORM B-LAS-BASER                                                  
001969                                                                          
001970     PERFORM C-SAETT-EV-FELKOD                                            
001971                                                                          
001972     IF ALLT-OK                                                           
001973       MOVE '   '             TO LINK-KDSVAR                              
001974     ELSE                                                                 
001975       MOVE 'F'               TO LINK-KDSVAR                              
001976     END-IF                                                               
001977                                                                          
001978*    DISPLAY '*** FÄRDIG MED W418KTL2 !!'                                 
001979     MOVE ZERO                TO RETURN-CODE                              
001980     GOBACK                                                               
001981     .                                                                    
001982     EJECT                                                                
001983                                                                          
001984 A-INIT SECTION.                                                          
001985*    DISPLAY '*** A-INIT '                                                
001986                                                                          
001987     ACCEPT DAGENS-DATUM      FROM DATE                                   
001988     MOVE LOW-VALUE           TO W-IDGMT-X                                
001989                                 W-IDGMT-MIN-X                            
001990                                 W-IDLEVANM-X                             
001991                                 W-WDA211KY-X                             
001992                                                                          
001993     MOVE HIGH-VALUE          TO W-IDGMT-MAX-X                            
001994     MOVE SPACE               TO LINK-KDSVAR                              
001995     MOVE JA                  TO ALLT-SW                                  
001996                                 WDB201-SW                                
001997                                 WLKREE11-SW                              
001998                                                                          
001999     MOVE +1                  TO INDX                                     
002000     PERFORM UNTIL INDX > MAX-INDX                                        
002001        MOVE SPACE            TO LINK-IDFELKOD(INDX)                      
002002        ADD +1                TO INDX                                     
002003     END-PERFORM                                                          
002004                                                                          
002005*--- FLYTTA NYCKLAR FÖR LÄSNINGAR                                         
002006                                                                          
002007     MOVE LINK-IDDISTR        TO W-IDDISTR-WDB2                           
002008                                 W-IDDISTR-WDB2-MIN                       
002009                                 W-IDDISTR-WDB2-MAX                       
002010                                 W-IDDISTR-WDA2                           
002011     MOVE LINK-IDKUNDNR       TO W-IDKUNDNR-WDB2                          
002012                                 W-IDKUNDNR-WDA2                          
002013     MOVE LINK-IDRAPPNR       TO W-IDRAPPNR-WDA2                          
002014     MOVE LINK-IDARTNR        TO W-IDARTNR                                
002015     MOVE LINK-IDRADNR        TO W-IDRADNR                                
002017     .                                                                    
002018     EJECT                                                                
002019                                                                          
002020 B-LAS-BASER SECTION.                                                     
002021*    DISPLAY '*** B-LAS-BASER '                                           
002022                                                                          
002023     PERFORM BA-LAS-KUNDREGISTRET                                         
002024     PERFORM BB-LAS-LEVANMREGISTRET                                       
002025     .                                                                    
002026     EJECT                                                                
002027 BA-LAS-KUNDREGISTRET SECTION.                                            
002028*    DISPLAY '*** BA-LAS-KUNDREGISTRET'                                   
002029                                                                          
002030*-- KUNDREGISTRET WDB2                                                    
002031                                                                          
002032     PERFORM IMS-GET-WDB201-UNIK                                          
002033     IF SEGMENT-SAKNAS                                                    
002034        PERFORM IMS-GET-WDB201                                            
002035     END-IF                                                               
002037                                                                          
002038     IF SEGMENT-SAKNAS                                                    
002039        MOVE NEJ              TO WDB201-SW                                
002040     ELSE                                                                 
002050        MOVE JA               TO WDB201-SW                                
002076     END-IF                                                               
002077                                                                          
002078     .                                                                    
002079     EJECT                                                                
002080                                                                          
002090 BB-LAS-LEVANMREGISTRET SECTION.                                          
002091*    DISPLAY '*** BB-LAS-LEVANMREGISTRET'                                 
002100     MOVE '*** BB-LAS-LEVANMREGISTRET *** '                               
002101                              TO FELTEXT-STR                              
002102                                                                          
002103*-- KREDITERINGSREGISTRET WDA2                                            
002104                                                                          
002105     PERFORM IMS-GET-WLKREE01                                             
002106     IF SEGMENT-SAKNAS                                                    
002107        MOVE NEJ              TO WLKREE11-SW                              
002108     ELSE                                                                 
002109        PERFORM IMS-GET-WLKREE11                                          
002111        IF SEGMENT-SAKNAS                                                 
002112           MOVE NEJ           TO WLKREE11-SW                              
002113        ELSE                                                              
002114           MOVE JA            TO WLKREE11-SW                              
002115        END-IF                                                            
002116     END-IF                                                               
002117     .                                                                    
002118     EJECT                                                                
002119 C-SAETT-EV-FELKOD SECTION.                                               
002120*    DISPLAY '*** C-SAETT-EV-FELKOD'                                      
002121                                                                          
002122     MOVE +1                  TO INDX                                     
002123                                                                          
002124     PERFORM CA-KOLLA-DISTRIKT-KUND                                       
002125     PERFORM CB-LEVANM                                                    
002129     .                                                                    
002130     EJECT                                                                
002131 CA-KOLLA-DISTRIKT-KUND SECTION.                                          
002132*    DISPLAY '*** CA-KOLLA-DISTRIKT-KUND'                                 
002133                                                                          
002134*--- DISTRIKT OCH/ELLER KUND                                              
002135     IF WDB201-SAKNAS                                                     
002136        MOVE '701'            TO LINK-IDFELKOD(INDX)                      
002137        MOVE NEJ              TO ALLT-SW                                  
002138        ADD +1                TO INDX                                     
002139     ELSE                                                                 
002140        MOVE DAGENS-DATUM   TO TMP1-YYMMDD                                
002141        MOVE GMT-TISTADAT   TO TMP2-YYMMDD                                
002142        PERFORM WY2000P1                                                  
002143        IF  TMP1-YYMMDD < TMP2-YYMMDD                                     
002144           MOVE '702'         TO LINK-IDFELKOD(INDX)                      
002145           MOVE NEJ           TO ALLT-SW                                  
002146           ADD +1             TO INDX                                     
002150        END-IF                                                            
002160     END-IF                                                               
002166     .                                                                    
002170     EJECT                                                                
002180 CB-LEVANM SECTION.                                                       
002181*    DISPLAY '*** CB-LEVANM'                                              
002182                                                                          
002183*--- SKALL  FINNAS PÅ KREDITERING-REGISTRET                               
002193                                                                          
002194     IF WLKREE11-SAKNAS                                                   
002200        MOVE '735'            TO LINK-IDFELKOD(INDX)                      
002300        MOVE NEJ              TO ALLT-SW                                  
002400        ADD +1                TO INDX                                     
002500     END-IF                                                               
002501                                                                          
002502*--- RETUREN FÅR INTE VARA PÅBÖRJAD                                       
002503                                                                          
002504     IF ANM-KDLEVANM > 4                                                  
002505        MOVE '736'            TO LINK-IDFELKOD(INDX)                      
002506        MOVE NEJ              TO ALLT-SW                                  
002507        ADD +1                TO INDX                                     
002508     END-IF                                                               
002510*    DISPLAY '*** INDX = ' INDX                                           
002600     .                                                                    
002700     EJECT                                                                
002800 IMS-GET-WDB201 SECTION.                                                  
003100                                                                          
003200     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
003300                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
003400            DELIMITED BY SIZE INTO SSA1                                   
003500                                                                          
003600     MOVE '  GE' TO GODK-STATUSKODER                                      
003700     CALL CBLTDLI USING                                                   
003800           GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1                            
003900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
004000     PERFORM IMS-STATUSKONTROLL                                           
004100     .                                                                    
004200     EJECT                                                                
004300 IMS-GET-WDB201-UNIK SECTION.                                             
004600                                                                          
004700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
004800            DELIMITED BY SIZE INTO SSA1                                   
004900                                                                          
005000     MOVE '  GE' TO GODK-STATUSKODER                                      
005100     CALL CBLTDLI USING                                                   
005200           GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1                            
005300     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
005400     PERFORM IMS-STATUSKONTROLL                                           
005500     .                                                                    
005600     EJECT                                                                
012603 IMS-GET-WLKREE01 SECTION.                                                
012604*    DISPLAY '*** IMS-GET-WLKREE01'                                       
012605     MOVE '*** IMS-GET-WLKREE01 *** '                                     
012606                              TO FELTEXT-STR                              
012607                                                                          
012608     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
012609            DELIMITED BY SIZE INTO SSA1                                   
012610                                                                          
012614     MOVE '  GE' TO GODK-STATUSKODER                                      
012615     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA-WDA201 SSA1               
012616     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
012617     PERFORM IMS-STATUSKONTROLL                                           
012618     .                                                                    
012620     EJECT                                                                
012621 IMS-GET-WLKREE11 SECTION.                                                
012622*    DISPLAY '*** IMS-GET-WLKREE11'                                       
012623     MOVE '*** IMS-GET-WLKREE01 *** '                                     
012624                              TO FELTEXT-STR                              
012625                                                                          
012629     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
012630            DELIMITED BY SIZE INTO SSA1                                   
012631                                                                          
012632     MOVE '  GE' TO GODK-STATUSKODER                                      
012633     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA-WDA211 SSA1               
012634     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
012640     PERFORM IMS-STATUSKONTROLL                                           
012650     .                                                                    
012660     EJECT                                                                
012690 IMS-STATUSKONTROLL SECTION.                                              
012700     SKIP2                                                                
012800     SET STATUS-IX TO 1                                                   
012900     SEARCH GODK-STATUS                                                   
013000       AT END                                                             
013100         MOVE 'FEL KOD FRÅN IMS' TO FELTEXT-STR                           
013200         CALL FELLOG                                                      
013300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
013400         CONTINUE                                                         
013500     END-SEARCH                                                           
013600     .                                                                    
013610     EJECT                                                                
013700*    -COPY WY2000P1                                                       
