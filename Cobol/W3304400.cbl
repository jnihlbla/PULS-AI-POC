000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3304400.                                                 
000400 AUTHOR.        PETER DAHLÖF.                                             
000500 DATE-WRITTEN.  DECEMBER 1989.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*    INNNEHÅLLER EN PROCEDURSORT OCH EN COBOLSORT                         
001000*    LÄSER FILEN W33043 SOM INNEHÅLLER RESULTAT FÖR P1-URVALEN            
001100*    OCH MATCHAR MED URVALSFILEN                                          
001200*    SUMMERAR OCH HÄMTAR DELBUDGET                                        
001300*    UPPGIFTER SAMT SKRIVER FIL W33045.                                   
001400*                                                                         
001500*                                                                         
001600*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*    --- INFILER:                                                         
002500     SELECT SORTFIL                      ASSIGN TO W33044DS.              
002600     SELECT W33031S                      ASSIGN TO W33044D1.              
002700     SELECT W33043                       ASSIGN TO W33044D2.              
003000     SELECT W33099                       ASSIGN TO W33044D5.              
003100*    --- UTFILER:                                                         
003200     SELECT W33045                       ASSIGN TO W33044D6.              
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 SD  SORTFIL                                                              
003900     RECORDING      V                                                     
004000     SKIP2                                                                
004100 01  SORTERAD-POST331.                                                    
004200   03  SORT-IDUSER                 PIC X(8).                              
004300   03  SORT-DAREGDAT               PIC 9(8).                              
004400   03  SORT-TIREGTID               PIC 9(7).                              
004500   03  SORT-PRODSL                 PIC 9(3).                              
004600   03  SORT-FKNGRP                 PIC 9(5).                              
004700   03  SORT-BEGREPP                PIC 9(5).                              
004800*03  POST -COPY W3303103  -L.                                             
004900     SKIP2                                                                
005000 01  SORTERAD-POST231.                                                    
005100   03  FILLER                      PIC X(36).                             
005200*03  POST -COPY W3303102  -L.                                             
005300     SKIP2                                                                
005400 01  SORTERAD-POST43.                                                     
005500   03  FILLER                      PIC X(36).                             
005600*03  POST -COPY W33045  -L.                                               
005700     EJECT                                                                
005800 FD  W33031S                                                              
005900     LABEL RECORD   STANDARD                                              
006000     RECORDING      V                                                     
006100     BLOCK CONTAINS 0.                                                    
006200     SKIP2                                                                
006300*01  POST -COPY W3303102  -L -PRE I231-.                                  
006400     SKIP2                                                                
006500*01  POST -COPY W3303103  -L -PRE I331-.                                  
006600     SKIP2                                                                
006700*01  POST -COPY W3303104  -L -PRE I431-.                                  
006800 FD  W33043                                                               
006900     LABEL RECORD   STANDARD                                              
007000     RECORDING      F                                                     
007100     BLOCK CONTAINS 0.                                                    
007200     SKIP2                                                                
007300*01  POST -COPY W33045  -L -PRE I43-.                                     
007400     EJECT                                                                
009300 FD  W33099                                                               
009400     LABEL RECORD   STANDARD                                              
009500     RECORDING      F                                                     
009600     BLOCK CONTAINS 0.                                                    
009700     SKIP2                                                                
009800*01  POST -COPY W33099  -L -PRE I99-.                                     
009900     EJECT                                                                
010000 FD  W33045                                                               
010100     LABEL RECORD   STANDARD                                              
010200     RECORDING      V                                                     
010300     BLOCK CONTAINS 0.                                                    
010400     SKIP2                                                                
010500*01  POST -COPY W3303102  -L -PRE U245-.                                  
010600     SKIP2                                                                
010700*01  POST -COPY W3303103  -L -PRE U345-.                                  
010800     SKIP2                                                                
010900*01  POST -COPY W33045    -L -PRE U045-.                                  
011000     EJECT                                                                
011100 WORKING-STORAGE SECTION.                                                 
011200     SKIP2                                                                
011201                                                                          
011210*    -- CHECKED BY WY2000                                                 
011300 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3304400'.            
011400 77  JA                          PIC X(1)    VALUE 'J'.                   
011500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
011600 77  SW-DISTR                    PIC X(1)    VALUE 'N'.                   
011700 77  SW-MARKN                    PIC X(1)    VALUE 'N'.                   
011800 77  SW-KONCNR                   PIC X(1)    VALUE 'N'.                   
011900 77  SW-TRAEFF                   PIC X(1)    VALUE 'N'.                   
012200 77  I43-EOF                     PIC X(1)    VALUE 'N'.                   
012300 77  I31-EOF                     PIC X(1)    VALUE 'N'.                   
012600 77  I99-EOF                     PIC X(1)    VALUE 'N'.                   
012700 77  SORT-EOF                    PIC X(1)    VALUE 'N'.                   
012800 77  IX                          PIC S9(9)   VALUE +1  COMP SYNC.         
012900 77  K-IX                        PIC S9(9)   VALUE +1  COMP SYNC.         
013000 77  M-IX                        PIC S9(9)   VALUE +1  COMP SYNC.         
013100 77  D-IX                        PIC S9(9)   VALUE +1  COMP SYNC.         
013200 77  MAX-TIPP                    PIC S9(4) VALUE +12   COMP SYNC.         
013300 77  MAX-M-IX                    PIC S9(9) VALUE +600  COMP SYNC.         
013400 77  MAX-K-IX                    PIC S9(9) VALUE +6000 COMP SYNC.         
013500 77  MAX-D-IX                    PIC S9(9) VALUE +60000 COMP SYNC.        
013700 77  MAX-I99-IX                  PIC S9(9) VALUE +2000 COMP SYNC.         
013800 77  WS-REFSGSIX                 PIC S9(3)V9(1)   VALUE ZERO.             
013900 77  WS-SUTOTFSG-BUDG            PIC S9(11)V9(2)  VALUE ZERO.             
014000 77  WS-RETOTBV-FRAAR            PIC S9(9)V9(2)  VALUE ZERO.              
014100 77  WS-RETOTBV-RAAR-TB          PIC S9(9)V9(2)  VALUE ZERO.              
014200 77  WS-RETOTBV-RAAR             PIC S9(9)V9(2)  VALUE ZERO.              
014300 77  WS-RETOTBV-AAR              PIC S9(9)V9(2)  VALUE ZERO.              
014400 77  WS-RETOTBV-PER              PIC S9(9)V9(2)  VALUE ZERO.              
014500 77  WS-RETOTBV-RAAR-TG          PIC S9(9)V9(2)  VALUE ZERO.              
014600 77  WS-REFSG-AAR                PIC S9(9)V9(2)  VALUE ZERO.              
014700 77  WS-REFSG-RAAR               PIC S9(9)V9(2)  VALUE ZERO.              
014800 77  WS-RELEVANT-RAAR            PIC S9(9)V9(2)  VALUE ZERO.              
014900 77  WS-ANTAL-URVAL              PIC S9(4)   VALUE +1  COMP SYNC.         
015000 77  WS-IDRADNR                  PIC S9(4)   VALUE +1  COMP SYNC.         
015010 77  SPAR-KDNIVA                 PIC S9(3)    COMP-3.                     
015100 77  ABENDKOD                    PIC S9(4)   VALUE +16 COMP SYNC.         
015200 77  WS-DAGENS-TIPP-MINUS-1      PIC 9(2).                                
015300 77  SPAR-BEGREPP                PIC 9(5).                                
015400 77  SPAR-PRODSL                 PIC 9(3).                                
015500 77  WS-DIFF                     PIC S9(9)V9(2)  VALUE ZERO.              
015600 77  WS-IDFKNGRP                 PIC 9(5).                                
015700 77  SPAR-IDFKNGRP               PIC 9(5).                                
015701                                                                          
018600 01  STRUKTUR-TABELL.                                                     
018700   03  STRUKT-TAB                          OCCURS 2000.                   
018800     05  STRUKT-IDFKNGRP         PIC S9(5) COMP-3.                        
018900     05  STRUKT-BEFKNGRP         PIC X(50).                               
019000     EJECT                                                                
019100*01 -COPY WDATAREA                                                        
019300     EJECT                                                                
019400 01  DYNAMISKA-SUBPROGRAM.                                                
019500     SKIP1                                                                
019600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
019700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
020000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
020100     SKIP2                                                                
020200*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
020300*                                                                         
020400*01  -COPY W0005 -PRE  POSTSUM-.                                          
020600     EJECT                                                                
020700 01  FILLER                       PIC X(16)  VALUE 'I31-FILEN'.           
020800*                                                                         
020900 01  I31-AREA.                                                            
021000   03  I31-IDUSER                     PIC X(8).                           
021100   03  I31-DAREGDAT                   PIC  9(8).                          
021200   03  I31-TIREGTID                   PIC S9(7) COMP-3.                   
021300   03  FILLER                         PIC X(8).                           
021400   03  I31-IDPTYP                     PIC X(3).                           
021500   03  I31-IDGTYP                     PIC S9(1) COMP-3.                   
021600   03  I31-IDTRANS                    PIC X(4).                           
021700   03  I31-IDKONCNR OCCURS 8 TIMES    PIC S9(3) COMP-3.                   
021800   03  I31-MARKNADS-GRP OCCURS 8 TIMES.                                   
021900     05  I31-KDMARK-BUDG-FOM          PIC S9(3) COMP-3.                   
022000     05  I31-KDMARK-BUDG-TOM          PIC S9(3) COMP-3.                   
022100   03  I31-DISTRIKT-GRP OCCURS 4 TIMES.                                   
022200     05  I31-IDDISTR-FOM              PIC S9(5) COMP-3.                   
022300     05  I31-IDDISTR-TOM              PIC S9(5) COMP-3.                   
022400   03  FILLER                         PIC X(2560).                        
022500*01  AREA  -COPY W3303103  -PRE I331- -RED I31-AREA.                      
022700    EJECT                                                                 
022800*01  AREA  -COPY W3303102  -PRE I231- -RED I31-AREA.                      
023000    EJECT                                                                 
023100 01  FILLER                       PIC X(16)  VALUE 'I43-FILEN'.           
023200*01  AREA  -COPY W33045  -PRE I43-.                                       
023400    EJECT                                                                 
025200 01  FILLER                       PIC X(16)  VALUE                        
025300                                          'WS-SORTERAD-AREA'.             
025400 01  WS-SORTERAD-AREA.                                                    
025500   03  WS-SORT-IDUSER              PIC X(8).                              
025600   03  WS-SORT-DAREGDAT            PIC 9(8).                              
025700   03  WS-SORT-TIREGTID            PIC 9(7).                              
025800   03  WS-SORT-PRODSL              PIC 9(3).                              
025900   03  WS-SORT-FKNGRP              PIC 9(5).                              
026000   03  WS-SORT-BEGREPP             PIC 9(5).                              
026100   03  WS-CTEXT.                                                          
026200     05  FILLER                PIC X(31).                                 
026300     05  WS-SORT-IDGTYP        PIC S9(1) COMP-3.                          
026400     05  WS-SORT-IDTRANS       PIC X(4).                                  
026500     05  FILLER                PIC X(2572).                               
026600*03  AREA -COPY W3303103  -PRE SORT331- -RED WS-CTEXT.                    
026800     EJECT                                                                
026900*03  AREA -COPY W3303102  -PRE SORT231- -RED WS-CTEXT.                    
027100     EJECT                                                                
027200*03  AREA -COPY W33045  -PRE SORT43-  -RED WS-CTEXT.                      
027400     EJECT                                                                
027500 01  FILLER                       PIC X(16)  VALUE 'UTFILER'.             
027600*                                                                         
027700*01  AREA  -COPY W3303102  -PRE U245-.                                    
027900    EJECT                                                                 
028000*01  AREA  -COPY W3303103  -PRE U345-.                                    
028200    EJECT                                                                 
028300 01  FILLER                       PIC X(16)  VALUE 'U045-AREA'.           
028400*                                                                         
028500*01  AREA  -COPY W33045  -PRE U045-.                                      
028700     EJECT                                                                
028800*01  AREA  -COPY W33045  -PRE 45NTLZD-.                                   
029000     EJECT                                                                
029100******************************************************************        
029200*   NYCKLAR TILL DLI                                                      
029300******************************************************************        
029400 01  FILLER                      PIC X(16)   VALUE 'DLI-NYCKLAR'.         
029500*                                                                         
029600 01  NYCKLAR-TILL-DLI.                                                    
029700*                               WDG2-FÖRÄLDER                             
029800     03  W-3133-X.                                                        
029900         05   FILLER             PIC X(4)    VALUE '3133'.                
030000         05   FILLER             PIC X(26)   VALUE LOW-VALUE.             
030100     03  W-3134-X.                                                        
030200         05   W-IDSKURVA         PIC S9(3) COMP-3.                        
030300         05   FILLER             PIC X(13)   VALUE LOW-VALUE.             
030400     03  W-3135-X.                                                        
030500         05   FILLER             PIC X(4)    VALUE '3135'.                
030600         05   FILLER             PIC X(26)   VALUE LOW-VALUE.             
030700     03  W-3136-X.                                                        
030800         05   W-KDMARK-BUDG      PIC S9(3) COMP-3.                        
030900         05   W-KDPRODSL         PIC S9(3) COMP-3.                        
031000         05   W-IDFKNGRP         PIC S9(5) COMP-3.                        
031100******************************************************************        
031200*                                                                         
031300*          ARBETSAREOR TILL IMS-SEKTIONERNA                               
031400*                                                                         
031500*01  IMS-WS.                                                              
031600     03  FILLER                  PIC X(16) VALUE 'IMS-WS     '.           
031700     SKIP3                                                                
031800*         STATUSKOD FRÅN IMS                                              
031900     03  STATUS-WS               PIC XX.                                  
032000       88  SEGMENT-FINNS                   VALUE '  '.                    
032100       88  SEGMENT-SAKNAS                  VALUE 'GE'.                    
032200     SKIP2                                                                
032300     03  GODK-STATUSKODER.                                                
032400       05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
032500     SKIP2                                                                
032600 01  SSA1                        PIC X(64).                               
032700 01  SSA2                        PIC X(64).                               
032800     EJECT                                                                
032900*--------------------IMS FUNKTIONSKODER                                   
033000*01  -COPY W0003                                                          
033200     EJECT                                                                
033300 01  FILLER                       PIC X(16)   VALUE                       
033400                                            'DLI-IO-AREA    '.            
033500*--------------------DLI INPUT-OUTPUT AREA                                
033600 01  DLI-IO-AREA.                                                         
033700   03  IO-AREA                    PIC X(70).                              
033800*  03  WLXXCH11 -COPY WDGX3134 -RED IO-AREA.                              
034000                                                                          
034100*  03  WLXXCI11 -COPY WDGX3136 -RED IO-AREA.                              
034300 LINKAGE SECTION.                                                         
034400*01    -COPY W0008  -PRE XXCH-                                            
034600     05  FILLER                      PIC X.                               
034700     EJECT                                                                
034800*01    -COPY W0008  -PRE XXCI-                                            
035000     05  FILLER                      PIC X.                               
035100     EJECT                                                                
035200 PROCEDURE DIVISION USING XXCH-PCB XXCI-PCB.                              
035300     ENTRY  'DLITCBL' USING XXCH-PCB XXCI-PCB.                            
035400     SKIP2                                                                
035500 STYR SECTION.                                                            
035600* BÖRJA MED ATT SORTERA URVALSFILEN I PROCEDUREN                          
035700     PERFORM A-INIT                                                       
035800     SORT SORTFIL                                                         
035900        ASCENDING KEY SORT-IDUSER                                         
036000                      SORT-DAREGDAT                                       
036100                      SORT-TIREGTID                                       
036200                      SORT-PRODSL                                         
036300                      SORT-FKNGRP                                         
036400                      SORT-BEGREPP                                        
036500        INPUT PROCEDURE  B-PLOCKA-P-URVAL                                 
036600        OUTPUT PROCEDURE C-BEHANDLA-OCH-SKRIV                             
036700     IF SORT-RETURN = ZERO                                                
036800        PERFORM Z-FINIT                                                   
036900        MOVE ZERO TO RETURN-CODE                                          
037000        GOBACK                                                            
037100     ELSE                                                                 
037200        DISPLAY 'FEL I SORTERINGEN'                                       
037300        CALL ABEND USING ABENDKOD                                         
037400     END-IF                                                               
037500     .                                                                    
037600     EJECT                                                                
037700 A-INIT SECTION.                                                          
037800     SKIP2                                                                
037900     OPEN INPUT  W33031S                                                  
038000                 W33043                                                   
038300                 W33099                                                   
038400     OPEN OUTPUT W33045                                                   
038500     MOVE PROGRAM-NAMN               TO POSTSUM-PROGNAMN                  
038600     MOVE 'AAMMDD'                   TO DAT-KDDATFORM                     
038700     MOVE 276                        TO SORT-MODE-SIZE                    
038800     ACCEPT DAT-I-TIDATUM  FROM DATE                                      
038900     PERFORM S99-CALL-WDATKONV                                            
039000     IF DAT-KDSVAR-OK                                                     
039100        MOVE DAT-TIPP                TO WS-DAGENS-TIPP-MINUS-1            
039200* TIPP STÅR FÖR PLANERINGSPERIOD 12 PER ÅR                                
039300        COMPUTE WS-DAGENS-TIPP-MINUS-1 = WS-DAGENS-TIPP-MINUS-1           
039400                                         - 1                              
039500        IF WS-DAGENS-TIPP-MINUS-1 = ZERO                                  
039600           MOVE MAX-TIPP           TO WS-DAGENS-TIPP-MINUS-1              
039700        END-IF                                                            
039800     END-IF                                                               
039900     INITIALIZE 45NTLZD-AREA                                              
040400     MOVE 45NTLZD-AREA                 TO U045-AREA                       
040500     PERFORM AA-FYLL-STRUKTURTABELL                                       
040600     .                                                                    
040700     EJECT                                                                
040800 AA-FYLL-STRUKTURTABELL     SECTION.                                      
040900     SKIP2                                                                
041000     MOVE +1                          TO IX                               
041100     INITIALIZE STRUKTUR-TABELL                                           
041200     PERFORM S12-LAS-99-FIL                                               
041300     PERFORM UNTIL I99-EOF = JA                                           
041400        IF IX > MAX-I99-IX                                                
041500           DISPLAY 'TABELL FÖR STRUKTUREGISTRET (W33099-FIL ÄR'           
041600           DISPLAY 'FÖR LITEN'                                            
041700           CALL ABEND USING ABENDKOD                                      
041800        END-IF                                                            
041900        ADD +1                        TO IX                               
042000        PERFORM S12-LAS-99-FIL                                            
042100     END-PERFORM                                                          
042200     .                                                                    
042300     EJECT                                                                
042400 B-PLOCKA-P-URVAL    SECTION.                                             
042500     SKIP2                                                                
042600     PERFORM S01-LAS-31-FIL                                               
042700     PERFORM S04-LAS-43-FIL                                               
042800     PERFORM UNTIL I43-EOF = JA AND                                       
042900     I31-EOF = JA                                                         
043000        IF I43-EOF = NEJ                                                  
043100           IF I43-IDUSER   = I31-IDUSER   AND                             
043200           I43-DAREGDAT    = I31-DAREGDAT AND                             
043300           I43-TIREGTID    = I31-TIREGTID AND                             
043400           I31-IDPTYP      = 'P1 ' OR 'PLV' OR 'PPV'                      
043500              PERFORM BA-RELEASE-RESULTAT                                 
043600              PERFORM BB-RELEASE-P1-POST                                  
043700           ELSE                                                           
043800              IF I31-IDPTYP  = 'P1 ' OR 'PLV' OR 'PPV'                    
043900                 MOVE ZERO                TO I31-IDGTYP                   
044000                 PERFORM BB-RELEASE-P1-POST                               
044100              END-IF                                                      
044200           END-IF                                                         
044300        ELSE                                                              
044400           IF I31-IDPTYP  = 'P1 ' OR 'PLV' OR 'PPV'                       
044500              MOVE ZERO                   TO I31-IDGTYP                   
044600              PERFORM BB-RELEASE-P1-POST                                  
044700           END-IF                                                         
044800        END-IF                                                            
044900        PERFORM S01-LAS-31-FIL                                            
045000     END-PERFORM                                                          
045100     .                                                                    
045200     EJECT                                                                
045300 BA-RELEASE-RESULTAT    SECTION.                                          
045400     SKIP2                                                                
045500     IF I31-IDGTYP = +1                                                   
045600        PERFORM BAA-SORTERA-PA-DISTR                                      
045700     ELSE                                                                 
045800        IF I31-IDGTYP = +2                                                
045900           PERFORM BAB-SORTERA-PA-KONCNR                                  
046000        ELSE                                                              
046100           IF I31-IDGTYP = +3                                             
046200              PERFORM BAC-SORTERA-PA-MARKN                                
046300           ELSE                                                           
046400              PERFORM BAA-SORTERA-PA-DISTR                                
046500           END-IF                                                         
046600        END-IF                                                            
046700     END-IF                                                               
046800     .                                                                    
046900     EJECT                                                                
047000 BAA-SORTERA-PA-DISTR       SECTION.                                      
047100     SKIP2                                                                
047200     PERFORM UNTIL                                                        
047300     I43-IDUSER   NOT = I31-IDUSER   OR                                   
047400     I43-DAREGDAT NOT = I31-DAREGDAT OR                                   
047500     I43-TIREGTID NOT = I31-TIREGTID OR                                   
047600     I43-EOF = JA                                                         
047700        MOVE I43-IDUSER               TO WS-SORT-IDUSER                   
047800        MOVE I43-DAREGDAT             TO WS-SORT-DAREGDAT                 
047900        MOVE I43-TIREGTID             TO WS-SORT-TIREGTID                 
048000        MOVE I43-KDPRODSL             TO WS-SORT-PRODSL                   
048100        MOVE I43-IDFKNGRP             TO WS-SORT-FKNGRP                   
048200        MOVE I43-IDDISTR              TO WS-SORT-BEGREPP                  
048300        MOVE I43-AREA                 TO WS-CTEXT                         
048400        RELEASE SORTERAD-POST43 FROM WS-SORTERAD-AREA                     
048500        PERFORM S04-LAS-43-FIL                                            
048600     END-PERFORM                                                          
048700     .                                                                    
048800     EJECT                                                                
048900 BAB-SORTERA-PA-KONCNR      SECTION.                                      
049000     SKIP2                                                                
049100     PERFORM UNTIL                                                        
049200     I43-IDUSER   NOT = I31-IDUSER   OR                                   
049300     I43-DAREGDAT NOT = I31-DAREGDAT OR                                   
049400     I43-TIREGTID NOT = I31-TIREGTID OR                                   
049500     I43-EOF = JA                                                         
049600        MOVE I43-IDUSER               TO WS-SORT-IDUSER                   
049700        MOVE I43-DAREGDAT             TO WS-SORT-DAREGDAT                 
049800        MOVE I43-TIREGTID             TO WS-SORT-TIREGTID                 
049900        MOVE I43-KDPRODSL             TO WS-SORT-PRODSL                   
050000        MOVE I43-IDFKNGRP             TO WS-SORT-FKNGRP                   
050100        MOVE I43-IDKONCNR             TO WS-SORT-BEGREPP                  
050200        MOVE I43-AREA                 TO WS-CTEXT                         
050300        RELEASE SORTERAD-POST43 FROM WS-SORTERAD-AREA                     
050400        PERFORM S04-LAS-43-FIL                                            
050500     END-PERFORM                                                          
050600     .                                                                    
050700     EJECT                                                                
050800 BAC-SORTERA-PA-MARKN       SECTION.                                      
050900     SKIP2                                                                
051000     PERFORM UNTIL                                                        
051100     I43-IDUSER   NOT = I31-IDUSER   OR                                   
051200     I43-DAREGDAT NOT = I31-DAREGDAT OR                                   
051300     I43-TIREGTID NOT = I31-TIREGTID OR                                   
051400     I43-EOF = JA                                                         
051500        MOVE I43-IDUSER               TO WS-SORT-IDUSER                   
051600        MOVE I43-DAREGDAT             TO WS-SORT-DAREGDAT                 
051700        MOVE I43-TIREGTID             TO WS-SORT-TIREGTID                 
051800        MOVE I43-KDPRODSL             TO WS-SORT-PRODSL                   
051900        MOVE I43-IDFKNGRP             TO WS-SORT-FKNGRP                   
052000        MOVE I43-KDMARK-BUDG          TO WS-SORT-BEGREPP                  
052100        MOVE I43-AREA                 TO WS-CTEXT                         
052200        RELEASE SORTERAD-POST43 FROM WS-SORTERAD-AREA                     
052300        PERFORM S04-LAS-43-FIL                                            
052400     END-PERFORM                                                          
052500     .                                                                    
052600     EJECT                                                                
052700 BB-RELEASE-P1-POST       SECTION.                                        
052800     SKIP2                                                                
052900     MOVE I31-IDUSER                  TO WS-SORT-IDUSER                   
053000     MOVE I31-DAREGDAT                TO WS-SORT-DAREGDAT                 
053100     MOVE I31-TIREGTID                TO WS-SORT-TIREGTID                 
053200     MOVE I31-AREA                    TO WS-CTEXT                         
053300     MOVE ZERO                        TO WS-SORT-PRODSL                   
053400                                         WS-SORT-FKNGRP                   
053500                                         WS-SORT-BEGREPP                  
053600     IF I31-IDTRANS = '3202'                                              
053700        RELEASE SORTERAD-POST231 FROM WS-SORTERAD-AREA                    
053800     ELSE                                                                 
053900        RELEASE SORTERAD-POST331 FROM WS-SORTERAD-AREA                    
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300 C-BEHANDLA-OCH-SKRIV     SECTION.                                        
054400     SKIP2                                                                
054600     PERFORM S11-RETURN                                                   
054700     PERFORM UNTIL SORT-EOF = JA                                          
054800        IF WS-SORT-IDGTYP < +5                                            
054900           PERFORM CB-BEHANDLA-URVAL                                      
055000           PERFORM S11-RETURN                                             
055100        ELSE                                                              
055200           MOVE WS-SORT-PRODSL            TO SPAR-PRODSL                  
055300           MOVE WS-SORT-FKNGRP            TO WS-IDFKNGRP                  
055400           MOVE SORT43-001-GRUPP          TO U045-001-GRUPP               
055500           MOVE SORT43-002-GRUPP          TO U045-002-GRUPP               
055600           MOVE WS-SORT-BEGREPP           TO SPAR-BEGREPP                 
055700           MOVE WS-SORT-FKNGRP            TO SPAR-IDFKNGRP                
055800           PERFORM UNTIL WS-SORT-IDGTYP < 5 OR                            
055900           SORT-EOF = JA                                                  
055910*       KOLLA KDNIVÅ OCH LÄGG VALD NIVÅ PÅ FUNKTIONSGRUPP                 
055920*       2 = RESULTAT PER HUNDRANIVÅ + TUSEN NIVÅ                          
055940              IF SPAR-KDNIVA = 2                                          
055956                PERFORM S14-HAEMTA-BEFKNGRP                               
055960              END-IF                                                      
055970*                                                                         
056000              PERFORM CC-EV-SKRIV-45-FIL                                  
056100              PERFORM CD-ADDERA                                           
056200              PERFORM S11-RETURN                                          
056300              MOVE WS-SORT-FKNGRP         TO WS-IDFKNGRP                  
056400           END-PERFORM                                                    
056500           IF SORT-EOF = JA                                               
056600              PERFORM S05-BERAKNA-SKRIV-BEGREPP                           
056700           ELSE                                                           
056800              IF WS-SORT-IDGTYP < +5                                      
056900                 PERFORM S05-BERAKNA-SKRIV-BEGREPP                        
057000              END-IF                                                      
057100           END-IF                                                         
057200        END-IF                                                            
057300     END-PERFORM                                                          
057400     .                                                                    
057500     EJECT                                                                
062800 CB-BEHANDLA-URVAL    SECTION.                                            
062900     SKIP2                                                                
063000     MOVE NEJ                        TO SW-DISTR                          
063100     MOVE NEJ                        TO SW-KONCNR                         
063200     MOVE NEJ                        TO SW-MARKN                          
063300     IF WS-SORT-IDTRANS = '3202'                                          
063310        MOVE SORT231-KDNIVA TO SPAR-KDNIVA                                
063700        WRITE U245-POST FROM SORT231-AREA                                 
063800     ELSE                                                                 
063810        MOVE ZERO TO SPAR-KDNIVA                                          
063900        WRITE U345-POST FROM SORT331-AREA                                 
064000     END-IF                                                               
064100     IF WS-SORT-IDGTYP =  +1                                              
064200        MOVE JA                      TO SW-DISTR                          
064300     ELSE                                                                 
064400        IF WS-SORT-IDGTYP =  +2                                           
064500           MOVE JA                   TO SW-KONCNR                         
064600        ELSE                                                              
064700           IF WS-SORT-IDGTYP =  +3                                        
064800              MOVE JA                TO SW-MARKN                          
064900           END-IF                                                         
065000        END-IF                                                            
065100     END-IF                                                               
065200     PERFORM S08-CALL-POSTSUM-45-FIL                                      
065300     .                                                                    
065400     EJECT                                                                
071600 CC-EV-SKRIV-45-FIL      SECTION.                                         
071700     SKIP2                                                                
071800     IF SPAR-PRODSL = WS-SORT-PRODSL                                      
071900        IF SPAR-IDFKNGRP = WS-IDFKNGRP                                    
072000           IF SPAR-BEGREPP  = WS-SORT-BEGREPP                             
072100              CONTINUE                                                    
072200           ELSE                                                           
072300              PERFORM S05-BERAKNA-SKRIV-BEGREPP                           
072400              MOVE WS-SORT-BEGREPP TO SPAR-BEGREPP                        
072500           END-IF                                                         
072600        ELSE                                                              
072700           PERFORM S05-BERAKNA-SKRIV-BEGREPP                              
072800           MOVE WS-SORT-BEGREPP    TO SPAR-BEGREPP                        
072900           MOVE WS-IDFKNGRP        TO SPAR-IDFKNGRP                       
073000        END-IF                                                            
073100     ELSE                                                                 
073200        PERFORM S05-BERAKNA-SKRIV-BEGREPP                                 
073300        MOVE WS-SORT-BEGREPP       TO SPAR-BEGREPP                        
073400        MOVE WS-IDFKNGRP           TO SPAR-IDFKNGRP                       
073500        MOVE WS-SORT-PRODSL        TO SPAR-PRODSL                         
073600     END-IF                                                               
073700     .                                                                    
073800     EJECT                                                                
073900 CD-ADDERA                  SECTION.                                      
074000     SKIP2                                                                
074100     ADD  SORT43-SUARTFSG-PER    TO U045-SUARTFSG-PER                     
074200     ADD  SORT43-SUARTFSG-AAR    TO U045-SUARTFSG-AAR                     
074300     ADD  SORT43-SUARTFSG-FAAR   TO U045-SUARTFSG-FAAR                    
074400     ADD  SORT43-SUARTFSG-RAAR   TO U045-SUARTFSG-RAAR                    
074500     ADD  SORT43-SUARTFSG-FRAAR  TO U045-SUARTFSG-FRAAR                   
074600                                                                          
074700     ADD  SORT43-SULEVANT-PER    TO U045-SULEVANT-PER                     
074800     ADD  SORT43-SULEVANT-AAR    TO U045-SULEVANT-AAR                     
074900     ADD  SORT43-SULEVANT-FAAR   TO U045-SULEVANT-FAAR                    
075000     ADD  SORT43-SULEVANT-RAAR   TO U045-SULEVANT-RAAR                    
075100     ADD  SORT43-SULEVANT-FRAAR  TO U045-SULEVANT-FRAAR                   
075200                                                                          
075300     ADD  SORT43-SUARTSJK-PER    TO U045-SUARTSJK-PER                     
075400     ADD  SORT43-SUARTSJK-AAR    TO U045-SUARTSJK-AAR                     
075500     ADD  SORT43-SUARTSJK-FAAR   TO U045-SUARTSJK-FAAR                    
075600     ADD  SORT43-SUARTSJK-RAAR   TO U045-SUARTSJK-RAAR                    
075700     ADD  SORT43-SUARTSJK-FRAAR  TO U045-SUARTSJK-FRAAR                   
075800     .                                                                    
075900     EJECT                                                                
076000 S01-LAS-31-FIL    SECTION.                                               
076100     SKIP2                                                                
076200     READ W33031S INTO I31-AREA                                           
076300     AT END                                                               
076400       MOVE JA                    TO I31-EOF                              
076500     END-READ                                                             
076600                                                                          
076700     IF I31-EOF = NEJ                                                     
076800       MOVE '    '                TO POSTSUM-TRANSTYP                     
076900       MOVE 'W33031'              TO POSTSUM-FDNAMN                       
077000       MOVE 'W33044D1'            TO POSTSUM-DDNAMN2                      
077100       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
077200     END-IF                                                               
077300     .                                                                    
077400     EJECT                                                                
080500 S04-LAS-43-FIL   SECTION.                                                
080600     SKIP2                                                                
080700     READ W33043 INTO I43-AREA                                            
080800     AT END                                                               
080900       MOVE JA                    TO I43-EOF                              
081000     END-READ                                                             
081100                                                                          
081200     IF I43-EOF = NEJ                                                     
081300       MOVE '    '                TO POSTSUM-TRANSTYP                     
081400       MOVE 'W33043'              TO POSTSUM-FDNAMN                       
081500       MOVE 'W33044D2'            TO POSTSUM-DDNAMN2                      
081600       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000 S05-BERAKNA-SKRIV-BEGREPP     SECTION.                                   
082100     SKIP2                                                                
082200     IF U045-IDGTYP = +5                                                  
082300*    AF2 TB           ***                                                 
082400     SKIP2                                                                
082500        COMPUTE U045-SUTOTBV-RAAR ROUNDED =                               
082600                                    U045-SUARTFSG-RAAR -                  
082700                                    U045-SUARTSJK-RAAR                    
082800*    AF5 TB           ***                                                 
082900     SKIP2                                                                
083000        COMPUTE U045-SUTOTBV-PER ROUNDED = U045-SUARTFSG-PER -            
083100                                          U045-SUARTSJK-PER               
083200*    AF2/AF1 TB       ***                                                 
083300     SKIP2                                                                
083400        COMPUTE WS-DIFF = U045-SUARTFSG-FRAAR -                           
083500                          U045-SUARTSJK-FRAAR                             
083600        IF WS-DIFF = ZERO                                                 
083700           CONTINUE                                                       
083800        ELSE                                                              
083900           COMPUTE WS-RETOTBV-RAAR-TB ROUNDED =                           
084000            100 * (U045-SUTOTBV-RAAR - WS-DIFF) / WS-DIFF                 
084100           IF WS-RETOTBV-RAAR-TB > +99.9                                  
084200              MOVE +99.9                TO U045-RETOTBV-RAAR-TB           
084300           ELSE                                                           
084400              IF WS-RETOTBV-RAAR-TB < -99.9                               
084500                 MOVE -99.9             TO U045-RETOTBV-RAAR-TB           
084600              ELSE                                                        
084700                 MOVE WS-RETOTBV-RAAR-TB TO U045-RETOTBV-RAAR-TB          
084800              END-IF                                                      
084900           END-IF                                                         
085000        END-IF                                                            
085100***                     TÄCKNINGSGRAD    ***                              
085200     SKIP2                                                                
085300***  AF1 TG           ***                                                 
085400     SKIP2                                                                
085500        IF U045-SUARTFSG-FRAAR = ZERO                                     
085600           CONTINUE                                                       
085700        ELSE                                                              
085800           COMPUTE WS-RETOTBV-FRAAR ROUNDED =                             
085900           (100 * (U045-SUARTFSG-FRAAR -                                  
086000           U045-SUARTSJK-FRAAR)) / U045-SUARTFSG-FRAAR                    
086100           IF WS-RETOTBV-FRAAR > +99.9                                    
086200              MOVE +99.9                TO WS-RETOTBV-FRAAR               
086300           ELSE                                                           
086400              IF WS-RETOTBV-FRAAR < -99.9                                 
086500                 MOVE -99.9             TO WS-RETOTBV-FRAAR               
086600              END-IF                                                      
086700           END-IF                                                         
086800        END-IF                                                            
086900***  AF2 TG           ***                                                 
087000     SKIP2                                                                
087100        IF U045-SUARTFSG-RAAR = ZERO                                      
087200           CONTINUE                                                       
087300        ELSE                                                              
087400           COMPUTE WS-RETOTBV-RAAR ROUNDED =                              
087500            100 * (U045-SUARTFSG-RAAR -                                   
087600           U045-SUARTSJK-RAAR) / U045-SUARTFSG-RAAR                       
087700           IF WS-RETOTBV-RAAR  > +99.9                                    
087800              MOVE +99.9                TO U045-RETOTBV-RAAR              
087900           ELSE                                                           
088000              IF WS-RETOTBV-RAAR  < -99.9                                 
088100                 MOVE -99.9             TO U045-RETOTBV-RAAR              
088200              ELSE                                                        
088300                 MOVE WS-RETOTBV-RAAR   TO U045-RETOTBV-RAAR              
088400              END-IF                                                      
088500           END-IF                                                         
088600        END-IF                                                            
088700***  AF4 TG           ***                                                 
088800     SKIP2                                                                
088900        IF U045-SUARTFSG-AAR = ZERO                                       
089000           CONTINUE                                                       
089100        ELSE                                                              
089200           COMPUTE WS-RETOTBV-AAR ROUNDED  =                              
089300            100 * (U045-SUARTFSG-AAR -                                    
089400           U045-SUARTSJK-AAR) / U045-SUARTFSG-AAR                         
089500           IF WS-RETOTBV-AAR   > +99.9                                    
089600              MOVE +99.9                TO U045-RETOTBV-AAR               
089700           ELSE                                                           
089800              IF WS-RETOTBV-AAR  < -99.9                                  
089900                 MOVE -99.9             TO U045-RETOTBV-AAR               
090000              ELSE                                                        
090100                 MOVE WS-RETOTBV-AAR    TO U045-RETOTBV-AAR               
090200              END-IF                                                      
090300           END-IF                                                         
090400        END-IF                                                            
090500***  AF5 TG           ***                                                 
090600     SKIP2                                                                
090700        IF U045-SUARTFSG-PER = ZERO                                       
090800           CONTINUE                                                       
090900        ELSE                                                              
091000           COMPUTE WS-RETOTBV-PER ROUNDED  =                              
091100            100 * (U045-SUARTFSG-PER -                                    
091200           U045-SUARTSJK-PER) / U045-SUARTFSG-PER                         
091300           IF WS-RETOTBV-PER   > +99.9                                    
091400              MOVE +99.9                TO U045-RETOTBV-PER               
091500           ELSE                                                           
091600              IF WS-RETOTBV-PER   < -99.9                                 
091700                 MOVE -99.9                TO U045-RETOTBV-PER            
091800              ELSE                                                        
091900                 MOVE WS-RETOTBV-PER       TO U045-RETOTBV-PER            
092000              END-IF                                                      
092100           END-IF                                                         
092200        END-IF                                                            
092300***  AF2/AF1 TG       ***                                                 
092400     SKIP2                                                                
092500        COMPUTE WS-RETOTBV-RAAR-TG ROUNDED =                              
092600        WS-RETOTBV-RAAR -  WS-RETOTBV-FRAAR                               
092700        IF WS-RETOTBV-RAAR-TG > +99.9                                     
092800           MOVE +99.9                TO U045-RETOTBV-RAAR-TG              
092900        ELSE                                                              
093000           IF WS-RETOTBV-RAAR-TG < -99.9                                  
093100              MOVE -99.9             TO U045-RETOTBV-RAAR-TG              
093200           ELSE                                                           
093300              MOVE WS-RETOTBV-RAAR-TG TO U045-RETOTBV-RAAR-TG             
093400           END-IF                                                         
093500        END-IF                                                            
093600*************************     AVVIKELSE        ***                        
093700***  AVV SEK AF4/AF3                                                      
093800     SKIP2                                                                
093900        IF U045-SUARTFSG-FAAR = ZERO                                      
094000           CONTINUE                                                       
094100        ELSE                                                              
094200           COMPUTE WS-REFSG-AAR ROUNDED = (100 *                          
094300           (U045-SUARTFSG-AAR - U045-SUARTFSG-FAAR)) /                    
094400                                  U045-SUARTFSG-FAAR                      
094500           IF WS-REFSG-AAR > +99.9                                        
094600              MOVE +99.9                TO U045-REFSG-AAR                 
094700           ELSE                                                           
094800              IF WS-REFSG-AAR < -99.9                                     
094900                 MOVE -99.9             TO U045-REFSG-AAR                 
095000              ELSE                                                        
095100                 MOVE WS-REFSG-AAR      TO U045-REFSG-AAR                 
095200              END-IF                                                      
095300           END-IF                                                         
095400        END-IF                                                            
095500***  AVV SEK AF2/AF1                                                      
095600     SKIP2                                                                
095700        IF U045-SUARTFSG-FRAAR = ZERO                                     
095800           CONTINUE                                                       
095900        ELSE                                                              
096000           COMPUTE WS-REFSG-RAAR ROUNDED =  100 *                         
096100           (U045-SUARTFSG-RAAR - U045-SUARTFSG-FRAAR) /                   
096200                                   U045-SUARTFSG-FRAAR                    
096300           IF WS-REFSG-RAAR > +99.9                                       
096400              MOVE +99.9                TO U045-REFSG-RAAR                
096500           ELSE                                                           
096600              IF WS-REFSG-RAAR < -99.9                                    
096700                 MOVE -99.9             TO U045-REFSG-RAAR                
096800              ELSE                                                        
096900                 MOVE WS-REFSG-RAAR     TO U045-REFSG-RAAR                
097000              END-IF                                                      
097100           END-IF                                                         
097200        END-IF                                                            
097300***  AVV STYCK AF2/AF1                                                    
097400     SKIP2                                                                
097500        IF U045-SULEVANT-FRAAR = ZERO                                     
097600           CONTINUE                                                       
097700        ELSE                                                              
097800           COMPUTE WS-RELEVANT-RAAR ROUNDED =  100 *                      
097900           (U045-SULEVANT-RAAR - U045-SULEVANT-FRAAR) /                   
098000                                  U045-SULEVANT-FRAAR                     
098100           IF WS-RELEVANT-RAAR > +99.9                                    
098200              MOVE +99.9                TO U045-RELEVANT-RAAR             
098300           ELSE                                                           
098400              IF WS-RELEVANT-RAAR < -99.9                                 
098500                 MOVE -99.9             TO U045-RELEVANT-RAAR             
098600              ELSE                                                        
098700                 MOVE WS-RELEVANT-RAAR  TO U045-RELEVANT-RAAR             
098800              END-IF                                                      
098900           END-IF                                                         
099000        END-IF                                                            
099110        IF U045-BEFKNGRP = SPACE                                          
099200           PERFORM S14-HAEMTA-BEFKNGRP                                    
099210        END-IF                                                            
100100        PERFORM S09-EV-SKAPA-BUDGET                                       
100200        PERFORM S07-SKRIV-45-FIL                                          
100300     END-IF                                                               
100400     .                                                                    
100500    EJECT                                                                 
100600 S07-SKRIV-45-FIL           SECTION.                                      
100700     SKIP2                                                                
100800     WRITE U045-POST FROM U045-AREA                                       
100900     PERFORM S08-CALL-POSTSUM-45-FIL                                      
101000     MOVE 45NTLZD-AREA                 TO U045-AREA                       
101100     IF SORT-EOF = NEJ                                                    
101200        MOVE SORT43-001-GRUPP          TO U045-001-GRUPP                  
101300        MOVE SORT43-002-GRUPP          TO U045-002-GRUPP                  
101400     END-IF                                                               
101500     .                                                                    
101600     EJECT                                                                
101700 S08-CALL-POSTSUM-45-FIL  SECTION.                                        
101800     SKIP2                                                                
101900     MOVE '    '                 TO POSTSUM-TRANSTYP                      
102000     MOVE 'W33045'               TO POSTSUM-FDNAMN                        
102100     MOVE 'W33044D6'             TO POSTSUM-DDNAMN2                       
102200     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
102300     .                                                                    
102400     EJECT                                                                
102500 S09-EV-SKAPA-BUDGET      SECTION.                                        
102600     SKIP2                                                                
102700     MOVE U045-KDMARK-BUDG            TO W-KDMARK-BUDG                    
102800     MOVE U045-KDPRODSL               TO W-KDPRODSL                       
102900     MOVE U045-IDFKNGRP               TO W-IDFKNGRP                       
103000     PERFORM IMS-GU-XXCI11                                                
103100     IF SEGMENT-FINNS                                                     
103200        MOVE 3136-IDSKURVA            TO W-IDSKURVA                       
103300        MOVE 3136-SUTOTFSG-BUDG       TO WS-SUTOTFSG-BUDG                 
103400        PERFORM IMS-GU-XXCH11                                             
103500        IF SEGMENT-FINNS                                                  
103600           MOVE ZERO                  TO WS-REFSGSIX                      
103700           MOVE +1                    TO IX                               
103800           PERFORM UNTIL IX > WS-DAGENS-TIPP-MINUS-1                      
103900           OR > MAX-TIPP                                                  
104000              ADD 3134-REFSGSIX (IX)  TO WS-REFSGSIX                      
104100              ADD +1                  TO IX                               
104200           END-PERFORM                                                    
104300           COMPUTE U045-SUTOTFSG-BUDG ROUNDED =                           
104400           WS-SUTOTFSG-BUDG * WS-REFSGSIX  / 100                          
104500        ELSE                                                              
104600           COMPUTE U045-SUTOTFSG-BUDG ROUNDED =                           
104700              WS-DAGENS-TIPP-MINUS-1 * WS-SUTOTFSG-BUDG /                 
104800              MAX-TIPP                                                    
104900        END-IF                                                            
107300     END-IF                                                               
107400     .                                                                    
107500     EJECT                                                                
113800 S11-RETURN       SECTION.                                                
113900     SKIP2                                                                
114000     RETURN SORTFIL INTO WS-SORTERAD-AREA                                 
114100     AT END                                                               
114200       MOVE JA                    TO SORT-EOF                             
114300     END-RETURN                                                           
114400                                                                          
114500     IF SORT-EOF = NEJ                                                    
114600        MOVE 'SORT'                TO POSTSUM-TRANSTYP                    
114700        MOVE '      '              TO POSTSUM-FDNAMN                      
114800        MOVE 'W33044DS'            TO POSTSUM-DDNAMN2                     
114900        CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                     
115000     END-IF                                                               
115100     .                                                                    
115200     EJECT                                                                
115300 S12-LAS-99-FIL    SECTION.                                               
115400     SKIP2                                                                
115500     READ W33099  INTO STRUKT-TAB (IX)                                    
115600     AT END                                                               
115700       MOVE JA                    TO I99-EOF                              
115800     END-READ                                                             
115900                                                                          
116000     IF I99-EOF = NEJ                                                     
116100       MOVE '    '                TO POSTSUM-TRANSTYP                     
116200       MOVE 'W33099'              TO POSTSUM-FDNAMN                       
116300       MOVE 'W33044D5'            TO POSTSUM-DDNAMN2                      
116400       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
116500     END-IF                                                               
116600     .                                                                    
116700     EJECT                                                                
116800 S14-HAEMTA-BEFKNGRP        SECTION.                                      
116900     SKIP2                                                                
117000     MOVE +1                          TO IX                               
117100     PERFORM UNTIL IX > MAX-I99-IX                                        
117200        IF STRUKT-IDFKNGRP (IX) = WS-IDFKNGRP                             
117300           MOVE STRUKT-BEFKNGRP (IX)  TO U045-BEFKNGRP                    
117400           ADD MAX-I99-IX             TO IX                               
117500        ELSE                                                              
117600           ADD +1                     TO IX                               
117700        END-IF                                                            
117800     END-PERFORM                                                          
117900     .                                                                    
118000     EJECT                                                                
118100 S99-CALL-WDATKONV      SECTION.                                          
118200     SKIP2                                                                
118300     CALL WDATKONV           USING DAT-KDDATFORM                          
118400                                   DAT-I-TIDATUM                          
118500                                   DAT-O-TIDATUM                          
118600                                   DAT-KDSVAR                             
118700     .                                                                    
118800     EJECT                                                                
118900 Z-FINIT SECTION.                                                         
119000     SKIP2                                                                
119100     CLOSE W33031S                                                        
119200           W33043                                                         
119500           W33099                                                         
119600           W33045                                                         
119700     MOVE 'S' TO POSTSUM-OPKOD                                            
119800     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
119900     .                                                                    
120000     EJECT                                                                
120100 IMS-GU-XXCI11 SECTION.                                                   
120200     SKIP2                                                                
120300     STRING 'WLXXCI01(WDGXKEY  =' W-3135-X ')'                            
120400             DELIMITED BY SIZE INTO SSA1                                  
120500     STRING 'WLXXCI11(WDGXKEY  =' W-3136-X ')'                            
120600             DELIMITED BY SIZE INTO SSA2                                  
120700     MOVE '  GE' TO GODK-STATUSKODER                                      
120800     CALL CBLTDLI USING GU XXCI-PCB DLI-IO-AREA SSA1 SSA2                 
120900     MOVE XXCI-STATUS-CODE TO STATUS-WS                                   
121000     PERFORM IMS-STATUSKONTROLL                                           
121100     .                                                                    
121200     SKIP2                                                                
121300 IMS-GU-XXCH11 SECTION.                                                   
121400     SKIP2                                                                
121500     STRING 'WLXXCH01(WDGXKEY  =' W-3133-X ')'                            
121600             DELIMITED BY SIZE INTO SSA1                                  
121700     STRING 'WLXXCH11(WDGXKEY  =' W-3134-X ')'                            
121800             DELIMITED BY SIZE INTO SSA2                                  
121900     MOVE '  GE' TO GODK-STATUSKODER                                      
122000     CALL CBLTDLI USING GU XXCH-PCB DLI-IO-AREA SSA1 SSA2                 
122100     MOVE XXCH-STATUS-CODE TO STATUS-WS                                   
122200     PERFORM IMS-STATUSKONTROLL                                           
122300     .                                                                    
122400     SKIP2                                                                
122500 IMS-STATUSKONTROLL SECTION.                                              
122600     SET STATUS-IX TO 1                                                   
122700     SEARCH GODK-STATUS AT END CALL FELLOG                                
122800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
122900     END-SEARCH                                                           
123000     .                                                                    
