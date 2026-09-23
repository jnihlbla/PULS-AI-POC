000100 ID  DIVISION.                                                            
000300 PROGRAM-ID.    W3307000.                                                 
000400 AUTHOR.        SUSANNE ENEGARD.                                          
000500 DATE-WRITTEN.  NOVEMBER 1989.                                            
000510 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PLOCKAR UT INFORMATION FÖR DE URVAL SOM ANGIVITS PÅ FILEN        
001000*                                                                         
001100                                                                          
001200 ENVIRONMENT DIVISION.                                                    
001300     EJECT                                                                
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700                                                                          
001800*    --- INFILER:                                                         
001900                                                                          
002000     SELECT W33035                       ASSIGN TO W33070D1.              
002100                                                                          
002200     SELECT W33013                       ASSIGN TO W33070D2.              
002300                                                                          
002400     SELECT W33019                       ASSIGN TO W33070D3.              
002500*    --- UTFIL:                                                           
002600*           --- AF-OMFATTADE POSTER:                                      
002700     SELECT W33071                       ASSIGN TO W33070D4.              
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000                                                                          
003100 FILE SECTION.                                                            
003200                                                                          
003300 FD  W33035                                                               
003400     LABEL RECORD   STANDARD                                              
003500     RECORDING      V                                                     
003600     BLOCK CONTAINS 0.                                                    
003700                                                                          
003800*01  -COPY W3303503  -L.                                                  
003900                                                                          
004000*01  -COPY W3303502  -L.                                                  
004100     EJECT                                                                
004200 FD  W33013                                                               
004300     LABEL RECORD   STANDARD                                              
004400     RECORDING      V                                                     
004500     BLOCK CONTAINS 0.                                                    
004600                                                                          
004700*01  -COPY W330310  -L.                                                   
004800                                                                          
004900*01  -COPY W330300  -L.                                                   
005000                                                                          
005100*01  -COPY W330320  -L.                                                   
005200                                                                          
005300*01  -COPY W330330  -L.                                                   
005400                                                                          
005500*01  -COPY W330340  -L.                                                   
005600                                                                          
005700*01  -COPY W330350  -L.                                                   
005800     EJECT                                                                
005900 FD  W33019                                                               
006000     LABEL RECORD   STANDARD                                              
006100     RECORDING      F                                                     
006200     BLOCK CONTAINS 0.                                                    
006300                                                                          
006400*01  -COPY W33019  -L.                                                    
006500     EJECT                                                                
006600 FD  W33071                                                               
006700     LABEL RECORD   STANDARD                                              
006800     RECORDING      F                                                     
006900     BLOCK CONTAINS 0.                                                    
007000                                                                          
007100*01  UT371-POST  -COPY W33071  -L.                                        
007200     EJECT                                                                
007300 WORKING-STORAGE SECTION.                                                 
007310*    -COPY WY2000W3                                                       
007400     SKIP3                                                                
007500 77  IDPGM                       PIC X(8)    VALUE 'W3307000'.            
007600 77  JA                          PIC X(1)    VALUE 'J'.                   
007700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007800 77  VECKOFIL-EOF                PIC X(1)    VALUE 'N'.                   
007900 77  URVALSFIL-EOF               PIC X(1)    VALUE 'N'.                   
008000 77  INFOFIL-EOF                 PIC X(1)    VALUE 'N'.                   
008100 77  PRISTYP-FINNS               PIC X(1)    VALUE 'N'.                   
008200 77  VOLKLASS-FINNS              PIC X(1)    VALUE 'N'.                   
008300 77  PRODSL-FINNS                PIC X(1)    VALUE 'N'.                   
008400 77  KONCERN-FINNS               PIC X(1)    VALUE 'N'.                   
008500 77  LEVERANT-FINNS              PIC X(1)    VALUE 'N'.                   
008600 77  LKTO-FINNS                  PIC X(1)    VALUE 'N'.                   
008700 77  MARKNAD-FINNS               PIC X(1)    VALUE 'N'.                   
008800 77  DISTRIKT-FINNS              PIC X(1)    VALUE 'N'.                   
008900 77  ANSK-FINNS                  PIC X(1)    VALUE 'N'.                   
009000 77  ARTIKEL-FINNS               PIC X(1)    VALUE 'N'.                   
009100 77  FUNKGRP-FINNS               PIC X(1)    VALUE 'N'.                   
009200 77  GRUNDURV-FINNS              PIC X(1)    VALUE 'N'.                   
009300 77  EXTRA-FINNS                 PIC X(1)    VALUE 'N'.                   
009400 77  SPEC-FINNS                  PIC X(1)    VALUE 'N'.                   
009500 77  RAB-FINNS                   PIC X(1)    VALUE 'N'.                   
009600 77  MAN-FINNS                   PIC X(1)    VALUE 'N'.                   
009700 77  KRE-FINNS                   PIC X(1)    VALUE 'N'.                   
009800 77  KUND-SAKNAS                 PIC X(1)    VALUE 'N'.                   
009900 77  URV-IX                      PIC S9(4)   VALUE +1  COMP SYNC.         
010000 77  URV-3202-IX                 PIC S9(4)   VALUE +1  COMP SYNC.         
010100 77  URV-3203-IX                 PIC S9(4)   VALUE +1  COMP SYNC.         
010200 77  ARB-IX                      PIC S9(4)   VALUE +1  COMP SYNC.         
010300 77  IX-PER                      PIC S9(4)   VALUE +1  COMP SYNC.         
010400 77  FORSTA-GANG                 PIC X(1)    VALUE 'J'.                   
010500 77  NY-300-POST                 PIC X(1)    VALUE 'J'.                   
010600 77  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
010700 77  SPAR-PRARTSJK               PIC S9(7)V9(2) VALUE ZERO COMP-3.        
010800 77  SPAR-KDPRTYPG               PIC X       VALUE SPACE.                 
010900 77  SPAR-IDLEVNR                PIC  X(5)   VALUE SPACE.                 
011000 77  SPAR-IDANSK                 PIC S9(3)   VALUE ZERO  COMP-3.          
011100 77  SPAR-KDVVKL                 PIC S9      VALUE ZERO  COMP-3.          
011200 77  SPAR-IDLKTO                 PIC S9(7)   VALUE ZERO  COMP-3.          
011300 77  TOT-RAKNARE-WEEK            PIC S9(9)   VALUE +0    COMP.            
011400 77  TOT-RAKNARE-URV             PIC S9(9)   VALUE +0    COMP.            
011500 77  TOT-RAKNARE-UT              PIC S9(9)   VALUE +0    COMP.            
011600 77  TOT-RAKNARE-INFO            PIC S9(9)   VALUE +0    COMP.            
011700                                                                          
011800 01  FILLER                     PIC X(16)  VALUE 'WS-ANT'.                
011900 01  WS-ANT-FSG-AREA.                                                     
012000     03  WS-SULEVANT             PIC S9(9)   COMP-3.                      
012100     03  WS-SULEVANT-RAB         PIC S9(9)   COMP-3.                      
012200     03  WS-SULEVANT-SPEC        PIC S9(9)   COMP-3.                      
012300     03  WS-SULEVANT-MAN         PIC S9(9)   COMP-3.                      
012400     03  WS-SULEVANT-KRE         PIC S9(9)   COMP-3.                      
012500     03  WS-SULEVANT-RABSPEC     PIC S9(9)   COMP-3.                      
012600     03  WS-SUARTFSG             PIC S9(9)V9(2)  COMP-3.                  
012700     03  WS-SUARTFSG-RAB         PIC S9(9)V9(2)  COMP-3.                  
012800     03  WS-SUARTFSG-SPEC        PIC S9(9)V9(2)  COMP-3.                  
012900     03  WS-SUARTFSG-MAN         PIC S9(9)V9(2)  COMP-3.                  
013000     03  WS-SUARTFSG-KRE         PIC S9(9)V9(2)  COMP-3.                  
013100     03  WS-SUARTFSG-RABSPEC     PIC S9(9)V9(2)  COMP-3.                  
013200     EJECT                                                                
013300                                                                          
013400 01  FILLER                     PIC X(16)  VALUE 'DATUM'.                 
013500 01  WS-DAFSGVV-FOM              PIC 9(6).                                
013600 01  FILLER     REDEFINES WS-DAFSGVV-FOM.                                 
013700*    03  FILLER                  PIC 9(2).                                
013800     03  WS-TIAAVV-FOM           PIC 9(6).                                
013900                                                                          
014000 01  WS-TIAAVV-FOM-FAAR          PIC 9(6).                                
014100 01  FILLER     REDEFINES WS-TIAAVV-FOM-FAAR.                             
014200     03  WS-TIAA-FOM-FAAR        PIC 9(4).                                
014300     03  FILLER                  PIC 9(2).                                
014400                                                                          
014500 01  WS-DAFSGVV-TOM              PIC 9(6).                                
014600 01  FILLER     REDEFINES WS-DAFSGVV-TOM.                                 
014700*    03  FILLER                  PIC 9(2).                                
014800     03  WS-TIAAVV-TOM           PIC 9(6).                                
014900                                                                          
015000 01  WS-TIAAVV-TOM-FAAR          PIC 9(6).                                
015100 01  FILLER     REDEFINES WS-TIAAVV-TOM-FAAR.                             
015200     03  WS-TIAA-TOM-FAAR        PIC 9(4).                                
015300     03  FILLER                  PIC 9(2).                                
015400                                                                          
015500 01  WS-DAFSGVV                  PIC 9(6).                                
015600 01  FILLER     REDEFINES WS-DAFSGVV.                                     
015700*    03  FILLER                  PIC 9(2).                                
015800     03  WS-TIAAVV               PIC 9(6).                                
015900     EJECT                                                                
016000 01  FILLER                     PIC X(16)  VALUE 'DYN-SUB-PGM'.           
016100 01  DYNAMISKA-SUBPROGRAM.                                                
016200*                                                                         
016300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016400     03  W330KUND                PIC X(8)    VALUE 'W330KUND'.            
016500     03  W510MARK                PIC X(8)    VALUE 'W510MARK'.            
016510     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
016600     SKIP2                                                                
016700*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
016800*                                                                         
016900 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
017000*01  -COPY W0005 -PRE  POSTSUM-                                           
017100     EJECT                                                                
017200*- - - - - - - - - - - - - - - - INFO-AREA                                
017300*                                                                         
017400 01  FILLER                       PIC X(16)  VALUE 'INFO-AREA'.           
017500*01  AREA  -PRE INFO-  -COPY W33019.                                      
017600     EJECT                                                                
017700*- - - - - - - - - - - - - - - - KLAR-AREA                                
017800*                                                                         
017900 01  FILLER                       PIC X(16)  VALUE 'KLARAREA'.            
018000*01  AREA  -PRE KLAR-  -COPY W33071.                                      
018100     EJECT                                                                
018200*- - - - - - - - - - - - - - - - VECKO-AREA                               
018300*                                                                         
018400 01  FILLER                       PIC X(16)  VALUE 'VECKO-AREA'.          
018500 01  VECKO-AREA                   PIC X(28).                              
018600*01  AREA  -PRE IN310- -COPY W330310  -RED VECKO-AREA.                    
018700     SKIP2                                                                
018800*01  AREA  -PRE IN300- -COPY W330300  -RED VECKO-AREA.                    
018900     SKIP2                                                                
019000*01  AREA  -PRE IN320- -COPY W330320  -RED VECKO-AREA.                    
019100     SKIP2                                                                
019200*01  AREA  -PRE IN330- -COPY W330330  -RED VECKO-AREA.                    
019300     SKIP2                                                                
019400*01  AREA  -PRE IN340- -COPY W330340  -RED VECKO-AREA.                    
019500     SKIP2                                                                
019600*01  AREA  -PRE IN350- -COPY W330350  -RED VECKO-AREA.                    
019700     EJECT                                                                
019800*- - - - - - - - - - - - - - - - IN-AREA                                  
019900*                                                                         
020000 01  FILLER                       PIC X(16)  VALUE 'INAREA'.              
020100 01  IN-AREA                      PIC X(2621).                            
020200*01  AREA  -PRE IN3203- -COPY W3303503  -RED IN-AREA.                     
020300     EJECT                                                                
020400*01  AREA  -PRE IN3202- -COPY W3303502  -RED IN-AREA.                     
020500     EJECT                                                                
020600*- - - - - - - - - - - - - - - - PARAMETRAR TILL W330KUND                 
020700*                                                                         
020800 01  FILLER                       PIC X(16)  VALUE 'W330KUND'.            
020900*01  KUND-AREA  -COPY W330KUND                                            
021000     EJECT                                                                
021100*- - - - - - - - - - - - - - - - PARAMETRAR TILL W510MARK                 
021200*                                                                         
021300 01  FILLER                       PIC X(16)  VALUE 'W510MARK'.            
021400*01  MARK-AREA  -COPY W510MARK                                            
021410     EJECT                                                                
021440 01  FILLER                       PIC X(16)  VALUE 'DATKORT '.            
021441 01  WDATUM                       PIC X(6)   VALUE 'WDATUM'.              
021450*01  -COPY WDATKORT                                                       
021500     EJECT                                                                
021600*- - - - - - - - - - - - - - - - 3202-AREA                                
021700*                                                                         
021800 01  FILLER                       PIC X(16)  VALUE '3202-AREA'.           
021900 01  TABELL-3202.                                                         
022000     03  3202-RAD OCCURS 100.                                             
022100*        05  AREA  -PRE 3202-  -COPY W3303502.                            
022200     EJECT                                                                
022300*- - - - - - - - - - - - - - - - 3203-AREA                                
022400*                                                                         
022500 01  FILLER                       PIC X(16)  VALUE '3203-AREA'.           
022600 01  TABELL-3203.                                                         
022700     03  3203-RAD OCCURS 100.                                             
022800*        05  AREA  -PRE 3203-  -COPY W3303503.                            
022900     EJECT                                                                
023000 LINKAGE SECTION.                                                         
023100*01  -COPY W0008  -PRE GMTA-.                                             
023200     05  FILLER                   PIC X.                                  
023300                                                                          
023310*01  -COPY W0008  -PRE BETA-.                                             
023320     05  FILLER                   PIC X.                                  
023330     EJECT                                                                
023400 PROCEDURE DIVISION USING GMTA-PCB BETA-PCB.                              
023410 STYR SECTION.                                                            
023500     ENTRY 'DLITCBL' USING GMTA-PCB BETA-PCB.                             
023600                                                                          
023800     PERFORM A-INIT                                                       
023900     PERFORM B-LAGRA-URVAL                                                
024000     PERFORM C-HAEMTA-KUND                                                
024100     PERFORM S01-LAS-VECKOFIL                                             
024200     PERFORM S06-LAS-INFOFIL                                              
024300     PERFORM UNTIL VECKOFIL-EOF = JA                                      
024400        EVALUATE IN300-IDPTYP                                             
024500          WHEN '300' PERFORM D-BEHANDLA-300-POST                          
024600          WHEN '310' PERFORM E-BEHANDLA-310-POST                          
024700          WHEN '320' PERFORM F-BEHANDLA-320-POST                          
024800          WHEN '330' PERFORM G-BEHANDLA-330-POST                          
024900          WHEN '340' PERFORM H-BEHANDLA-340-POST                          
025000          WHEN '350' PERFORM I-BEHANDLA-350-POST                          
025100        END-EVALUATE                                                      
025200        PERFORM S01-LAS-VECKOFIL                                          
025300     END-PERFORM                                                          
025400     PERFORM S02-KOLLA-SKRIV-POST                                         
025500     PERFORM Z-FINIT                                                      
025600     MOVE ZERO TO RETURN-CODE                                             
025700     GOBACK                                                               
025800     .                                                                    
025900     EJECT                                                                
026000 A-INIT SECTION.                                                          
026100                                                                          
026200     OPEN INPUT W33035                                                    
026300                W33013                                                    
026400                W33019                                                    
026500     OPEN OUTPUT W33071                                                   
026600                                                                          
026610     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
026620                                                                          
026800     MOVE JA TO FORSTA-GANG                                               
026900                                                                          
027000     MOVE NEJ TO KUND-SAKNAS                                              
027100                                                                          
027200     INITIALIZE TABELL-3202                                               
027300     INITIALIZE TABELL-3203                                               
027400                                                                          
027500     MOVE +0 TO TOT-RAKNARE-WEEK                                          
027600     MOVE +0 TO TOT-RAKNARE-URV                                           
027700     MOVE +0 TO TOT-RAKNARE-UT                                            
027800     MOVE +0 TO TOT-RAKNARE-INFO                                          
027900     .                                                                    
028000     EJECT                                                                
028100 B-LAGRA-URVAL SECTION.                                                   
028200                                                                          
028300     PERFORM S03-LAS-URVALSFIL                                            
028400     MOVE +1 TO URV-3202-IX                                               
028500     MOVE +1 TO URV-3203-IX                                               
028600     PERFORM UNTIL URVALSFIL-EOF = JA                                     
028700        IF IN3202-IDTRANS = '3202'                                        
028800           MOVE IN3202-AREA TO 3202-RAD(URV-3202-IX)                      
028900           ADD +1 TO URV-3202-IX                                          
029000        ELSE                                                              
029100           MOVE IN3203-AREA TO 3203-RAD(URV-3203-IX)                      
029200           ADD +1 TO URV-3203-IX                                          
029300        END-IF                                                            
029400        PERFORM S03-LAS-URVALSFIL                                         
029500     END-PERFORM                                                          
029600     .                                                                    
029700     EJECT                                                                
029800 C-HAEMTA-KUND SECTION.                                                   
029900                                                                          
030000     CALL W330KUND USING KUND-AREA GMTA-PCB BETA-PCB                      
030100                                                                          
030200****    FYLL KUNDTABELL MED MARKNADSKOD OCH MARKNADSBENÄMNING             
030300                                                                          
030500     MOVE D-AAR                   TO MARK-TIAA                            
030600     MOVE +0                      TO MARK-KDCALL                          
030610     SET KUND-IX TO +1                                                    
030700     PERFORM 1600 TIMES                                                   
030800       MOVE KUND-IDDISTR(KUND-IX) TO MARK-IDDISTR                         
030900       CALL W510MARK USING MARK-AREA                                      
032300       MOVE MARK-KDMARK-BUDG      TO KUND-KDMARK-BUDG(KUND-IX)            
032400       MOVE MARK-BEMARKN          TO KUND-BEMARK-BUDG(KUND-IX)            
032600       SET KUND-IX UP BY +1                                               
032700     END-PERFORM                                                          
032800     .                                                                    
032900     EJECT                                                                
033000 D-BEHANDLA-300-POST  SECTION.                                            
033100                                                                          
033200     IF KUND-SAKNAS = JA                                                  
033300        MOVE NEJ TO KUND-SAKNAS                                           
033400     END-IF                                                               
033500                                                                          
033600     IF FORSTA-GANG = JA                                                  
033700        PERFORM S030-NOLLSTAELL-AREA                                      
033800        MOVE NEJ TO FORSTA-GANG                                           
033900        MOVE IN300-IDARTNR  TO KLAR-IDARTNR                               
034000        PERFORM DA-HAMTA-ARTINFO                                          
034100     ELSE                                                                 
034200        PERFORM S02-KOLLA-SKRIV-POST                                      
034300        PERFORM S030-NOLLSTAELL-AREA                                      
034400        IF IN300-IDARTNR = KLAR-IDARTNR                                   
034500           CONTINUE                                                       
034600        ELSE                                                              
034700           MOVE IN300-IDARTNR  TO KLAR-IDARTNR                            
034800           PERFORM DA-HAMTA-ARTINFO                                       
034900        END-IF                                                            
035000     END-IF                                                               
035100     MOVE JA  TO NY-300-POST                                              
035200     MOVE IN300-PRARTSJK TO KLAR-PRARTSJK                                 
035300     MOVE IN300-DAFSGVV  TO KLAR-DAFSGVV                                  
035400     .                                                                    
035500     EJECT                                                                
035600 DA-HAMTA-ARTINFO SECTION.                                                
035700                                                                          
035800     PERFORM UNTIL INFOFIL-EOF = JA OR                                    
035900       INFO-IDARTNR > KLAR-IDARTNR                                        
036000        IF INFO-IDARTNR = KLAR-IDARTNR                                    
036100           MOVE INFO-BEART-SVE TO KLAR-BEART-SVE                          
036200           MOVE INFO-KDPRODSL  TO KLAR-KDPRODSL                           
036300           MOVE INFO-BEPRODSL  TO KLAR-BEPRODSL                           
036400           MOVE INFO-IDFKNGRP  TO KLAR-IDFKNGRP                           
036500           MOVE INFO-BEFKNGRP  TO KLAR-BEFKNGRP                           
036600           MOVE INFO-IDLEVNR   TO SPAR-IDLEVNR                            
036700           MOVE INFO-IDANSK    TO SPAR-IDANSK                             
036800           MOVE INFO-KDVVKL    TO SPAR-KDVVKL                             
036900           MOVE INFO-IDLKTO    TO SPAR-IDLKTO                             
037000        END-IF                                                            
037100        PERFORM S06-LAS-INFOFIL                                           
037200     END-PERFORM                                                          
037300     .                                                                    
037400     EJECT                                                                
037500 E-BEHANDLA-310-POST  SECTION.                                            
037600                                                                          
037700     IF KUND-SAKNAS = JA                                                  
037800        MOVE NEJ TO KUND-SAKNAS                                           
037900     END-IF                                                               
038000                                                                          
038100     IF NY-300-POST = JA                                                  
038200        MOVE NEJ TO NY-300-POST                                           
038300     ELSE                                                                 
038400        PERFORM S02-KOLLA-SKRIV-POST                                      
038500        PERFORM S030-NOLLSTAELL-AREA                                      
038600     END-IF                                                               
038700     MOVE IN310-IDDISTR TO KLAR-IDDISTR                                   
038800     PERFORM EA-LAGRA-KUND-INFO                                           
038900     MOVE IN310-SULEVANT TO WS-SULEVANT                                   
039000     MOVE IN310-SUARTFSG TO WS-SUARTFSG                                   
039100     .                                                                    
039200     EJECT                                                                
039300 EA-LAGRA-KUND-INFO   SECTION.                                            
039400                                                                          
039500     SEARCH ALL KUND-RAD                                                  
039600       AT END                                                             
039700******* FIX FÖR ATT KLARA RENSNING AV KUNDBASEN                           
039800******************************************************************        
039900        MOVE D-AAR            TO MARK-TIAA                                
040000        MOVE IN310-IDDISTR    TO MARK-IDDISTR                             
040100        MOVE +0               TO MARK-KDCALL                              
040200        CALL W510MARK USING MARK-AREA                                     
040300*FIX PGA SPLITT 930601 KAN TAS BORT 950701 (TVÅ ÅR)                       
040400**** FÖR ATT INTE FÅ MED SIG KONCNR 0 SOM ALLA GAMLA                      
040500**** LV DISTRIKT HAR.                                                     
040600*       MOVE +0                         TO KLAR-IDKONCNR                  
040700        MOVE +999                       TO KLAR-IDKONCNR                  
040800*FIX                                                                      
040900        MOVE MARK-KDMARK-BUDG           TO KLAR-KDMARK-BUDG               
041000        MOVE MARK-BEMARKN               TO KLAR-BEMARK-BUDG               
041100******************************************************************        
041300       WHEN KUND-IDDISTR(KUND-IX) = IN310-IDDISTR                         
041400*FIX     MOVE KUND-IDKONCNR   (KUND-IX) TO KLAR-IDKONCNR                  
041500         MOVE +0                        TO KLAR-IDKONCNR                  
041600         MOVE KUND-KDMARK-BUDG(KUND-IX) TO KLAR-KDMARK-BUDG               
041800         MOVE KUND-BEMARK-BUDG(KUND-IX) TO KLAR-BEMARK-BUDG               
041900     END-SEARCH                                                           
042000     .                                                                    
042100     EJECT                                                                
042200 F-BEHANDLA-320-POST  SECTION.                                            
042300                                                                          
042400     IF KUND-SAKNAS = NEJ                                                 
042500        MOVE IN320-SULEVANT-RAB TO WS-SULEVANT-RAB                        
042600        MOVE IN320-SUARTFSG-RAB TO WS-SUARTFSG-RAB                        
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000 G-BEHANDLA-330-POST  SECTION.                                            
043100                                                                          
043200     IF KUND-SAKNAS = NEJ                                                 
043300        MOVE IN330-SULEVANT-SPEC TO WS-SULEVANT-SPEC                      
043400        MOVE IN330-SUARTFSG-SPEC TO WS-SUARTFSG-SPEC                      
043500     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800 H-BEHANDLA-340-POST  SECTION.                                            
043900                                                                          
044000     IF KUND-SAKNAS = NEJ                                                 
044100        MOVE IN340-SULEVANT-MAN TO WS-SULEVANT-MAN                        
044200        MOVE IN340-SUARTFSG-MAN TO WS-SUARTFSG-MAN                        
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 I-BEHANDLA-350-POST  SECTION.                                            
044700                                                                          
044800     IF KUND-SAKNAS = NEJ                                                 
044900        MOVE IN350-SULEVANT-KRE TO WS-SULEVANT-KRE                        
045000        MOVE IN350-SUARTFSG-KRE TO WS-SUARTFSG-KRE                        
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 S01-LAS-VECKOFIL SECTION.                                                
045500                                                                          
045600     READ W33013 INTO VECKO-AREA                                          
045700     AT END                                                               
045800       MOVE JA TO VECKOFIL-EOF                                            
045900     END-READ                                                             
046000                                                                          
046100     IF VECKOFIL-EOF = NEJ                                                
046200       ADD +1 TO TOT-RAKNARE-WEEK                                         
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600 S02-KOLLA-SKRIV-POST SECTION.                                            
046700                                                                          
046800     PERFORM S02A-KOLLA-3202-URVAL                                        
046900     PERFORM S02B-KOLLA-3203-URVAL                                        
047000     .                                                                    
047100     EJECT                                                                
047200 S02A-KOLLA-3202-URVAL SECTION.                                           
047300                                                                          
047400     MOVE +1 TO URV-IX                                                    
047500     PERFORM UNTIL (3202-IDUSER(URV-IX) = SPACE) OR                       
047600        (URV-IX > 100)                                                    
047700        MOVE 3202-DAFSGVV-FOM(URV-IX) TO WS-DAFSGVV-FOM                   
047800        MOVE WS-TIAAVV-FOM TO WS-TIAAVV-FOM-FAAR                          
047900        SUBTRACT 1 FROM WS-TIAA-FOM-FAAR                                  
048000        MOVE 3202-DAFSGVV-TOM(URV-IX) TO WS-DAFSGVV-TOM                   
048100        MOVE WS-TIAAVV-TOM TO WS-TIAAVV-TOM-FAAR                          
048200        SUBTRACT 1 FROM WS-TIAA-TOM-FAAR                                  
048300        MOVE KLAR-DAFSGVV     TO WS-DAFSGVV                               
048410        IF WS-TIAAVV > WS-TIAAVV-TOM                                      
048500           CONTINUE                                                       
048600        ELSE                                                              
048710           IF WS-TIAAVV < WS-TIAAVV-FOM-FAAR                              
048800              CONTINUE                                                    
048900           ELSE                                                           
049110              IF WS-TIAAVV > WS-TIAAVV-TOM-FAAR AND                       
049120                 WS-TIAAVV < WS-TIAAVV-FOM                                
049200                 CONTINUE                                                 
049300              ELSE                                                        
049400                 MOVE NEJ TO GRUNDURV-FINNS                               
049500                 MOVE NEJ TO EXTRA-FINNS                                  
049600                 PERFORM S02AA-KOLLA-GRUNDURV-FINNS                       
049700                 IF GRUNDURV-FINNS = JA                                   
049800                    PERFORM S02AB-KOLLA-EXTRA                             
049900                    IF EXTRA-FINNS = JA                                   
050000                       MOVE 3202-001-GRUPP(URV-IX) TO                     
050100                       KLAR-001-GRUPP                                     
050200                       PERFORM S05-SKRIV-UTPOST                           
050300                    END-IF                                                
050400                 END-IF                                                   
050500              END-IF                                                      
050600           END-IF                                                         
050700        END-IF                                                            
050800        ADD +1 TO URV-IX                                                  
050900     END-PERFORM                                                          
051000     .                                                                    
051100     EJECT                                                                
051200 S02AA-KOLLA-GRUNDURV-FINNS SECTION.                                      
051300                                                                          
051400     MOVE 3202-KDPRTYPG(URV-IX) TO SPAR-KDPRTYPG                          
051500     PERFORM S020-KOLLA-PRISTYP                                           
051600     IF PRISTYP-FINNS = JA                                                
051700        PERFORM S02AAB-KOLLA-VOLKLASS                                     
051800        IF VOLKLASS-FINNS = JA                                            
051900           PERFORM S02AAC-KOLLA-PRODSL                                    
052000           IF PRODSL-FINNS = JA                                           
052300              PERFORM S02AAE-KOLLA-LEVERANT                               
052400              IF LEVERANT-FINNS = JA                                      
052500                 PERFORM S02AAF-KOLLA-LKTO                                
052600                 IF LKTO-FINNS = JA                                       
052700                    MOVE JA TO GRUNDURV-FINNS                             
052800                  END-IF                                                  
052900              END-IF                                                      
053100           END-IF                                                         
053200        END-IF                                                            
053300     END-IF                                                               
053400     .                                                                    
053500     EJECT                                                                
053600 S02AAB-KOLLA-VOLKLASS SECTION.                                           
053700                                                                          
053800                                                                          
053900     MOVE NEJ TO VOLKLASS-FINNS                                           
054000     IF 3202-KDVVKL(URV-IX, 1) = ZERO AND                                 
054100        3202-KDVVKL(URV-IX, 2) = ZERO AND                                 
054200        3202-KDVVKL(URV-IX, 3) = ZERO AND                                 
054300        3202-KDVVKL(URV-IX, 4) = ZERO AND                                 
054400        3202-KDVVKL(URV-IX, 5) = ZERO                                     
054500        MOVE JA TO VOLKLASS-FINNS                                         
054600     ELSE                                                                 
054700        IF 3202-KDVVKL(URV-IX, 1) = SPAR-KDVVKL OR                        
054800           3202-KDVVKL(URV-IX, 2) = SPAR-KDVVKL OR                        
054900           3202-KDVVKL(URV-IX, 3) = SPAR-KDVVKL OR                        
055000           3202-KDVVKL(URV-IX, 4) = SPAR-KDVVKL OR                        
055100           3202-KDVVKL(URV-IX, 5) = SPAR-KDVVKL                           
055200           MOVE JA TO VOLKLASS-FINNS                                      
055300        END-IF                                                            
055400     END-IF                                                               
055500     .                                                                    
055600     EJECT                                                                
055700 S02AAC-KOLLA-PRODSL SECTION.                                             
055800                                                                          
055900                                                                          
056000     MOVE NEJ TO PRODSL-FINNS                                             
056100     IF 3202-KDPRODSL(URV-IX, 1) = ZERO AND                               
056200        3202-KDPRODSL(URV-IX, 2) = ZERO AND                               
056300        3202-KDPRODSL(URV-IX, 3) = ZERO AND                               
056400        3202-KDPRODSL(URV-IX, 4) = ZERO AND                               
056500        3202-KDPRODSL(URV-IX, 5) = ZERO AND                               
056600        3202-KDPRODSL(URV-IX, 6) = ZERO AND                               
056700        3202-KDPRODSL(URV-IX, 7) = ZERO                                   
056800        MOVE JA TO PRODSL-FINNS                                           
056900     ELSE                                                                 
057000        IF 3202-KDPRODSL(URV-IX, 1) = KLAR-KDPRODSL  OR                   
057100           3202-KDPRODSL(URV-IX, 2) = KLAR-KDPRODSL  OR                   
057200           3202-KDPRODSL(URV-IX, 3) = KLAR-KDPRODSL  OR                   
057300           3202-KDPRODSL(URV-IX, 4) = KLAR-KDPRODSL  OR                   
057400           3202-KDPRODSL(URV-IX, 5) = KLAR-KDPRODSL  OR                   
057500           3202-KDPRODSL(URV-IX, 6) = KLAR-KDPRODSL  OR                   
057600           3202-KDPRODSL(URV-IX, 7) = KLAR-KDPRODSL                       
057700           MOVE JA TO PRODSL-FINNS                                        
057800        END-IF                                                            
057900     END-IF                                                               
058000     .                                                                    
058100     EJECT                                                                
060900 S02AAE-KOLLA-LEVERANT SECTION.                                           
061000                                                                          
061100                                                                          
061200     MOVE NEJ TO LEVERANT-FINNS                                           
061300                                                                          
061400     IF 3202-IDLEVNR(URV-IX, 1) = SPACE AND                               
061500        3202-IDLEVNR(URV-IX, 2) = SPACE AND                               
061600        3202-IDLEVNR(URV-IX, 3) = SPACE AND                               
061700        3202-IDLEVNR(URV-IX, 4) = SPACE AND                               
061800        3202-IDLEVNR(URV-IX, 5) = SPACE AND                               
061900        3202-IDLEVNR(URV-IX, 6) = SPACE AND                               
062000        3202-IDLEVNR(URV-IX, 7) = SPACE AND                               
062100        3202-IDLEVNR(URV-IX, 8) = SPACE                                   
062200        MOVE JA TO LEVERANT-FINNS                                         
062300     ELSE                                                                 
062400*---- DET HAR VISAT SIG ATT VISSA ARTNR HAR LEVNR 0                       
062500*---- OM MAN DÅ GÖR URVAL LEVERANTÖR                                      
062600*---- OCH DÅ GÖR URVAL PÅ PÅ MINDRE ÄN 8 LEVERANTÖRER                     
062700*---- SÅ FÅR MAN MED SIG ALLA ARTIKLAR MED LEVNR 0                        
062800*-->>> DETTA SPÄRRAS BORT I S02AAEA  <<<<                                 
062900                                                                          
063000        PERFORM S02AAEA-LEVNR-NOLL-KOLL                                   
063100        IF 3202-IDLEVNR(URV-IX, 1) = SPAR-IDLEVNR  OR                     
063200           3202-IDLEVNR(URV-IX, 2) = SPAR-IDLEVNR  OR                     
063300           3202-IDLEVNR(URV-IX, 3) = SPAR-IDLEVNR  OR                     
063400           3202-IDLEVNR(URV-IX, 4) = SPAR-IDLEVNR  OR                     
063500           3202-IDLEVNR(URV-IX, 5) = SPAR-IDLEVNR  OR                     
063600           3202-IDLEVNR(URV-IX, 6) = SPAR-IDLEVNR  OR                     
063700           3202-IDLEVNR(URV-IX, 7) = SPAR-IDLEVNR  OR                     
063800           3202-IDLEVNR(URV-IX, 8) = SPAR-IDLEVNR                         
063900           MOVE JA TO LEVERANT-FINNS                                      
064000        END-IF                                                            
064100     END-IF                                                               
064200     .                                                                    
064300     EJECT                                                                
064400 S02AAEA-LEVNR-NOLL-KOLL  SECTION.                                        
064500                                                                          
064600     IF 3202-IDLEVNR(URV-IX, 1)  NOT = SPACE                              
064700       CONTINUE                                                           
064800     ELSE                                                                 
064900       MOVE '99999' TO  3202-IDLEVNR(URV-IX, 1)                           
065000     END-IF                                                               
065100                                                                          
065200     IF 3202-IDLEVNR(URV-IX, 2)  NOT = SPACE                              
065300       CONTINUE                                                           
065400     ELSE                                                                 
065500       MOVE '99999' TO  3202-IDLEVNR(URV-IX, 2)                           
065600     END-IF                                                               
065700                                                                          
065800     IF 3202-IDLEVNR(URV-IX, 3)  NOT = SPACE                              
065900       CONTINUE                                                           
066000     ELSE                                                                 
066100       MOVE '99999' TO  3202-IDLEVNR(URV-IX, 3)                           
066200     END-IF                                                               
066300                                                                          
066400     IF 3202-IDLEVNR(URV-IX, 4)  NOT = SPACE                              
066500       CONTINUE                                                           
066600     ELSE                                                                 
066700       MOVE '99999' TO  3202-IDLEVNR(URV-IX, 4)                           
066800     END-IF                                                               
066900                                                                          
067000     IF 3202-IDLEVNR(URV-IX, 5)  NOT = SPACE                              
067100       CONTINUE                                                           
067200     ELSE                                                                 
067300       MOVE '99999' TO  3202-IDLEVNR(URV-IX, 5)                           
067400     END-IF                                                               
067500                                                                          
067600     IF 3202-IDLEVNR(URV-IX, 6)  NOT = SPACE                              
067700       CONTINUE                                                           
067800     ELSE                                                                 
067900       MOVE '99999' TO  3202-IDLEVNR(URV-IX, 6)                           
068000     END-IF                                                               
068100                                                                          
068200     IF 3202-IDLEVNR(URV-IX, 7)  NOT = SPACE                              
068300       CONTINUE                                                           
068400     ELSE                                                                 
068500       MOVE '99999' TO  3202-IDLEVNR(URV-IX, 7)                           
068600     END-IF                                                               
068700                                                                          
068800     IF 3202-IDLEVNR(URV-IX, 8)  NOT = SPACE                              
068900       CONTINUE                                                           
069000     ELSE                                                                 
069100       MOVE '99999' TO  3202-IDLEVNR(URV-IX, 8)                           
069200     END-IF                                                               
069300                                                                          
069400     .                                                                    
069500     EJECT                                                                
069600 S02AAF-KOLLA-LKTO SECTION.                                               
069700                                                                          
069800                                                                          
069900     MOVE NEJ TO LKTO-FINNS                                               
070000     IF 3202-IDLKTO(URV-IX, 1) = ZERO AND                                 
070100        3202-IDLKTO(URV-IX, 2) = ZERO AND                                 
070200        3202-IDLKTO(URV-IX, 3) = ZERO AND                                 
070300        3202-IDLKTO(URV-IX, 4) = ZERO AND                                 
070400        3202-IDLKTO(URV-IX, 5) = ZERO AND                                 
070500        3202-IDLKTO(URV-IX, 6) = ZERO AND                                 
070600        3202-IDLKTO(URV-IX, 7) = ZERO AND                                 
070700        3202-IDLKTO(URV-IX, 8) = ZERO                                     
070800        MOVE JA TO LKTO-FINNS                                             
070900     ELSE                                                                 
071000        IF 3202-IDLKTO(URV-IX, 1) = SPAR-IDLKTO  OR                       
071100           3202-IDLKTO(URV-IX, 2) = SPAR-IDLKTO  OR                       
071200           3202-IDLKTO(URV-IX, 3) = SPAR-IDLKTO  OR                       
071300           3202-IDLKTO(URV-IX, 4) = SPAR-IDLKTO  OR                       
071400           3202-IDLKTO(URV-IX, 5) = SPAR-IDLKTO  OR                       
071500           3202-IDLKTO(URV-IX, 6) = SPAR-IDLKTO  OR                       
071600           3202-IDLKTO(URV-IX, 7) = SPAR-IDLKTO  OR                       
071700           3202-IDLKTO(URV-IX, 8) = SPAR-IDLKTO                           
071800           MOVE JA TO LKTO-FINNS                                          
071900        END-IF                                                            
072000     END-IF                                                               
072100     .                                                                    
072200     EJECT                                                                
072300 S02AB-KOLLA-EXTRA SECTION.                                               
072400                                                                          
072500     PERFORM S02ABA-KOLLA-MARKNAD                                         
072600     IF MARKNAD-FINNS = JA                                                
072700        PERFORM S02ABB-KOLLA-DISTRIKT                                     
072800        IF DISTRIKT-FINNS = JA                                            
072900           PERFORM S02ABC-KOLLA-ANSK                                      
073000           IF ANSK-FINNS = JA                                             
073100              PERFORM S02ABD-KOLLA-FUNKGRP                                
073200              IF FUNKGRP-FINNS = JA                                       
073300                 MOVE JA TO EXTRA-FINNS                                   
073400              END-IF                                                      
073500           END-IF                                                         
073600        END-IF                                                            
073700     END-IF                                                               
073800     .                                                                    
073900     EJECT                                                                
074000 S02ABA-KOLLA-MARKNAD SECTION.                                            
074100                                                                          
074200                                                                          
074300     MOVE NEJ TO MARKNAD-FINNS                                            
074400     IF 3202-KDMARK-BUDG-FOM(URV-IX, 1) = ZERO AND                        
074500        3202-KDMARK-BUDG-FOM(URV-IX, 2) = ZERO AND                        
074600        3202-KDMARK-BUDG-FOM(URV-IX, 3) = ZERO AND                        
074700        3202-KDMARK-BUDG-FOM(URV-IX, 4) = ZERO AND                        
074800        3202-KDMARK-BUDG-FOM(URV-IX, 5) = ZERO AND                        
074900        3202-KDMARK-BUDG-FOM(URV-IX, 6) = ZERO AND                        
075000        3202-KDMARK-BUDG-FOM(URV-IX, 7) = ZERO AND                        
075100        3202-KDMARK-BUDG-FOM(URV-IX, 8) = ZERO AND                        
075200        3202-KDMARK-BUDG-TOM(URV-IX, 1) = ZERO AND                        
075300        3202-KDMARK-BUDG-TOM(URV-IX, 2) = ZERO AND                        
075400        3202-KDMARK-BUDG-TOM(URV-IX, 3) = ZERO AND                        
075500        3202-KDMARK-BUDG-TOM(URV-IX, 4) = ZERO AND                        
075600        3202-KDMARK-BUDG-TOM(URV-IX, 5) = ZERO AND                        
075700        3202-KDMARK-BUDG-TOM(URV-IX, 6) = ZERO AND                        
075800        3202-KDMARK-BUDG-TOM(URV-IX, 7) = ZERO AND                        
075900        3202-KDMARK-BUDG-TOM(URV-IX, 8) = ZERO                            
076000        MOVE JA TO MARKNAD-FINNS                                          
076100     ELSE                                                                 
076200        IF (KLAR-KDMARK-BUDG NOT <                                        
076300            3202-KDMARK-BUDG-FOM(URV-IX, 1)  AND                          
076400            KLAR-KDMARK-BUDG NOT >                                        
076500            3202-KDMARK-BUDG-TOM(URV-IX, 1))  OR                          
076600           (KLAR-KDMARK-BUDG NOT <                                        
076700            3202-KDMARK-BUDG-FOM(URV-IX, 2)  AND                          
076800            KLAR-KDMARK-BUDG NOT >                                        
076900            3202-KDMARK-BUDG-TOM(URV-IX, 2))  OR                          
077000           (KLAR-KDMARK-BUDG NOT <                                        
077100            3202-KDMARK-BUDG-FOM(URV-IX, 3)  AND                          
077200            KLAR-KDMARK-BUDG NOT >                                        
077300            3202-KDMARK-BUDG-TOM(URV-IX, 3))  OR                          
077400           (KLAR-KDMARK-BUDG NOT <                                        
077500            3202-KDMARK-BUDG-FOM(URV-IX, 4)  AND                          
077600            KLAR-KDMARK-BUDG NOT >                                        
077700            3202-KDMARK-BUDG-TOM(URV-IX, 4))  OR                          
077800           (KLAR-KDMARK-BUDG NOT <                                        
077900            3202-KDMARK-BUDG-FOM(URV-IX, 5)  AND                          
078000            KLAR-KDMARK-BUDG NOT >                                        
078100            3202-KDMARK-BUDG-TOM(URV-IX, 5))  OR                          
078200           (KLAR-KDMARK-BUDG NOT <                                        
078300            3202-KDMARK-BUDG-FOM(URV-IX, 6)  AND                          
078400            KLAR-KDMARK-BUDG NOT >                                        
078500            3202-KDMARK-BUDG-TOM(URV-IX, 6))  OR                          
078600           (KLAR-KDMARK-BUDG NOT <                                        
078700            3202-KDMARK-BUDG-FOM(URV-IX, 7)  AND                          
078800            KLAR-KDMARK-BUDG NOT >                                        
078900            3202-KDMARK-BUDG-TOM(URV-IX, 7))  OR                          
079000           (KLAR-KDMARK-BUDG NOT <                                        
079100            3202-KDMARK-BUDG-FOM(URV-IX, 8)  AND                          
079200            KLAR-KDMARK-BUDG NOT >                                        
079300            3202-KDMARK-BUDG-TOM(URV-IX, 8))                              
079400           MOVE JA TO MARKNAD-FINNS                                       
079500        END-IF                                                            
079600     END-IF                                                               
079700     .                                                                    
079800     EJECT                                                                
079900 S02ABB-KOLLA-DISTRIKT SECTION.                                           
080000                                                                          
080100     MOVE NEJ TO DISTRIKT-FINNS                                           
080200     IF 3202-IDDISTR-FOM(URV-IX, 1) = ZERO AND                            
080300        3202-IDDISTR-FOM(URV-IX, 2) = ZERO AND                            
080400        3202-IDDISTR-FOM(URV-IX, 3) = ZERO AND                            
080500        3202-IDDISTR-FOM(URV-IX, 4) = ZERO AND                            
080600        3202-IDDISTR-TOM(URV-IX, 1) = ZERO AND                            
080700        3202-IDDISTR-TOM(URV-IX, 2) = ZERO AND                            
080800        3202-IDDISTR-TOM(URV-IX, 3) = ZERO AND                            
080900        3202-IDDISTR-TOM(URV-IX, 4) = ZERO                                
081000        MOVE JA TO DISTRIKT-FINNS                                         
081100     ELSE                                                                 
081200      IF (KLAR-IDDISTR NOT < 3202-IDDISTR-FOM(URV-IX, 1)  AND             
081300          KLAR-IDDISTR NOT > 3202-IDDISTR-TOM(URV-IX, 1))  OR             
081400         (KLAR-IDDISTR NOT < 3202-IDDISTR-FOM(URV-IX, 2)  AND             
081500          KLAR-IDDISTR NOT > 3202-IDDISTR-TOM(URV-IX, 2))  OR             
081600         (KLAR-IDDISTR NOT < 3202-IDDISTR-FOM(URV-IX, 3)  AND             
081700          KLAR-IDDISTR NOT > 3202-IDDISTR-TOM(URV-IX, 3))  OR             
081800         (KLAR-IDDISTR NOT < 3202-IDDISTR-FOM(URV-IX, 4)  AND             
081900          KLAR-IDDISTR NOT > 3202-IDDISTR-TOM(URV-IX, 4))                 
082000        MOVE JA TO DISTRIKT-FINNS                                         
082100      END-IF                                                              
082200     END-IF                                                               
082300     .                                                                    
082400     EJECT                                                                
082500 S02ABC-KOLLA-ANSK SECTION.                                               
082600                                                                          
082700                                                                          
082800     MOVE NEJ TO ANSK-FINNS                                               
082900     IF 3202-IDANSK-FOM(URV-IX, 1) = ZERO AND                             
083000        3202-IDANSK-FOM(URV-IX, 2) = ZERO AND                             
083100        3202-IDANSK-FOM(URV-IX, 3) = ZERO AND                             
083200        3202-IDANSK-FOM(URV-IX, 4) = ZERO AND                             
083300        3202-IDANSK-TOM(URV-IX, 1) = ZERO AND                             
083400        3202-IDANSK-TOM(URV-IX, 2) = ZERO AND                             
083500        3202-IDANSK-TOM(URV-IX, 3) = ZERO AND                             
083600        3202-IDANSK-TOM(URV-IX, 4) = ZERO                                 
083700        MOVE JA TO ANSK-FINNS                                             
083800     ELSE                                                                 
083900        IF (SPAR-IDANSK NOT < 3202-IDANSK-FOM(URV-IX, 1)  AND             
084000            SPAR-IDANSK NOT > 3202-IDANSK-TOM(URV-IX, 1)) OR              
084100           (SPAR-IDANSK NOT < 3202-IDANSK-FOM(URV-IX, 2)  AND             
084200            SPAR-IDANSK NOT > 3202-IDANSK-TOM(URV-IX, 2)) OR              
084300           (SPAR-IDANSK NOT < 3202-IDANSK-FOM(URV-IX, 3)  AND             
084400            SPAR-IDANSK NOT > 3202-IDANSK-TOM(URV-IX, 3)) OR              
084500           (SPAR-IDANSK NOT < 3202-IDANSK-FOM(URV-IX, 4)  AND             
084600            SPAR-IDANSK NOT > 3202-IDANSK-TOM(URV-IX, 4))                 
084700           MOVE JA TO ANSK-FINNS                                          
084800        END-IF                                                            
084900     END-IF                                                               
085000     .                                                                    
085100     EJECT                                                                
085200 S02ABD-KOLLA-FUNKGRP SECTION.                                            
085300                                                                          
085400     MOVE NEJ TO FUNKGRP-FINNS                                            
085500     MOVE +1 TO ARB-IX                                                    
085600     PERFORM UNTIL ARB-IX > 99 OR FUNKGRP-FINNS = JA                      
085700        IF (3202-IDFKNGRP-FOM(URV-IX, ARB-IX) = ZERO) AND                 
085800           (3202-IDFKNGRP-TOM(URV-IX, ARB-IX) = ZERO)                     
085900           MOVE +99 TO ARB-IX                                             
086000        ELSE                                                              
086100          IF (KLAR-IDFKNGRP > 3202-IDFKNGRP-FOM(URV-IX, ARB-IX))          
086200          OR (KLAR-IDFKNGRP = 3202-IDFKNGRP-FOM(URV-IX, ARB-IX))          
086300            IF (KLAR-IDFKNGRP < 3202-IDFKNGRP-TOM(URV-IX, ARB-IX))        
086400            OR (KLAR-IDFKNGRP = 3202-IDFKNGRP-TOM(URV-IX, ARB-IX))        
086500               MOVE JA TO FUNKGRP-FINNS                                   
086600            END-IF                                                        
086700          END-IF                                                          
086800        END-IF                                                            
086900        ADD +1 TO ARB-IX                                                  
087000     END-PERFORM                                                          
087100     .                                                                    
087200     EJECT                                                                
087300 S02B-KOLLA-3203-URVAL SECTION.                                           
087400                                                                          
087500     MOVE +1 TO URV-IX                                                    
087600     PERFORM UNTIL (3203-IDUSER(URV-IX) = SPACE) OR                       
087700        (URV-IX > 100)                                                    
087800        MOVE 3203-DAFSGVV-FOM(URV-IX) TO WS-DAFSGVV-FOM                   
087900        MOVE WS-TIAAVV-FOM TO WS-TIAAVV-FOM-FAAR                          
088000        SUBTRACT 1 FROM WS-TIAA-FOM-FAAR                                  
088100        MOVE 3203-DAFSGVV-TOM(URV-IX) TO WS-DAFSGVV-TOM                   
088200        MOVE WS-TIAAVV-TOM TO WS-TIAAVV-TOM-FAAR                          
088300        SUBTRACT 1 FROM WS-TIAA-TOM-FAAR                                  
088400        MOVE KLAR-DAFSGVV     TO WS-DAFSGVV                               
088510        IF WS-TIAAVV > WS-TIAAVV-TOM                                      
088600           CONTINUE                                                       
088700        ELSE                                                              
088810           IF WS-TIAAVV < WS-TIAAVV-FOM-FAAR                              
088900              CONTINUE                                                    
089000           ELSE                                                           
089210              IF WS-TIAAVV > WS-TIAAVV-TOM-FAAR AND                       
089220                 WS-TIAAVV < WS-TIAAVV-FOM                                
089300                 CONTINUE                                                 
089400              ELSE                                                        
089500                 MOVE NEJ TO GRUNDURV-FINNS                               
089600                 PERFORM S02BA-KOLLA-GRUNDURV-FINNS                       
089700                 IF GRUNDURV-FINNS = JA                                   
089800                    MOVE NEJ TO ARTIKEL-FINNS                             
089900                    PERFORM S02BB-KOLLA-ARTIKEL-FINNS                     
090000                    IF ARTIKEL-FINNS = JA                                 
090100                       MOVE 3203-001-GRUPP(URV-IX) TO                     
090200                       KLAR-001-GRUPP                                     
090300                       PERFORM S05-SKRIV-UTPOST                           
090400                    END-IF                                                
090500                 END-IF                                                   
090600              END-IF                                                      
090700           END-IF                                                         
090800        END-IF                                                            
090900        ADD +1 TO URV-IX                                                  
091000     END-PERFORM                                                          
091100     .                                                                    
091200     EJECT                                                                
091300 S02BA-KOLLA-GRUNDURV-FINNS SECTION.                                      
091400                                                                          
091500     MOVE 3203-KDPRTYPG(URV-IX) TO SPAR-KDPRTYPG                          
091600     PERFORM S020-KOLLA-PRISTYP                                           
091700     IF PRISTYP-FINNS = JA                                                
092000        PERFORM S02BAB-KOLLA-MARKNAD                                      
092100        IF MARKNAD-FINNS = JA                                             
092200           PERFORM S02BAC-KOLLA-DISTRIKT                                  
092300           IF DISTRIKT-FINNS = JA                                         
092400              MOVE JA TO GRUNDURV-FINNS                                   
092500           END-IF                                                         
092600        END-IF                                                            
092800     END-IF                                                               
092900     .                                                                    
093000     EJECT                                                                
095800 S02BAB-KOLLA-MARKNAD SECTION.                                            
095900                                                                          
096000     MOVE NEJ TO MARKNAD-FINNS                                            
096100     IF 3203-KDMARK-BUDG-FOM(URV-IX, 1) = ZERO AND                        
096200        3203-KDMARK-BUDG-FOM(URV-IX, 2) = ZERO AND                        
096300        3203-KDMARK-BUDG-FOM(URV-IX, 3) = ZERO AND                        
096400        3203-KDMARK-BUDG-FOM(URV-IX, 4) = ZERO AND                        
096500        3203-KDMARK-BUDG-FOM(URV-IX, 5) = ZERO AND                        
096600        3203-KDMARK-BUDG-FOM(URV-IX, 6) = ZERO AND                        
096700        3203-KDMARK-BUDG-FOM(URV-IX, 7) = ZERO AND                        
096800        3203-KDMARK-BUDG-FOM(URV-IX, 8) = ZERO AND                        
096900        3203-KDMARK-BUDG-TOM(URV-IX, 1) = ZERO AND                        
097000        3203-KDMARK-BUDG-TOM(URV-IX, 2) = ZERO AND                        
097100        3203-KDMARK-BUDG-TOM(URV-IX, 3) = ZERO AND                        
097200        3203-KDMARK-BUDG-TOM(URV-IX, 4) = ZERO AND                        
097300        3203-KDMARK-BUDG-TOM(URV-IX, 5) = ZERO AND                        
097400        3203-KDMARK-BUDG-TOM(URV-IX, 6) = ZERO AND                        
097500        3203-KDMARK-BUDG-TOM(URV-IX, 7) = ZERO AND                        
097600        3203-KDMARK-BUDG-TOM(URV-IX, 8) = ZERO                            
097700        MOVE JA TO MARKNAD-FINNS                                          
097800     ELSE                                                                 
097900        IF (KLAR-KDMARK-BUDG NOT <                                        
098000            3203-KDMARK-BUDG-FOM(URV-IX, 1)  AND                          
098100            KLAR-KDMARK-BUDG NOT >                                        
098200            3203-KDMARK-BUDG-TOM(URV-IX, 1))  OR                          
098300           (KLAR-KDMARK-BUDG NOT <                                        
098400            3203-KDMARK-BUDG-FOM(URV-IX, 2)  AND                          
098500            KLAR-KDMARK-BUDG NOT >                                        
098600            3203-KDMARK-BUDG-TOM(URV-IX, 2))  OR                          
098700           (KLAR-KDMARK-BUDG NOT <                                        
098800            3203-KDMARK-BUDG-FOM(URV-IX, 3)  AND                          
098900            KLAR-KDMARK-BUDG NOT >                                        
099000            3203-KDMARK-BUDG-TOM(URV-IX, 3))  OR                          
099100           (KLAR-KDMARK-BUDG NOT <                                        
099200            3203-KDMARK-BUDG-FOM(URV-IX, 4)  AND                          
099300            KLAR-KDMARK-BUDG NOT >                                        
099400            3203-KDMARK-BUDG-TOM(URV-IX, 4))  OR                          
099500           (KLAR-KDMARK-BUDG NOT <                                        
099600            3203-KDMARK-BUDG-FOM(URV-IX, 5)  AND                          
099700            KLAR-KDMARK-BUDG NOT >                                        
099800            3203-KDMARK-BUDG-TOM(URV-IX, 5))  OR                          
099900           (KLAR-KDMARK-BUDG NOT <                                        
100000            3203-KDMARK-BUDG-FOM(URV-IX, 6)  AND                          
100100            KLAR-KDMARK-BUDG NOT >                                        
100200            3203-KDMARK-BUDG-TOM(URV-IX, 6))  OR                          
100300           (KLAR-KDMARK-BUDG NOT <                                        
100400            3203-KDMARK-BUDG-FOM(URV-IX, 7)  AND                          
100500            KLAR-KDMARK-BUDG NOT >                                        
100600            3203-KDMARK-BUDG-TOM(URV-IX, 7))  OR                          
100700           (KLAR-KDMARK-BUDG NOT <                                        
100800            3203-KDMARK-BUDG-FOM(URV-IX, 8)  AND                          
100900            KLAR-KDMARK-BUDG NOT >                                        
101000            3203-KDMARK-BUDG-TOM(URV-IX, 8))                              
101100           MOVE JA TO MARKNAD-FINNS                                       
101200        END-IF                                                            
101300     END-IF                                                               
101400     .                                                                    
101500     EJECT                                                                
101600 S02BAC-KOLLA-DISTRIKT SECTION.                                           
101700                                                                          
101800     MOVE NEJ TO DISTRIKT-FINNS                                           
101900     IF 3203-IDDISTR-FOM(URV-IX, 1) = ZERO AND                            
102000        3203-IDDISTR-FOM(URV-IX, 2) = ZERO AND                            
102100        3203-IDDISTR-FOM(URV-IX, 3) = ZERO AND                            
102200        3203-IDDISTR-FOM(URV-IX, 4) = ZERO AND                            
102300        3203-IDDISTR-TOM(URV-IX, 1) = ZERO AND                            
102400        3203-IDDISTR-TOM(URV-IX, 2) = ZERO AND                            
102500        3203-IDDISTR-TOM(URV-IX, 3) = ZERO AND                            
102600        3203-IDDISTR-TOM(URV-IX, 4) = ZERO                                
102700        MOVE JA TO DISTRIKT-FINNS                                         
102800     ELSE                                                                 
102900      IF (KLAR-IDDISTR NOT < 3203-IDDISTR-FOM(URV-IX, 1)  AND             
103000          KLAR-IDDISTR NOT > 3203-IDDISTR-TOM(URV-IX, 1))  OR             
103100         (KLAR-IDDISTR NOT < 3203-IDDISTR-FOM(URV-IX, 2)  AND             
103200          KLAR-IDDISTR NOT > 3203-IDDISTR-TOM(URV-IX, 2))  OR             
103300         (KLAR-IDDISTR NOT < 3203-IDDISTR-FOM(URV-IX, 3)  AND             
103400          KLAR-IDDISTR NOT > 3203-IDDISTR-TOM(URV-IX, 3))  OR             
103500         (KLAR-IDDISTR NOT < 3203-IDDISTR-FOM(URV-IX, 4)  AND             
103600          KLAR-IDDISTR NOT > 3203-IDDISTR-TOM(URV-IX, 4))                 
103700        MOVE JA TO DISTRIKT-FINNS                                         
103800      END-IF                                                              
103900     END-IF                                                               
104000     .                                                                    
104100     EJECT                                                                
104200 S02BB-KOLLA-ARTIKEL-FINNS SECTION.                                       
104300                                                                          
104400     MOVE +1 TO ARB-IX                                                    
104500     PERFORM UNTIL ARTIKEL-FINNS = JA OR ARB-IX > 500                     
104600        IF 3203-IDARTNR(URV-IX, ARB-IX) > KLAR-IDARTNR                    
104700           MOVE +500 TO ARB-IX                                            
104800        ELSE                                                              
104900           IF 3203-IDARTNR(URV-IX, ARB-IX) = ZERO                         
105000              MOVE +500 TO ARB-IX                                         
105100           ELSE                                                           
105200              IF 3203-IDARTNR(URV-IX, ARB-IX) = KLAR-IDARTNR              
105300                 MOVE JA TO ARTIKEL-FINNS                                 
105400              END-IF                                                      
105500           END-IF                                                         
105600        END-IF                                                            
105700        ADD +1 TO ARB-IX                                                  
105800     END-PERFORM                                                          
105900     .                                                                    
106000     EJECT                                                                
106100 S03-LAS-URVALSFIL SECTION.                                               
106200                                                                          
106300     READ W33035 INTO IN-AREA                                             
106400     AT END                                                               
106500       MOVE JA TO URVALSFIL-EOF                                           
106600     END-READ                                                             
106700                                                                          
106800     IF URVALSFIL-EOF = NEJ                                               
106900       ADD +1 TO TOT-RAKNARE-URV                                          
107000     END-IF                                                               
107100     .                                                                    
107200     EJECT                                                                
107300 S05-SKRIV-UTPOST SECTION.                                                
107400                                                                          
107500     MOVE +5 TO KLAR-IDGTYP                                               
107600     WRITE UT371-POST FROM KLAR-AREA                                      
107700     ADD +1 TO TOT-RAKNARE-UT                                             
107800     .                                                                    
107900     EJECT                                                                
108000 S06-LAS-INFOFIL SECTION.                                                 
108100                                                                          
108200     READ W33019 INTO INFO-AREA                                           
108300     AT END                                                               
108400       MOVE JA TO INFOFIL-EOF                                             
108500     END-READ                                                             
108600                                                                          
108700     IF INFOFIL-EOF = NEJ                                                 
108800       ADD +1 TO TOT-RAKNARE-INFO                                         
108900     END-IF                                                               
109000     .                                                                    
109100     EJECT                                                                
109200 S020-KOLLA-PRISTYP SECTION.                                              
109300                                                                          
109400     MOVE NEJ TO PRISTYP-FINNS                                            
109500     EVALUATE SPAR-KDPRTYPG                                               
109600       WHEN 'S' PERFORM S020A-SPECTYP                                     
109700       WHEN 'F' PERFORM S020B-FUNKTYP                                     
109800       WHEN 'M' PERFORM S020C-MANTYP                                      
109900       WHEN 'K' PERFORM S020D-KRETYP                                      
110000       WHEN 'R' PERFORM S020E-SPECFUNKTYP                                 
110100       WHEN ' ' PERFORM S020F-BLANKTYP                                    
110200     END-EVALUATE                                                         
110300     .                                                                    
110400     EJECT                                                                
110500 S020A-SPECTYP SECTION.                                                   
110600                                                                          
110700     IF WS-SULEVANT-SPEC > ZERO                                           
110800        MOVE WS-SULEVANT-SPEC TO KLAR-SULEVANT                            
110900        MOVE WS-SUARTFSG-SPEC TO KLAR-SUARTFSG                            
111000        MOVE JA TO PRISTYP-FINNS                                          
111100     END-IF                                                               
111200     .                                                                    
111300     EJECT                                                                
111400 S020B-FUNKTYP SECTION.                                                   
111500                                                                          
111600     IF WS-SULEVANT-RAB > ZERO                                            
111700        MOVE WS-SULEVANT-RAB  TO KLAR-SULEVANT                            
111800        MOVE WS-SUARTFSG-RAB  TO KLAR-SUARTFSG                            
111900        MOVE JA TO PRISTYP-FINNS                                          
112000     END-IF                                                               
112100     .                                                                    
112200     EJECT                                                                
112300 S020C-MANTYP SECTION.                                                    
112400                                                                          
112500     IF WS-SULEVANT-MAN > ZERO                                            
112600        MOVE WS-SULEVANT-MAN  TO KLAR-SULEVANT                            
112700        MOVE WS-SUARTFSG-MAN  TO KLAR-SUARTFSG                            
112800        MOVE JA TO PRISTYP-FINNS                                          
112900     END-IF                                                               
113000     .                                                                    
113100     EJECT                                                                
113200 S020D-KRETYP SECTION.                                                    
113300                                                                          
113400     IF WS-SULEVANT-KRE > ZERO                                            
113500        MOVE WS-SULEVANT-KRE  TO KLAR-SULEVANT                            
113600        MOVE WS-SUARTFSG-KRE  TO KLAR-SUARTFSG                            
113700        MOVE JA TO PRISTYP-FINNS                                          
113800     END-IF                                                               
113900     .                                                                    
114000     EJECT                                                                
114100 S020E-SPECFUNKTYP SECTION.                                               
114200                                                                          
114300     COMPUTE WS-SULEVANT-RABSPEC = WS-SULEVANT-SPEC +                     
114400             WS-SULEVANT-RAB                                              
114500     IF WS-SULEVANT-RABSPEC > ZERO                                        
114600       COMPUTE WS-SUARTFSG-RABSPEC = WS-SUARTFSG-SPEC +                   
114700               WS-SUARTFSG-RAB                                            
114800        MOVE WS-SULEVANT-RABSPEC TO KLAR-SULEVANT                         
114900        MOVE WS-SUARTFSG-RABSPEC TO KLAR-SUARTFSG                         
115000        MOVE JA TO PRISTYP-FINNS                                          
115100     END-IF                                                               
115200     .                                                                    
115300     EJECT                                                                
115400 S020F-BLANKTYP SECTION.                                                  
115500                                                                          
115600     MOVE WS-SULEVANT TO KLAR-SULEVANT                                    
115700     MOVE WS-SUARTFSG TO KLAR-SUARTFSG                                    
115800     MOVE JA TO PRISTYP-FINNS                                             
115900     .                                                                    
116000     EJECT                                                                
116100 S030-NOLLSTAELL-AREA SECTION.                                            
116200                                                                          
116300     MOVE ZERO TO WS-SULEVANT                                             
116400     MOVE ZERO TO WS-SULEVANT-RAB                                         
116500     MOVE ZERO TO WS-SULEVANT-SPEC                                        
116600     MOVE ZERO TO WS-SULEVANT-MAN                                         
116700     MOVE ZERO TO WS-SULEVANT-KRE                                         
116800     MOVE ZERO TO WS-SULEVANT-RABSPEC                                     
116900     MOVE ZERO TO WS-SUARTFSG                                             
117000     MOVE ZERO TO WS-SUARTFSG-RAB                                         
117100     MOVE ZERO TO WS-SUARTFSG-SPEC                                        
117200     MOVE ZERO TO WS-SUARTFSG-MAN                                         
117300     MOVE ZERO TO WS-SUARTFSG-KRE                                         
117400     MOVE ZERO TO WS-SUARTFSG-RABSPEC                                     
117500     .                                                                    
117600     EJECT                                                                
117700 Z-FINIT SECTION.                                                         
117800                                                                          
117900     CLOSE W33035                                                         
118000           W33013                                                         
118100           W33019                                                         
118200           W33071                                                         
118300                                                                          
118400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
118500     MOVE 'T'                TO POSTSUM-OPKOD                             
118600                                                                          
118700     MOVE 'URV '             TO POSTSUM-TRANSTYP                          
118800     MOVE 'W33035'           TO POSTSUM-FDNAMN                            
118900     MOVE 'W33070D1'         TO POSTSUM-DDNAMN2                           
119000     MOVE TOT-RAKNARE-URV    TO POSTSUM-TOTTRANS                          
119100     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
119200                                                                          
119300     MOVE 'WEEK'             TO POSTSUM-TRANSTYP                          
119400     MOVE 'W33013'           TO POSTSUM-FDNAMN                            
119500     MOVE 'W33070D2'         TO POSTSUM-DDNAMN2                           
119600     MOVE TOT-RAKNARE-WEEK   TO POSTSUM-TOTTRANS                          
119700     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
119800                                                                          
119900     MOVE 'INFO'             TO POSTSUM-TRANSTYP                          
120000     MOVE 'W33019'           TO POSTSUM-FDNAMN                            
120100     MOVE 'W33070D3'         TO POSTSUM-DDNAMN2                           
120200     MOVE TOT-RAKNARE-INFO   TO POSTSUM-TOTTRANS                          
120300     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
120400                                                                          
120500     MOVE 'UT  '             TO POSTSUM-TRANSTYP                          
120600     MOVE 'W33071'           TO POSTSUM-FDNAMN                            
120700     MOVE 'W33070D4'         TO POSTSUM-DDNAMN2                           
120800     MOVE TOT-RAKNARE-UT     TO POSTSUM-TOTTRANS                          
120900     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
121000                                                                          
121100     MOVE 'S'                TO POSTSUM-OPKOD                             
121200     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
121300     .                                                                    
121310     EJECT                                                                
121410     EJECT                                                                
