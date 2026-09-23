000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3354200.                                                
000400*AUTHOR.         RONNY STENHOLM                                           
000500*DATE-WRITTEN.   APRIL 1994.                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*          FÖR VCNA : OBS DET FINNS INGET SJK TILL VCNA                   
001100*                     DET FINNS ISTÄLLET ETT PRARTVNA SOM                 
001200*                     ÄR SJK ELLER LEV:S BESTÄLLNINGSPRIS                 
001300*          PROGRAMMET LÄSER FIL INEHÅLLANDE ARTIKELINFO TILL              
001400*        MARKNADSBOLAG VCNA.AV DENNA FIL SKAPAS "LAGOM STOR" UTFIL        
001500*        SOM SKICKAS VIA VCOM. EV. RESTERANDE POSTER SKRIVS SOM           
001600*        NY GENERATION AV INFILEN.                                        
001700*          OM RESTERANDE POSTER FINNS LÄMNAS RETURKOD 8.RETURKODEN        
001800*        TESTAS SEDAN I JCL OCH OM DEN ÄR 8 BESTÄLLS JOBBET               
001900*        IGEN OCH DEN NYA GENERATIONEN TAS IN FÖR ATT                     
002000*        KUNNA SKICKA RESTERANDE POSTER OSV.                              
002100*                                                                         
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 - FEL COPYTEXTVERSION                                      
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700*    CHANGE LOG:                                                          
002800*                                                                         
002900*    DVK  17/04/02                                                        
003000*    FILE LAYOUT OF W33544V IS CHANGED TO HAVE THE IDLEVNR AND            
003100*    SLAG-IDLEVNR IN COMP-3 AND ALPHANUMERIC FORMAT. NEW COPY BOOK        
003200*    W335404B IS USED INSTEAD OF W335404A FOR W33544V.                    
003300*    MAX-POSTER IS CHANGED FROM 60000 TO 58000 SINCE RECORD LENGTH        
003400*    IS CHANGED FROM 164 TO 170                                           
003500*                                                                         
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     SKIP2                                                                
004000 INPUT-OUTPUT SECTION.                                                    
004100                                                                          
004200 FILE-CONTROL.                                                            
004300     SKIP2                                                                
004400*          --- FIL MED FÖRÄNDR. PÅ ART.REG TILL MB   VCNA                 
004500*          --- POSTER ATT SÄNDA TILL MB.                                  
004600     SELECT W33544I                    ASSIGN TO W33542D2.                
004700     SKIP2                                                                
004800*          --- FIL MED FÖRÄNDR. PÅ ART.REG TILL MB    VCNA                
004900*          --- POSTER KVAR ATT SÄNDA EFTER DENNA SÄNDNING.                
005000     SELECT W33544U                    ASSIGN TO W33542D3.                
005100     SKIP2                                                                
005200*          --- FIL MED FÖRÄNDR. PÅ ART.REG TILL MB   VCNA                 
005300*          --- 'DEL-FIL' ATT SÄNDA VIA VCOM                               
005400     SELECT W33544V                    ASSIGN TO W33542D4.                
005500     SKIP2                                                                
005600*          --- FIL INNEHÅLLANDE EN POST.                                  
005700*          --- DENNA POST INNEHÅLLER DET TOTALA ANTALET TRANSAR.          
005800     SELECT W33543                     ASSIGN TO W33542D5.                
005900     EJECT                                                                
006000 DATA DIVISION.                                                           
006100     SKIP3                                                                
006200 FILE SECTION.                                                            
006300     SKIP3                                                                
006400 FD  W33544I                                                              
006500     RECORDING       V                                                    
006600     BLOCK CONTAINS  0.                                                   
006700     SKIP2                                                                
006800*01  -COPY W335404A      -L.                                              
006900     SKIP3                                                                
007000*01  -COPY W335402A      -L.                                              
007100     SKIP3                                                                
007200*01  -COPY W335403A      -L.                                              
007300     SKIP3                                                                
007400 FD  W33544U                                                              
007500     RECORDING       V                                                    
007600     BLOCK CONTAINS  0.                                                   
007700     SKIP2                                                                
007800*01  POST -COPY W335404A -PRE  UT1-  -L.                                  
007900*01  POST -COPY W335402A -PRE  UT2-  -L.                                  
008000*01  POST -COPY W335403A -PRE  UT3-  -L.                                  
008100     SKIP3                                                                
008200 FD  W33544V                                                              
008300     RECORDING       V                                                    
008400     BLOCK CONTAINS  0.                                                   
008500     SKIP2                                                                
008600*01  POST -COPY W335404B -PRE  UTVCOM1-  -L.                              
008700*01  POST -COPY W335400A -PRE  UTVCOM0-  -L.                              
008800*01  POST -COPY W335402A -PRE  UTVCOM2-  -L.                              
008900*01  POST -COPY W335403A -PRE  UTVCOM3-  -L.                              
009000     EJECT                                                                
009100 FD  W33543                                                               
009200     RECORDING       F                                                    
009300     BLOCK CONTAINS  0.                                                   
009400     SKIP2                                                                
009500*01  POST -COPY W335400A -PRE  ANTAL-  -L.                                
009600     EJECT                                                                
009700 WORKING-STORAGE SECTION.                                                 
009800     SKIP2                                                                
009900                                                                          
010000*    -- CHECKED BY WY2000                                                 
010100 77  IDPGM                       PIC X(8)    VALUE 'W3354200'.            
010200 77  JA                          PIC X       VALUE 'J'.                   
010300 77  NEJ                         PIC X       VALUE 'N'.                   
010400 77  WS-POSTRAKNARE              PIC 9(5)    VALUE ZERO.                  
010500 77  MAX-POSTER                  PIC 9(5)    VALUE 58000.                 
010600 77  RETURKOD                    PIC S9(2)   COMP-3 VALUE ZERO.           
010700                                                                          
010730     EJECT                                                                
010800                                                                          
010900 77  W33544I-EOF-SW              PIC X       VALUE 'N'.                   
011000     88  END-OF-W33544I                      VALUE 'J'.                   
011100     EJECT                                                                
011200 77  W33543-EOF-SW              PIC X       VALUE 'N'.                    
011300     88  END-OF-W33543                      VALUE 'J'.                    
011400     EJECT                                                                
011500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011600 01  FILLER REDEFINES DAGENS-DATUM.                                       
011700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
011900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012000     EJECT                                                                
012100*    --- PARAMETERS FOR ALPHA-TO-NUM                                      
012200 77  W-IDLEVNR-NUM               PIC S9(5)   COMP-3.                      
012300 77  W-IDLEVNR-ALPHA             PIC X(5).                                
012400 77  W-TALLY                     PIC 9(5).                                
012500                                                                          
012600 01  DYNAMISKA-SUBPROGRAM.                                                
012700*                                                                         
012800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013000     SKIP2                                                                
013100*    --- PARAMETRAR TILL ABEND                                            
013200                                                                          
013300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013500     SKIP2                                                                
013600 01  FELTEXT.                                                             
013700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013900     EJECT                                                                
014000*    --- PARAMETRAR TILL POSTSUM                                          
014100*                                                                         
014200*01  -COPY W0005   -PRE  POSTSUM-                                         
014300     EJECT                                                                
014400 01  IN-AREA-START               PIC X(24)   VALUE                        
014500                                 'IN-AREA-START  '.                       
014600     SKIP2                                                                
014700 01  IN-AREA.                                                             
014800     03  FILLER                  PIC X(400).                              
014900*01  FILLER -COPY W335404A      -PRE IN1-   -RED  IN-AREA                 
015000*01  FILLER -COPY W335402A      -PRE IN2-   -RED  IN-AREA                 
015100*01  FILLER -COPY W335403A      -PRE IN3-   -RED  IN-AREA                 
015200     EJECT                                                                
015300 01  ANTAL-AREA-START               PIC X(24)   VALUE                     
015400                                 'ANTAL-AREA-START  '.                    
015500     SKIP2                                                                
015600 01  ANTAL-AREA.                                                          
015700     03  FILLER                  PIC X(50).                               
015800*01  FILLER -COPY W335400A      -PRE ANTAL-   -RED  ANTAL-AREA            
015900     EJECT                                                                
016000 01  UT-AREA-START               PIC X(24)   VALUE                        
016100                                 'UT-AREA-START  '.                       
016200     SKIP2                                                                
016300 01  UT-AREA.                                                             
016400     03  FILLER                  PIC X(400).                              
016500*01  FILLER -COPY W335404A      -PRE UT1-   -RED  UT-AREA                 
016600*01  FILLER -COPY W335402A      -PRE UT2-   -RED  UT-AREA                 
016700*01  FILLER -COPY W335403A      -PRE UT3-   -RED  UT-AREA                 
016800     EJECT                                                                
016900 01  UTVCOM-AREA-START           PIC X(24)   VALUE                        
017000                                 'UTVCOM-AREA-START  '.                   
017100     SKIP2                                                                
017200 01  UTVCOM-AREA.                                                         
017300     03  FILLER                  PIC X(400).                              
017400*01  FILLER -COPY W335400A      -PRE UTVCOM0-   -RED  UTVCOM-AREA         
017500*01  FILLER -COPY W335404B      -PRE UTVCOM1-   -RED  UTVCOM-AREA         
017600*01  FILLER -COPY W335402A      -PRE UTVCOM2-   -RED  UTVCOM-AREA         
017700*01  FILLER -COPY W335403A      -PRE UTVCOM3-   -RED  UTVCOM-AREA         
017800     EJECT                                                                
017900 PROCEDURE DIVISION.                                                      
018000                                                                          
018100     PERFORM A-INIT                                                       
018200     PERFORM S02-LAES-W33544I                                             
018300     PERFORM S03-LAES-W33543                                              
018400     PERFORM S10-SKRIV-W33544V-000                                        
018500     MOVE 1 TO WS-POSTRAKNARE                                             
018600                                                                          
018700     PERFORM UNTIL WS-POSTRAKNARE > MAX-POSTER OR                         
018800                   END-OF-W33544I                                         
018900                                                                          
019000       PERFORM B-FLYTTA-INPOST-TILL-VCOMFIL                               
019100                                                                          
019200       PERFORM S02-LAES-W33544I                                           
019300       ADD 1 TO WS-POSTRAKNARE                                            
019400     END-PERFORM                                                          
019500                                                                          
019600     IF NOT END-OF-W33544I                                                
019700*** FÖR ATT NÄSTA FIL SKA BÖRJA MED 401-POST.                             
019800       PERFORM UNTIL IN1-IDPTYP = '401'                                   
019900         EVALUATE IN1-IDPTYP                                              
020000           WHEN '402'                                                     
020100             PERFORM BB-FLYTTA-POST-402-TILL-VCOM                         
020200           WHEN '403'                                                     
020300             PERFORM BC-FLYTTA-POST-403-TILL-VCOM                         
020400         END-EVALUATE                                                     
020500         PERFORM S02-LAES-W33544I                                         
020600       END-PERFORM                                                        
020700     END-IF                                                               
020800     IF END-OF-W33544I                                                    
020900       MOVE ZERO TO RETURKOD                                              
021000     ELSE                                                                 
021100       MOVE +8   TO RETURKOD                                              
021200                                                                          
021300       PERFORM UNTIL END-OF-W33544I                                       
021400                                                                          
021500         PERFORM C-FLYTTA-INPOST-TILL-UTFIL                               
021600                                                                          
021700         PERFORM S02-LAES-W33544I                                         
021800       END-PERFORM                                                        
021900     END-IF                                                               
022000                                                                          
022100     PERFORM Z-FINIT                                                      
022200                                                                          
022300     MOVE RETURKOD TO RETURN-CODE                                         
022400     GOBACK                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 A-INIT SECTION.                                                          
022800                                                                          
022900     OPEN INPUT                                                           
023000                 W33544I                                                  
023100                 W33543                                                   
023200                                                                          
023300     OPEN OUTPUT W33544U                                                  
023400                 W33544V                                                  
023500                                                                          
023600     ACCEPT DAGENS-DATUM FROM DATE                                        
023700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023800     .                                                                    
023900     EJECT                                                                
024000 B-FLYTTA-INPOST-TILL-VCOMFIL SECTION.                                    
024100                                                                          
024200     EVALUATE IN1-IDPTYP                                                  
024300       WHEN '401'                                                         
024400         PERFORM BA-FLYTTA-POST-401-TILL-VCOM                             
024500       WHEN '402'                                                         
024600         PERFORM BB-FLYTTA-POST-402-TILL-VCOM                             
024700       WHEN '403'                                                         
024800         PERFORM BC-FLYTTA-POST-403-TILL-VCOM                             
024900       WHEN OTHER                                                         
025000         STRING 'FEL COPYTEXTVERSION : '                                  
025100                IN1-IDPTYP                                                
025200           DELIMITED BY SIZE INTO FELTEXT-STR                             
025300         DISPLAY FELTEXT                                                  
025400         PERFORM S99-ABEND                                                
025500     END-EVALUATE                                                         
025600     .                                                                    
025700     EJECT                                                                
025800 BA-FLYTTA-POST-401-TILL-VCOM SECTION.                                    
025900                                                                          
026000     MOVE IN1-IDPTYP          TO UTVCOM1-IDPTYP                           
026100     MOVE IN1-IDVTYP          TO UTVCOM1-IDVTYP                           
026200     MOVE IN1-IDARTNR         TO UTVCOM1-IDARTNR                          
026300     MOVE IN1-IDFKNGRP        TO UTVCOM1-IDFKNGRP                         
026400     MOVE IN1-KDSRA           TO UTVCOM1-KDSRA                            
026500     MOVE IN1-KVQPACK-0       TO UTVCOM1-KVQPACK-0                        
026600     MOVE IN1-KDARTURS-NUM    TO UTVCOM1-KDARTURS-NUM                     
026700     MOVE IN1-KDPRODSL        TO UTVCOM1-KDPRODSL                         
026800     MOVE IN1-VLARTNTO        TO UTVCOM1-VLARTNTO                         
026900     MOVE IN1-VKART           TO UTVCOM1-VKART                            
027000     MOVE IN1-KDVSOP          TO UTVCOM1-KDVSOP                           
027100     MOVE IN1-IDSTATNR        TO UTVCOM1-IDSTATNR                         
027200     MOVE IN1-KDSORT          TO UTVCOM1-KDSORT                           
027300     MOVE IN1-KDERS           TO UTVCOM1-KDERS                            
027400     MOVE IN1-KDBPSR          TO UTVCOM1-KDBPSR                           
027500     MOVE IN1-KDBBCL          TO UTVCOM1-KDBBCL                           
027600     MOVE IN1-IDLEVNR         TO UTVCOM1-IDLEVNR                          
027700                                 W-IDLEVNR-ALPHA                          
027800****FIX FÖR ALFA LEVNR**************************                          
027900     IF W-IDLEVNR-ALPHA = 'BQ2QA'                                         
028000        MOVE '01385' TO W-IDLEVNR-ALPHA                                   
028100     END-IF                                                               
028200     IF W-IDLEVNR-ALPHA = 'D12YA'                                         
028300        MOVE '06414' TO W-IDLEVNR-ALPHA                                   
028400     END-IF                                                               
028500     IF W-IDLEVNR-ALPHA = 'D0KLA'                                         
028600        MOVE '06916' TO W-IDLEVNR-ALPHA                                   
028700     END-IF                                                               
028800**********************************************                            
028900*************************************************                         
029000     IF W-IDLEVNR-ALPHA = 'BJ7TB'                                         
029100        MOVE '10132' TO W-IDLEVNR-ALPHA  END-IF                           
029200     IF W-IDLEVNR-ALPHA = 'D6M4A'                                         
029300        MOVE '19064' TO W-IDLEVNR-ALPHA  END-IF                           
029400     IF W-IDLEVNR-ALPHA = 'BQCTA'                                         
029500        MOVE '19535' TO W-IDLEVNR-ALPHA  END-IF                           
029600     IF W-IDLEVNR-ALPHA = 'R14MA'                                         
029700        MOVE '19610' TO W-IDLEVNR-ALPHA  END-IF                           
029800     IF W-IDLEVNR-ALPHA = 'BQYGA'                                         
029900        MOVE '22   ' TO W-IDLEVNR-ALPHA  END-IF                           
030000     IF W-IDLEVNR-ALPHA = 'BLTLA'                                         
030100        MOVE '2591 ' TO W-IDLEVNR-ALPHA  END-IF                           
030200     IF W-IDLEVNR-ALPHA = 'F4P6B'                                         
030300        MOVE '4445 ' TO W-IDLEVNR-ALPHA  END-IF                           
030400     IF W-IDLEVNR-ALPHA = 'CN5CA'                                         
030500        MOVE '4893 ' TO W-IDLEVNR-ALPHA  END-IF                           
030600     IF W-IDLEVNR-ALPHA = 'BWMZA'                                         
030700        MOVE '7630 ' TO W-IDLEVNR-ALPHA  END-IF                           
030800     IF W-IDLEVNR-ALPHA = 'CFT2B'                                         
030900        MOVE '7949 ' TO W-IDLEVNR-ALPHA  END-IF                           
031000**********************************************                            
031100     IF W-IDLEVNR-ALPHA = 'BWS5A'                                         
031200        MOVE '132  ' TO W-IDLEVNR-ALPHA  END-IF                           
031300     IF W-IDLEVNR-ALPHA = 'BMBQA'                                         
031400        MOVE '226  ' TO W-IDLEVNR-ALPHA  END-IF                           
031500     IF W-IDLEVNR-ALPHA = 'BKFVA'                                         
031600        MOVE '346  ' TO W-IDLEVNR-ALPHA  END-IF                           
031700     IF W-IDLEVNR-ALPHA = 'BWS9A'                                         
031800        MOVE '350  ' TO W-IDLEVNR-ALPHA  END-IF                           
031900     IF W-IDLEVNR-ALPHA = 'R9KSA'                                         
032000        MOVE '500  ' TO W-IDLEVNR-ALPHA  END-IF                           
032100     IF W-IDLEVNR-ALPHA = 'BQ2MA'                                         
032200        MOVE '1362 ' TO W-IDLEVNR-ALPHA  END-IF                           
032300     IF W-IDLEVNR-ALPHA = 'BQ2RA'                                         
032400        MOVE '1389 ' TO W-IDLEVNR-ALPHA  END-IF                           
032500     IF W-IDLEVNR-ALPHA = 'BPW9A'                                         
032600        MOVE '1425 ' TO W-IDLEVNR-ALPHA  END-IF                           
032700     IF W-IDLEVNR-ALPHA = 'DDDPA'                                         
032800        MOVE '1594 ' TO W-IDLEVNR-ALPHA  END-IF                           
032900     IF W-IDLEVNR-ALPHA = 'BQ3BA'                                         
033000        MOVE '1603 ' TO W-IDLEVNR-ALPHA  END-IF                           
033100     IF W-IDLEVNR-ALPHA = 'CFNKA'                                         
033200        MOVE '1675 ' TO W-IDLEVNR-ALPHA  END-IF                           
033300     IF W-IDLEVNR-ALPHA = 'MNDLA'                                         
033400        MOVE '2299 ' TO W-IDLEVNR-ALPHA  END-IF                           
033500     IF W-IDLEVNR-ALPHA = 'BQ6AA'                                         
033600        MOVE '2507 ' TO W-IDLEVNR-ALPHA  END-IF                           
033700     IF W-IDLEVNR-ALPHA = 'CFN0A'                                         
033800        MOVE '3664 ' TO W-IDLEVNR-ALPHA  END-IF                           
033900     IF W-IDLEVNR-ALPHA = 'CFN9A'                                         
034000        MOVE '3718 ' TO W-IDLEVNR-ALPHA  END-IF                           
034100     IF W-IDLEVNR-ALPHA = 'C96AA'                                         
034200        MOVE '5183 ' TO W-IDLEVNR-ALPHA  END-IF                           
034300     IF W-IDLEVNR-ALPHA = 'D0RED'                                         
034400        MOVE '6323 ' TO W-IDLEVNR-ALPHA  END-IF                           
034500     IF W-IDLEVNR-ALPHA = 'D0REB'                                         
034600        MOVE '6704 ' TO W-IDLEVNR-ALPHA  END-IF                           
034700     IF W-IDLEVNR-ALPHA = 'D0REA'                                         
034800        MOVE '6770 ' TO W-IDLEVNR-ALPHA  END-IF                           
034900     IF W-IDLEVNR-ALPHA = 'BUA7A'                                         
035000        MOVE '10108' TO W-IDLEVNR-ALPHA  END-IF                           
035100     IF W-IDLEVNR-ALPHA = 'AGPBA'                                         
035200        MOVE '14829' TO W-IDLEVNR-ALPHA  END-IF                           
035300*************************************************                         
035400     IF W-IDLEVNR-ALPHA = 'BYLRA'                                         
035500        MOVE '839  ' TO W-IDLEVNR-ALPHA  END-IF                           
035600     IF W-IDLEVNR-ALPHA = 'BQ2DA'                                         
035700        MOVE '1100 ' TO W-IDLEVNR-ALPHA  END-IF                           
035800     IF W-IDLEVNR-ALPHA = 'BKWRA'                                         
035900        MOVE '1205 ' TO W-IDLEVNR-ALPHA  END-IF                           
036000     IF W-IDLEVNR-ALPHA = 'BQ2HA'                                         
036100        MOVE '1285 ' TO W-IDLEVNR-ALPHA  END-IF                           
036200     IF W-IDLEVNR-ALPHA = 'N81NA'                                         
036300        MOVE '1336 ' TO W-IDLEVNR-ALPHA  END-IF                           
036400     IF W-IDLEVNR-ALPHA = 'S51YA'                                         
036500        MOVE '1605 ' TO W-IDLEVNR-ALPHA  END-IF                           
036600     IF W-IDLEVNR-ALPHA = 'AHTXA'                                         
036700        MOVE '3948 ' TO W-IDLEVNR-ALPHA  END-IF                           
036800     IF W-IDLEVNR-ALPHA = 'R500F'                                         
036900        MOVE '4034 ' TO W-IDLEVNR-ALPHA  END-IF                           
037000     IF W-IDLEVNR-ALPHA = 'K4UKA'                                         
037100        MOVE '4937 ' TO W-IDLEVNR-ALPHA  END-IF                           
037200     IF W-IDLEVNR-ALPHA = 'S3ULA'                                         
037300        MOVE '4979 ' TO W-IDLEVNR-ALPHA  END-IF                           
037400     IF W-IDLEVNR-ALPHA = 'C9F8A'                                         
037500        MOVE '5012 ' TO W-IDLEVNR-ALPHA  END-IF                           
037600     IF W-IDLEVNR-ALPHA = 'CFN1A'                                         
037700        MOVE '5145 ' TO W-IDLEVNR-ALPHA  END-IF                           
037800     IF W-IDLEVNR-ALPHA = 'BVNUA'                                         
037900        MOVE '5197 ' TO W-IDLEVNR-ALPHA  END-IF                           
038000     IF W-IDLEVNR-ALPHA = 'BQ7BA'                                         
038100        MOVE '5356 ' TO W-IDLEVNR-ALPHA  END-IF                           
038200     IF W-IDLEVNR-ALPHA = 'T727Z'                                         
038300        MOVE '5645 ' TO W-IDLEVNR-ALPHA  END-IF                           
038400     IF W-IDLEVNR-ALPHA = 'U494Q'                                         
038500        MOVE '5868 ' TO W-IDLEVNR-ALPHA  END-IF                           
038600     IF W-IDLEVNR-ALPHA = 'D35VA'                                         
038700        MOVE '6543 ' TO W-IDLEVNR-ALPHA  END-IF                           
038800     IF W-IDLEVNR-ALPHA = 'FNT8A'                                         
038900        MOVE '6985 ' TO W-IDLEVNR-ALPHA  END-IF                           
039000     IF W-IDLEVNR-ALPHA = 'CFT0A'                                         
039100        MOVE '7139 ' TO W-IDLEVNR-ALPHA  END-IF                           
039200     IF W-IDLEVNR-ALPHA = 'CFT8A'                                         
039300        MOVE '8187 ' TO W-IDLEVNR-ALPHA  END-IF                           
039400     IF W-IDLEVNR-ALPHA = 'K4UKB'                                         
039500        MOVE '11577' TO W-IDLEVNR-ALPHA  END-IF                           
039600     IF W-IDLEVNR-ALPHA = 'H387D'                                         
039700        MOVE '14985' TO W-IDLEVNR-ALPHA  END-IF                           
039800     IF W-IDLEVNR-ALPHA = 'S12HA'                                         
039900        MOVE '16076' TO W-IDLEVNR-ALPHA  END-IF                           
040000     IF W-IDLEVNR-ALPHA = 'BPXJA'                                         
040100        MOVE '16265' TO W-IDLEVNR-ALPHA  END-IF                           
040200     IF W-IDLEVNR-ALPHA = 'CFT8B'                                         
040300        MOVE '22410' TO W-IDLEVNR-ALPHA  END-IF                           
040400     IF W-IDLEVNR-ALPHA = 'C9G6A'                                         
040500        MOVE '24030' TO W-IDLEVNR-ALPHA  END-IF                           
040600     IF W-IDLEVNR-ALPHA = 'S12HE'                                         
040700        MOVE '24331' TO W-IDLEVNR-ALPHA  END-IF                           
040800*************************************************                         
040900*************************************************                         
041000     IF W-IDLEVNR-ALPHA = 'BQ1FA'                                         
041100        MOVE '640  ' TO W-IDLEVNR-ALPHA  END-IF                           
041200     IF W-IDLEVNR-ALPHA = 'BK3DA'                                         
041300        MOVE '813  ' TO W-IDLEVNR-ALPHA  END-IF                           
041400     IF W-IDLEVNR-ALPHA = 'N81FA'                                         
041500        MOVE '836  ' TO W-IDLEVNR-ALPHA  END-IF                           
041600     IF W-IDLEVNR-ALPHA = 'BQ1JA'                                         
041700        MOVE '845  ' TO W-IDLEVNR-ALPHA  END-IF                           
041800     IF W-IDLEVNR-ALPHA = 'BQ1KA'                                         
041900        MOVE '850  ' TO W-IDLEVNR-ALPHA  END-IF                           
042000     IF W-IDLEVNR-ALPHA = 'BQ1LA'                                         
042100        MOVE '861  ' TO W-IDLEVNR-ALPHA  END-IF                           
042200     IF W-IDLEVNR-ALPHA = 'BHFCA'                                         
042300        MOVE '927  ' TO W-IDLEVNR-ALPHA  END-IF                           
042400     IF W-IDLEVNR-ALPHA = 'BKCKA'                                         
042500        MOVE '930  ' TO W-IDLEVNR-ALPHA  END-IF                           
042600     IF W-IDLEVNR-ALPHA = 'N81JA'                                         
042700        MOVE '933  ' TO W-IDLEVNR-ALPHA  END-IF                           
042800     IF W-IDLEVNR-ALPHA = 'BQ2XA'                                         
042900        MOVE '1326 ' TO W-IDLEVNR-ALPHA  END-IF                           
043000     IF W-IDLEVNR-ALPHA = 'BLU5A'                                         
043100        MOVE '1659 ' TO W-IDLEVNR-ALPHA  END-IF                           
043200     IF W-IDLEVNR-ALPHA = 'BQ2YA'                                         
043300        MOVE '1977 ' TO W-IDLEVNR-ALPHA  END-IF                           
043400     IF W-IDLEVNR-ALPHA = 'CN5GA'                                         
043500        MOVE '7186 ' TO W-IDLEVNR-ALPHA  END-IF                           
043600     IF W-IDLEVNR-ALPHA = 'D0FTA'                                         
043700        MOVE '7208 ' TO W-IDLEVNR-ALPHA  END-IF                           
043800     IF W-IDLEVNR-ALPHA = 'FLZ6B'                                         
043900        MOVE '16154' TO W-IDLEVNR-ALPHA  END-IF                           
044000     IF W-IDLEVNR-ALPHA = 'S9GAA'                                         
044100        MOVE '18086' TO W-IDLEVNR-ALPHA  END-IF                           
044200*************************************************                         
044300     IF W-IDLEVNR-ALPHA = 'BWTDA'                                         
044400        MOVE '548  ' TO W-IDLEVNR-ALPHA  END-IF                           
044500     IF W-IDLEVNR-ALPHA = 'BWTFA'                                         
044600        MOVE '598  ' TO W-IDLEVNR-ALPHA  END-IF                           
044700     IF W-IDLEVNR-ALPHA = 'BQ1CA'                                         
044800        MOVE '605  ' TO W-IDLEVNR-ALPHA  END-IF                           
044900     IF W-IDLEVNR-ALPHA = 'DBHJA'                                         
045000        MOVE '667  ' TO W-IDLEVNR-ALPHA  END-IF                           
045100     IF W-IDLEVNR-ALPHA = 'BQ1MA'                                         
045200        MOVE '890  ' TO W-IDLEVNR-ALPHA  END-IF                           
045300     IF W-IDLEVNR-ALPHA = 'CD2JA'                                         
045400        MOVE '897  ' TO W-IDLEVNR-ALPHA  END-IF                           
045500     IF W-IDLEVNR-ALPHA = 'BLRQA'                                         
045600        MOVE '929  ' TO W-IDLEVNR-ALPHA  END-IF                           
045700     IF W-IDLEVNR-ALPHA = 'BQ2PA'                                         
045800        MOVE '1380 ' TO W-IDLEVNR-ALPHA  END-IF                           
045900     IF W-IDLEVNR-ALPHA = 'BQ6QB'                                         
046000        MOVE '1386 ' TO W-IDLEVNR-ALPHA  END-IF                           
046100     IF W-IDLEVNR-ALPHA = 'BSBZA'                                         
046200        MOVE '1560 ' TO W-IDLEVNR-ALPHA  END-IF                           
046300     IF W-IDLEVNR-ALPHA = 'BQ3EA'                                         
046400        MOVE '1728 ' TO W-IDLEVNR-ALPHA  END-IF                           
046500     IF W-IDLEVNR-ALPHA = 'BQ3NA'                                         
046600        MOVE '1736 ' TO W-IDLEVNR-ALPHA  END-IF                           
046700     IF W-IDLEVNR-ALPHA = 'BQ8AA'                                         
046800        MOVE '1783 ' TO W-IDLEVNR-ALPHA  END-IF                           
046900     IF W-IDLEVNR-ALPHA = 'BK6ZA'                                         
047000        MOVE '1809 ' TO W-IDLEVNR-ALPHA  END-IF                           
047100     IF W-IDLEVNR-ALPHA = 'BQ5MA'                                         
047200        MOVE '1978 ' TO W-IDLEVNR-ALPHA  END-IF                           
047300     IF W-IDLEVNR-ALPHA = 'BQ5NA'                                         
047400        MOVE '1982 ' TO W-IDLEVNR-ALPHA  END-IF                           
047500     IF W-IDLEVNR-ALPHA = 'S52HA'                                         
047600        MOVE '2087 ' TO W-IDLEVNR-ALPHA  END-IF                           
047700     IF W-IDLEVNR-ALPHA = 'CFNYA'                                         
047800        MOVE '3380 ' TO W-IDLEVNR-ALPHA  END-IF                           
047900     IF W-IDLEVNR-ALPHA = 'E622D'                                         
048000        MOVE '3654 ' TO W-IDLEVNR-ALPHA  END-IF                           
048100     IF W-IDLEVNR-ALPHA = 'D3D4A'                                         
048200        MOVE '6881 ' TO W-IDLEVNR-ALPHA  END-IF                           
048300     IF W-IDLEVNR-ALPHA = 'BQ7YC'                                         
048400        MOVE '7314 ' TO W-IDLEVNR-ALPHA  END-IF                           
048500     IF W-IDLEVNR-ALPHA = 'BQ7YB'                                         
048600        MOVE '7369 ' TO W-IDLEVNR-ALPHA  END-IF                           
048700     IF W-IDLEVNR-ALPHA = 'BQ7YA'                                         
048800        MOVE '7420 ' TO W-IDLEVNR-ALPHA  END-IF                           
048900     IF W-IDLEVNR-ALPHA = 'BPVBC'                                         
049000        MOVE '7881 ' TO W-IDLEVNR-ALPHA  END-IF                           
049100     IF W-IDLEVNR-ALPHA = 'BPVBB'                                         
049200        MOVE '10096' TO W-IDLEVNR-ALPHA  END-IF                           
049300     IF W-IDLEVNR-ALPHA = 'CFNZB'                                         
049400        MOVE '13691' TO W-IDLEVNR-ALPHA  END-IF                           
049500     IF W-IDLEVNR-ALPHA = 'U7SAD'                                         
049600        MOVE '14996' TO W-IDLEVNR-ALPHA  END-IF                           
049700     IF W-IDLEVNR-ALPHA = 'AN3AB'                                         
049800        MOVE '16226' TO W-IDLEVNR-ALPHA  END-IF                           
049900     IF W-IDLEVNR-ALPHA = 'U910A'                                         
050000        MOVE '24719' TO W-IDLEVNR-ALPHA  END-IF                           
050100     IF W-IDLEVNR-ALPHA = 'J5C2B'                                         
050200        MOVE '25016' TO W-IDLEVNR-ALPHA  END-IF                           
050300     IF W-IDLEVNR-ALPHA = 'LMJGA'                                         
050400        MOVE '25676' TO W-IDLEVNR-ALPHA  END-IF                           
050500     IF W-IDLEVNR-ALPHA = 'BPVBA'                                         
050600        MOVE '13445' TO W-IDLEVNR-ALPHA  END-IF                           
050700*************************************************                         
050800*************************************************                         
050900     IF W-IDLEVNR-ALPHA = 'BQ8ZD'                                         
051000        MOVE '93   ' TO W-IDLEVNR-ALPHA  END-IF                           
051100     IF W-IDLEVNR-ALPHA = 'BQ9CB'                                         
051200        MOVE '234  ' TO W-IDLEVNR-ALPHA  END-IF                           
051300     IF W-IDLEVNR-ALPHA = 'J2A6B'                                         
051400        MOVE '386  ' TO W-IDLEVNR-ALPHA  END-IF                           
051500     IF W-IDLEVNR-ALPHA = 'BQ8ZB'                                         
051600        MOVE '607  ' TO W-IDLEVNR-ALPHA  END-IF                           
051700     IF W-IDLEVNR-ALPHA = 'CXC6A'                                         
051800        MOVE '780  ' TO W-IDLEVNR-ALPHA  END-IF                           
051900     IF W-IDLEVNR-ALPHA = 'C7Q2D'                                         
052000        MOVE '835  ' TO W-IDLEVNR-ALPHA  END-IF                           
052100     IF W-IDLEVNR-ALPHA = 'BQ9AA'                                         
052200        MOVE '1187 ' TO W-IDLEVNR-ALPHA  END-IF                           
052300     IF W-IDLEVNR-ALPHA = 'C7Q2B'                                         
052400        MOVE '1228 ' TO W-IDLEVNR-ALPHA  END-IF                           
052500     IF W-IDLEVNR-ALPHA = 'BQ9CC'                                         
052600        MOVE '1393 ' TO W-IDLEVNR-ALPHA  END-IF                           
052700     IF W-IDLEVNR-ALPHA = 'BPU0A'                                         
052800        MOVE '2065 ' TO W-IDLEVNR-ALPHA  END-IF                           
052900     IF W-IDLEVNR-ALPHA = 'BQ5PA'                                         
053000        MOVE '2108 ' TO W-IDLEVNR-ALPHA  END-IF                           
053100     IF W-IDLEVNR-ALPHA = 'BPU0B'                                         
053200        MOVE '2157 ' TO W-IDLEVNR-ALPHA  END-IF                           
053300     IF W-IDLEVNR-ALPHA = 'BKVVA'                                         
053400        MOVE '2220 ' TO W-IDLEVNR-ALPHA  END-IF                           
053500     IF W-IDLEVNR-ALPHA = 'J2A6A'                                         
053600        MOVE '2288 ' TO W-IDLEVNR-ALPHA  END-IF                           
053700     IF W-IDLEVNR-ALPHA = 'BX0ZA'                                         
053800        MOVE '2289 ' TO W-IDLEVNR-ALPHA  END-IF                           
053900     IF W-IDLEVNR-ALPHA = 'BQ5RA'                                         
054000        MOVE '2315 ' TO W-IDLEVNR-ALPHA  END-IF                           
054100     IF W-IDLEVNR-ALPHA = 'D3F1A'                                         
054200        MOVE '2379 ' TO W-IDLEVNR-ALPHA  END-IF                           
054300     IF W-IDLEVNR-ALPHA = 'CXC6B'                                         
054400        MOVE '2545 ' TO W-IDLEVNR-ALPHA  END-IF                           
054500     IF W-IDLEVNR-ALPHA = 'D3F1B'                                         
054600        MOVE '2560 ' TO W-IDLEVNR-ALPHA  END-IF                           
054700     IF W-IDLEVNR-ALPHA = 'S053A'                                         
054800        MOVE '5033 ' TO W-IDLEVNR-ALPHA  END-IF                           
054900     IF W-IDLEVNR-ALPHA = 'P90CA'                                         
055000        MOVE '6664 ' TO W-IDLEVNR-ALPHA  END-IF                           
055100     IF W-IDLEVNR-ALPHA = 'BQ9CD'                                         
055200        MOVE '8294 ' TO W-IDLEVNR-ALPHA  END-IF                           
055300     IF W-IDLEVNR-ALPHA = 'BQ1QB'                                         
055400        MOVE '10220' TO W-IDLEVNR-ALPHA  END-IF                           
055500     IF W-IDLEVNR-ALPHA = 'BQ8UA'                                         
055600        MOVE '10374' TO W-IDLEVNR-ALPHA  END-IF                           
055700     IF W-IDLEVNR-ALPHA = 'N5MXB'                                         
055800        MOVE '14032' TO W-IDLEVNR-ALPHA  END-IF                           
055900     IF W-IDLEVNR-ALPHA = 'BQ9CE'                                         
056000        MOVE '19623' TO W-IDLEVNR-ALPHA  END-IF                           
056100     IF W-IDLEVNR-ALPHA = 'D3F1C'                                         
056200        MOVE '25979' TO W-IDLEVNR-ALPHA  END-IF                           
056300     IF W-IDLEVNR-ALPHA = 'D3F1D'                                         
056400        MOVE '25980' TO W-IDLEVNR-ALPHA  END-IF                           
056500     IF W-IDLEVNR-ALPHA = 'DMS1A'                                         
056600        MOVE '25923' TO W-IDLEVNR-ALPHA  END-IF                           
056700*************************************************                         
056800*************************************************                         
056900     IF W-IDLEVNR-ALPHA = 'BHFAA'                                         
057000        MOVE '4    ' TO W-IDLEVNR-ALPHA  END-IF                           
057100     IF W-IDLEVNR-ALPHA = 'CFM6A'                                         
057200        MOVE '32   ' TO W-IDLEVNR-ALPHA  END-IF                           
057300     IF W-IDLEVNR-ALPHA = 'BKR3A'                                         
057400        MOVE '64   ' TO W-IDLEVNR-ALPHA  END-IF                           
057500     IF W-IDLEVNR-ALPHA = 'CL3WA'                                         
057600        MOVE '75   ' TO W-IDLEVNR-ALPHA  END-IF                           
057700     IF W-IDLEVNR-ALPHA = 'BJPKA'                                         
057800        MOVE '81   ' TO W-IDLEVNR-ALPHA  END-IF                           
057900     IF W-IDLEVNR-ALPHA = 'CL3ZA'                                         
058000        MOVE '682  ' TO W-IDLEVNR-ALPHA  END-IF                           
058100     IF W-IDLEVNR-ALPHA = 'CXN9A'                                         
058200        MOVE '777  ' TO W-IDLEVNR-ALPHA  END-IF                           
058300     IF W-IDLEVNR-ALPHA = 'BLRUA'                                         
058400        MOVE '1952 ' TO W-IDLEVNR-ALPHA  END-IF                           
058500     IF W-IDLEVNR-ALPHA = 'BQ5UA'                                         
058600        MOVE '2387 ' TO W-IDLEVNR-ALPHA  END-IF                           
058700     IF W-IDLEVNR-ALPHA = 'BQ5YA'                                         
058800        MOVE '2437 ' TO W-IDLEVNR-ALPHA  END-IF                           
058900     IF W-IDLEVNR-ALPHA = 'BJR1A'                                         
059000        MOVE '2483 ' TO W-IDLEVNR-ALPHA  END-IF                           
059100     IF W-IDLEVNR-ALPHA = 'BQ5ZA'                                         
059200        MOVE '2495 ' TO W-IDLEVNR-ALPHA  END-IF                           
059300     IF W-IDLEVNR-ALPHA = 'BQ6BA'                                         
059400        MOVE '2539 ' TO W-IDLEVNR-ALPHA  END-IF                           
059500     IF W-IDLEVNR-ALPHA = 'CZ1SA'                                         
059600        MOVE '3730 ' TO W-IDLEVNR-ALPHA  END-IF                           
059700     IF W-IDLEVNR-ALPHA = 'F842A'                                         
059800        MOVE '4961 ' TO W-IDLEVNR-ALPHA  END-IF                           
059900     IF W-IDLEVNR-ALPHA = 'P7ZFA'                                         
060000        MOVE '4969 ' TO W-IDLEVNR-ALPHA  END-IF                           
060100     IF W-IDLEVNR-ALPHA = 'C9B7B'                                         
060200        MOVE '6757 ' TO W-IDLEVNR-ALPHA  END-IF                           
060300     IF W-IDLEVNR-ALPHA = 'BQ5VA'                                         
060400        MOVE '10370' TO W-IDLEVNR-ALPHA  END-IF                           
060500     IF W-IDLEVNR-ALPHA = 'R2FLG'                                         
060600        MOVE '11096' TO W-IDLEVNR-ALPHA  END-IF                           
060700     IF W-IDLEVNR-ALPHA = 'BPUKA'                                         
060800        MOVE '13360' TO W-IDLEVNR-ALPHA  END-IF                           
060900     IF W-IDLEVNR-ALPHA = 'BPUMA'                                         
061000        MOVE '13538' TO W-IDLEVNR-ALPHA  END-IF                           
061100     IF W-IDLEVNR-ALPHA = 'E019A'                                         
061200        MOVE '50535' TO W-IDLEVNR-ALPHA  END-IF                           
061300*************************************************                         
061400*************************************************                         
061500     IF W-IDLEVNR-ALPHA = 'LCWLA'                                         
061600        MOVE '139  ' TO W-IDLEVNR-ALPHA  END-IF                           
061700     IF W-IDLEVNR-ALPHA = 'CN4TA'                                         
061800        MOVE '793  ' TO W-IDLEVNR-ALPHA  END-IF                           
061900     IF W-IDLEVNR-ALPHA = 'DL2QA'                                         
062000        MOVE '3911 ' TO W-IDLEVNR-ALPHA  END-IF                           
062100     IF W-IDLEVNR-ALPHA = 'CBVLA'                                         
062200        MOVE '808  ' TO W-IDLEVNR-ALPHA  END-IF                           
062300     IF W-IDLEVNR-ALPHA = 'BMBMA'                                         
062400        MOVE '855  ' TO W-IDLEVNR-ALPHA  END-IF                           
062500     IF W-IDLEVNR-ALPHA = 'BQ3AA'                                         
062600        MOVE '1566 ' TO W-IDLEVNR-ALPHA  END-IF                           
062700     IF W-IDLEVNR-ALPHA = 'CFNMA'                                         
062800        MOVE '1859 ' TO W-IDLEVNR-ALPHA  END-IF                           
062900     IF W-IDLEVNR-ALPHA = 'CBSDA'                                         
063000        MOVE '1863 ' TO W-IDLEVNR-ALPHA  END-IF                           
063100     IF W-IDLEVNR-ALPHA = 'BUF5A'                                         
063200        MOVE '1902 ' TO W-IDLEVNR-ALPHA  END-IF                           
063300     IF W-IDLEVNR-ALPHA = 'BQ6EA'                                         
063400        MOVE '2609 ' TO W-IDLEVNR-ALPHA  END-IF                           
063500     IF W-IDLEVNR-ALPHA = 'BLUDA'                                         
063600        MOVE '3034 ' TO W-IDLEVNR-ALPHA  END-IF                           
063700     IF W-IDLEVNR-ALPHA = 'BKJDA'                                         
063800        MOVE '3152 ' TO W-IDLEVNR-ALPHA  END-IF                           
063900     IF W-IDLEVNR-ALPHA = 'BL1UA'                                         
064000        MOVE '3167 ' TO W-IDLEVNR-ALPHA  END-IF                           
064100     IF W-IDLEVNR-ALPHA = 'BQ6RA'                                         
064200        MOVE '3370 ' TO W-IDLEVNR-ALPHA  END-IF                           
064300     IF W-IDLEVNR-ALPHA = 'D3U3A'                                         
064400        MOVE '3538 ' TO W-IDLEVNR-ALPHA  END-IF                           
064500     IF W-IDLEVNR-ALPHA = 'S0H5D'                                         
064600        MOVE '3669 ' TO W-IDLEVNR-ALPHA  END-IF                           
064700     IF W-IDLEVNR-ALPHA = 'G5FPC'                                         
064800        MOVE '3722 ' TO W-IDLEVNR-ALPHA  END-IF                           
064900     IF W-IDLEVNR-ALPHA = 'V136C'                                         
065000        MOVE '3770 ' TO W-IDLEVNR-ALPHA  END-IF                           
065100     IF W-IDLEVNR-ALPHA = 'P38NA'                                         
065200        MOVE '3988 ' TO W-IDLEVNR-ALPHA  END-IF                           
065300     IF W-IDLEVNR-ALPHA = 'G5FPD'                                         
065400        MOVE '3994 ' TO W-IDLEVNR-ALPHA  END-IF                           
065500     IF W-IDLEVNR-ALPHA = 'E623F'                                         
065600        MOVE '4880 ' TO W-IDLEVNR-ALPHA  END-IF                           
065700     IF W-IDLEVNR-ALPHA = 'D3U2A'                                         
065800        MOVE '4942 ' TO W-IDLEVNR-ALPHA  END-IF                           
065900     IF W-IDLEVNR-ALPHA = 'D3K6A'                                         
066000        MOVE '5314 ' TO W-IDLEVNR-ALPHA  END-IF                           
066100     IF W-IDLEVNR-ALPHA = 'D3L3A'                                         
066200        MOVE '5362 ' TO W-IDLEVNR-ALPHA  END-IF                           
066300     IF W-IDLEVNR-ALPHA = 'BQYEA'                                         
066400        MOVE '25982' TO W-IDLEVNR-ALPHA  END-IF                           
066500     IF W-IDLEVNR-ALPHA = 'V2NYA'                                         
066600        MOVE '25847' TO W-IDLEVNR-ALPHA  END-IF                           
066700     IF W-IDLEVNR-ALPHA = 'DRTHA'                                         
066800        MOVE '25996' TO W-IDLEVNR-ALPHA  END-IF                           
066900     IF W-IDLEVNR-ALPHA = 'D3D0A'                                         
067000        MOVE '6014 ' TO W-IDLEVNR-ALPHA  END-IF                           
067100     IF W-IDLEVNR-ALPHA = 'CRGJA'                                         
067200        MOVE '6101 ' TO W-IDLEVNR-ALPHA  END-IF                           
067300     IF W-IDLEVNR-ALPHA = 'C6S4A'                                         
067400        MOVE '6200 ' TO W-IDLEVNR-ALPHA  END-IF                           
067500     IF W-IDLEVNR-ALPHA = 'Q6QLA'                                         
067600        MOVE '6346 ' TO W-IDLEVNR-ALPHA  END-IF                           
067700     IF W-IDLEVNR-ALPHA = 'D25KA'                                         
067800        MOVE '6810 ' TO W-IDLEVNR-ALPHA  END-IF                           
067900     IF W-IDLEVNR-ALPHA = 'E623B'                                         
068000        MOVE '6842 ' TO W-IDLEVNR-ALPHA  END-IF                           
068100     IF W-IDLEVNR-ALPHA = 'T7WFA'                                         
068200        MOVE '6996 ' TO W-IDLEVNR-ALPHA  END-IF                           
068300     IF W-IDLEVNR-ALPHA = 'BP8JA'                                         
068400        MOVE '10135' TO W-IDLEVNR-ALPHA  END-IF                           
068500     IF W-IDLEVNR-ALPHA = 'BP8JE'                                         
068600        MOVE '10483' TO W-IDLEVNR-ALPHA  END-IF                           
068700     IF W-IDLEVNR-ALPHA = 'R5YYA'                                         
068800        MOVE '10488' TO W-IDLEVNR-ALPHA  END-IF                           
068900     IF W-IDLEVNR-ALPHA = 'BP8JC'                                         
069000        MOVE '10493' TO W-IDLEVNR-ALPHA  END-IF                           
069100     IF W-IDLEVNR-ALPHA = 'Q5EGB'                                         
069200        MOVE '10494' TO W-IDLEVNR-ALPHA  END-IF                           
069300     IF W-IDLEVNR-ALPHA = 'Q5EGB'                                         
069400        MOVE '10494' TO W-IDLEVNR-ALPHA  END-IF                           
069500     IF W-IDLEVNR-ALPHA = 'BQ6RB'                                         
069600        MOVE '13389' TO W-IDLEVNR-ALPHA  END-IF                           
069700     IF W-IDLEVNR-ALPHA = 'M9TMB'                                         
069800        MOVE '13578' TO W-IDLEVNR-ALPHA  END-IF                           
069900     IF W-IDLEVNR-ALPHA = 'F432J'                                         
070000        MOVE '13594' TO W-IDLEVNR-ALPHA  END-IF                           
070100     IF W-IDLEVNR-ALPHA = 'D07LG'                                         
070200        MOVE '13612' TO W-IDLEVNR-ALPHA  END-IF                           
070300     IF W-IDLEVNR-ALPHA = 'D07LB'                                         
070400        MOVE '13614' TO W-IDLEVNR-ALPHA  END-IF                           
070500     IF W-IDLEVNR-ALPHA = 'CR9GA'                                         
070600        MOVE '13741' TO W-IDLEVNR-ALPHA  END-IF                           
070700     IF W-IDLEVNR-ALPHA = 'AN1JE'                                         
070800        MOVE '13793' TO W-IDLEVNR-ALPHA  END-IF                           
070900     IF W-IDLEVNR-ALPHA = 'BQJFA'                                         
071000        MOVE '15451' TO W-IDLEVNR-ALPHA  END-IF                           
071100     IF W-IDLEVNR-ALPHA = 'D25KD'                                         
071200        MOVE '16004' TO W-IDLEVNR-ALPHA  END-IF                           
071300     IF W-IDLEVNR-ALPHA = 'D25KE'                                         
071400        MOVE '16005' TO W-IDLEVNR-ALPHA  END-IF                           
071500     IF W-IDLEVNR-ALPHA = 'E623C'                                         
071600        MOVE '16039' TO W-IDLEVNR-ALPHA  END-IF                           
071700     IF W-IDLEVNR-ALPHA = 'B41YA'                                         
071800        MOVE '16080' TO W-IDLEVNR-ALPHA  END-IF                           
071900     IF W-IDLEVNR-ALPHA = 'E623E'                                         
072000        MOVE '16268' TO W-IDLEVNR-ALPHA  END-IF                           
072100     IF W-IDLEVNR-ALPHA = 'EKM4A'                                         
072200        MOVE '16403' TO W-IDLEVNR-ALPHA  END-IF                           
072300     IF W-IDLEVNR-ALPHA = 'BP8JD'                                         
072400        MOVE '17715' TO W-IDLEVNR-ALPHA  END-IF                           
072500     IF W-IDLEVNR-ALPHA = 'BKPTA'                                         
072600        MOVE '18203' TO W-IDLEVNR-ALPHA  END-IF                           
072700     IF W-IDLEVNR-ALPHA = 'ANEBA'                                         
072800        MOVE '19907' TO W-IDLEVNR-ALPHA  END-IF                           
072900     IF W-IDLEVNR-ALPHA = 'C72DC'                                         
073000        MOVE '22292' TO W-IDLEVNR-ALPHA  END-IF                           
073100     IF W-IDLEVNR-ALPHA = 'DDC3A'                                         
073200        MOVE '25729' TO W-IDLEVNR-ALPHA  END-IF                           
073300     IF W-IDLEVNR-ALPHA = 'AUJ7B'                                         
073400        MOVE '25749' TO W-IDLEVNR-ALPHA  END-IF                           
073500     IF W-IDLEVNR-ALPHA = 'AUJ7C'                                         
073600        MOVE '25846' TO W-IDLEVNR-ALPHA  END-IF                           
073700     IF W-IDLEVNR-ALPHA = 'DNK9A'                                         
073800        MOVE '25931' TO W-IDLEVNR-ALPHA  END-IF                           
073900*************************************************                         
074000**********************************************                            
074100     IF W-IDLEVNR-ALPHA = 'BQEFA'                                         
074200        MOVE '29   ' TO W-IDLEVNR-ALPHA  END-IF                           
074300     IF W-IDLEVNR-ALPHA = 'BQZ7A'                                         
074400        MOVE '280  ' TO W-IDLEVNR-ALPHA  END-IF                           
074500     IF W-IDLEVNR-ALPHA = 'BQZ9A'                                         
074600        MOVE '320  ' TO W-IDLEVNR-ALPHA  END-IF                           
074700     IF W-IDLEVNR-ALPHA = 'BQ0DA'                                         
074800        MOVE '372  ' TO W-IDLEVNR-ALPHA  END-IF                           
074900     IF W-IDLEVNR-ALPHA = 'BQ0EA'                                         
075000        MOVE '495  ' TO W-IDLEVNR-ALPHA  END-IF                           
075100     IF W-IDLEVNR-ALPHA = 'BQ6K1'                                         
075200        MOVE '579  ' TO W-IDLEVNR-ALPHA  END-IF                           
075300     IF W-IDLEVNR-ALPHA = 'CFNEA'                                         
075400        MOVE '710  ' TO W-IDLEVNR-ALPHA  END-IF                           
075500     IF W-IDLEVNR-ALPHA = 'BQ5LA'                                         
075600        MOVE '1912 ' TO W-IDLEVNR-ALPHA  END-IF                           
075700     IF W-IDLEVNR-ALPHA = 'CN4WA'                                         
075800        MOVE '1944 ' TO W-IDLEVNR-ALPHA  END-IF                           
075900     IF W-IDLEVNR-ALPHA = 'CW6BA'                                         
076000        MOVE '2020 ' TO W-IDLEVNR-ALPHA  END-IF                           
076100     IF W-IDLEVNR-ALPHA = 'C6T3A'                                         
076200        MOVE '2351 ' TO W-IDLEVNR-ALPHA  END-IF                           
076300     IF W-IDLEVNR-ALPHA = 'CFNSA'                                         
076400        MOVE '2410 ' TO W-IDLEVNR-ALPHA  END-IF                           
076500     IF W-IDLEVNR-ALPHA = 'P112B'                                         
076600        MOVE '3559 ' TO W-IDLEVNR-ALPHA  END-IF                           
076700     IF W-IDLEVNR-ALPHA = 'C75RA'                                         
076800        MOVE '3606 ' TO W-IDLEVNR-ALPHA  END-IF                           
076900     IF W-IDLEVNR-ALPHA = 'P112L'                                         
077000        MOVE '3705 ' TO W-IDLEVNR-ALPHA  END-IF                           
077100     IF W-IDLEVNR-ALPHA = 'R7NAB'                                         
077200        MOVE '3865 ' TO W-IDLEVNR-ALPHA  END-IF                           
077300     IF W-IDLEVNR-ALPHA = 'B2N4A'                                         
077400        MOVE '4964 ' TO W-IDLEVNR-ALPHA  END-IF                           
077500     IF W-IDLEVNR-ALPHA = 'C8Z7A'                                         
077600        MOVE '6605 ' TO W-IDLEVNR-ALPHA  END-IF                           
077700     IF W-IDLEVNR-ALPHA = 'D0U2A'                                         
077800        MOVE '6765 ' TO W-IDLEVNR-ALPHA  END-IF                           
077900     IF W-IDLEVNR-ALPHA = 'C97GA'                                         
078000        MOVE '6947 ' TO W-IDLEVNR-ALPHA  END-IF                           
078100     IF W-IDLEVNR-ALPHA = 'AZJLA'                                         
078200        MOVE '11148' TO W-IDLEVNR-ALPHA  END-IF                           
078300     IF W-IDLEVNR-ALPHA = 'K0R6F'                                         
078400        MOVE '11326' TO W-IDLEVNR-ALPHA  END-IF                           
078500     IF W-IDLEVNR-ALPHA = 'Q18RA'                                         
078600        MOVE '13382' TO W-IDLEVNR-ALPHA  END-IF                           
078700     IF W-IDLEVNR-ALPHA = 'P112D'                                         
078800        MOVE '13540' TO W-IDLEVNR-ALPHA  END-IF                           
078900     IF W-IDLEVNR-ALPHA = 'P112M'                                         
079000        MOVE '13541' TO W-IDLEVNR-ALPHA  END-IF                           
079100     IF W-IDLEVNR-ALPHA = 'T0CJA'                                         
079200        MOVE '13548' TO W-IDLEVNR-ALPHA  END-IF                           
079300     IF W-IDLEVNR-ALPHA = 'C75RB'                                         
079400        MOVE '13579' TO W-IDLEVNR-ALPHA  END-IF                           
079500     IF W-IDLEVNR-ALPHA = 'K0R6G'                                         
079600        MOVE '13587' TO W-IDLEVNR-ALPHA  END-IF                           
079700     IF W-IDLEVNR-ALPHA = 'BP8HB'                                         
079800        MOVE '14621' TO W-IDLEVNR-ALPHA  END-IF                           
079900     IF W-IDLEVNR-ALPHA = 'D0U2B'                                         
080000        MOVE '16172' TO W-IDLEVNR-ALPHA  END-IF                           
080100     IF W-IDLEVNR-ALPHA = 'D17KA'                                         
080200        MOVE '19255' TO W-IDLEVNR-ALPHA  END-IF                           
080300     IF W-IDLEVNR-ALPHA = 'R76JA'                                         
080400        MOVE '25936' TO W-IDLEVNR-ALPHA  END-IF                           
080500     IF W-IDLEVNR-ALPHA = 'D26QC'                                         
080600        MOVE '25937' TO W-IDLEVNR-ALPHA  END-IF                           
080700     IF W-IDLEVNR-ALPHA = 'BP8HA'                                         
080800        MOVE '34621' TO W-IDLEVNR-ALPHA  END-IF                           
080900*************************************************                         
081000**********************************************                            
081100     IF W-IDLEVNR-ALPHA = 'BJWQA'                                         
081200        MOVE '354  ' TO W-IDLEVNR-ALPHA  END-IF                           
081300     IF W-IDLEVNR-ALPHA = 'BQ3DA'                                         
081400        MOVE '1612 ' TO W-IDLEVNR-ALPHA  END-IF                           
081500     IF W-IDLEVNR-ALPHA = 'CFNLA'                                         
081600        MOVE '1755 ' TO W-IDLEVNR-ALPHA  END-IF                           
081700     IF W-IDLEVNR-ALPHA = 'C96UD'                                         
081800        MOVE '1800 ' TO W-IDLEVNR-ALPHA  END-IF                           
081900     IF W-IDLEVNR-ALPHA = 'BK9KA'                                         
082000        MOVE '2248 ' TO W-IDLEVNR-ALPHA  END-IF                           
082100     IF W-IDLEVNR-ALPHA = 'GPR5A'                                         
082200        MOVE '5425 ' TO W-IDLEVNR-ALPHA  END-IF                           
082300     IF W-IDLEVNR-ALPHA = 'Q0ERA'                                         
082400        MOVE '5489 ' TO W-IDLEVNR-ALPHA  END-IF                           
082500     IF W-IDLEVNR-ALPHA = 'AHHSA'                                         
082600        MOVE '5679 ' TO W-IDLEVNR-ALPHA  END-IF                           
082700     IF W-IDLEVNR-ALPHA = 'S2ZLA'                                         
082800        MOVE '6756 ' TO W-IDLEVNR-ALPHA  END-IF                           
082900     IF W-IDLEVNR-ALPHA = 'C7B1A'                                         
083000        MOVE '6775 ' TO W-IDLEVNR-ALPHA  END-IF                           
083100     IF W-IDLEVNR-ALPHA = 'D2S2A'                                         
083200        MOVE '6795 ' TO W-IDLEVNR-ALPHA  END-IF                           
083300     IF W-IDLEVNR-ALPHA = 'D23YA'                                         
083400        MOVE '6807 ' TO W-IDLEVNR-ALPHA  END-IF                           
083500     IF W-IDLEVNR-ALPHA = 'BQ7PA'                                         
083600        MOVE '7218 ' TO W-IDLEVNR-ALPHA  END-IF                           
083700     IF W-IDLEVNR-ALPHA = 'CDHSA'                                         
083800        MOVE '7757 ' TO W-IDLEVNR-ALPHA  END-IF                           
083900     IF W-IDLEVNR-ALPHA = 'D23YB'                                         
084000        MOVE '7922 ' TO W-IDLEVNR-ALPHA  END-IF                           
084100     IF W-IDLEVNR-ALPHA = 'P8C8A'                                         
084200        MOVE '14615' TO W-IDLEVNR-ALPHA  END-IF                           
084300     IF W-IDLEVNR-ALPHA = 'N2M5A'                                         
084400        MOVE '16088' TO W-IDLEVNR-ALPHA  END-IF                           
084500     IF W-IDLEVNR-ALPHA = 'DAFTA'                                         
084600        MOVE '16171' TO W-IDLEVNR-ALPHA  END-IF                           
084700     IF W-IDLEVNR-ALPHA = 'BQ9VA'                                         
084800        MOVE '16388' TO W-IDLEVNR-ALPHA  END-IF                           
084900     IF W-IDLEVNR-ALPHA = 'BHZ3A'                                         
085000        MOVE '18688' TO W-IDLEVNR-ALPHA  END-IF                           
085100     IF W-IDLEVNR-ALPHA = 'V4FVA'                                         
085200        MOVE '19454' TO W-IDLEVNR-ALPHA  END-IF                           
085300     IF W-IDLEVNR-ALPHA = 'S3JJA'                                         
085400        MOVE '19455' TO W-IDLEVNR-ALPHA  END-IF                           
085500     IF W-IDLEVNR-ALPHA = 'BQ7PB'                                         
085600        MOVE '24065' TO W-IDLEVNR-ALPHA  END-IF                           
085700     IF W-IDLEVNR-ALPHA = 'BQ7RB'                                         
085800        MOVE '24066' TO W-IDLEVNR-ALPHA  END-IF                           
085900     IF W-IDLEVNR-ALPHA = 'G13FC'                                         
086000        MOVE '24078' TO W-IDLEVNR-ALPHA  END-IF                           
086100     IF W-IDLEVNR-ALPHA = 'BQ7RC'                                         
086200        MOVE '25752' TO W-IDLEVNR-ALPHA  END-IF                           
086300*************************************************                         
086400     IF W-IDLEVNR-ALPHA = 'BMLWA'                                         
086500        MOVE '143  ' TO W-IDLEVNR-ALPHA  END-IF                           
086600     IF W-IDLEVNR-ALPHA = 'BQ1NA'                                         
086700        MOVE '934  ' TO W-IDLEVNR-ALPHA  END-IF                           
086800     IF W-IDLEVNR-ALPHA = 'BPUYA'                                         
086900        MOVE '1008 ' TO W-IDLEVNR-ALPHA  END-IF                           
087000     IF W-IDLEVNR-ALPHA = 'BKRZA'                                         
087100        MOVE '1062 ' TO W-IDLEVNR-ALPHA  END-IF                           
087200     IF W-IDLEVNR-ALPHA = 'S34XF'                                         
087300        MOVE '1134 ' TO W-IDLEVNR-ALPHA  END-IF                           
087400     IF W-IDLEVNR-ALPHA = 'BKDQA'                                         
087500        MOVE '1196 ' TO W-IDLEVNR-ALPHA  END-IF                           
087600     IF W-IDLEVNR-ALPHA = 'U7ABB'                                         
087700        MOVE '1244 ' TO W-IDLEVNR-ALPHA  END-IF                           
087800     IF W-IDLEVNR-ALPHA = 'U7ABC'                                         
087900        MOVE '1269 ' TO W-IDLEVNR-ALPHA  END-IF                           
088000     IF W-IDLEVNR-ALPHA = 'BSKYA'                                         
088100        MOVE '1449 ' TO W-IDLEVNR-ALPHA  END-IF                           
088200     IF W-IDLEVNR-ALPHA = 'BKMMA'                                         
088300        MOVE '1662 ' TO W-IDLEVNR-ALPHA  END-IF                           
088400     IF W-IDLEVNR-ALPHA = 'U7ABA'                                         
088500        MOVE '1847 ' TO W-IDLEVNR-ALPHA  END-IF                           
088600     IF W-IDLEVNR-ALPHA = 'S34XD'                                         
088700        MOVE '2500 ' TO W-IDLEVNR-ALPHA  END-IF                           
088800     IF W-IDLEVNR-ALPHA = 'BQAJA'                                         
088900        MOVE '2552 ' TO W-IDLEVNR-ALPHA  END-IF                           
089000     IF W-IDLEVNR-ALPHA = 'BWKGA'                                         
089100        MOVE '2553 ' TO W-IDLEVNR-ALPHA  END-IF                           
089200     IF W-IDLEVNR-ALPHA = 'BWTCA'                                         
089300        MOVE '2558 ' TO W-IDLEVNR-ALPHA  END-IF                           
089400     IF W-IDLEVNR-ALPHA = 'CFNVA'                                         
089500        MOVE '2633 ' TO W-IDLEVNR-ALPHA  END-IF                           
089600     IF W-IDLEVNR-ALPHA = 'BQAJB'                                         
089700        MOVE '2684 ' TO W-IDLEVNR-ALPHA  END-IF                           
089800     IF W-IDLEVNR-ALPHA = 'BWKSA'                                         
089900        MOVE '3054 ' TO W-IDLEVNR-ALPHA  END-IF                           
090000     IF W-IDLEVNR-ALPHA = 'CN4YA'                                         
090100        MOVE '3165 ' TO W-IDLEVNR-ALPHA  END-IF                           
090200     IF W-IDLEVNR-ALPHA = 'CFNWA'                                         
090300        MOVE '3197 ' TO W-IDLEVNR-ALPHA  END-IF                           
090400     IF W-IDLEVNR-ALPHA = 'D3C5A'                                         
090500        MOVE '3468 ' TO W-IDLEVNR-ALPHA  END-IF                           
090600     IF W-IDLEVNR-ALPHA = 'BP9ZA'                                         
090700        MOVE '3558 ' TO W-IDLEVNR-ALPHA  END-IF                           
090800     IF W-IDLEVNR-ALPHA = 'Q6TFA'                                         
090900        MOVE '3662 ' TO W-IDLEVNR-ALPHA  END-IF                           
091000     IF W-IDLEVNR-ALPHA = 'D5H4A'                                         
091100        MOVE '3807 ' TO W-IDLEVNR-ALPHA  END-IF                           
091200     IF W-IDLEVNR-ALPHA = 'BQ6VA'                                         
091300        MOVE '3982 ' TO W-IDLEVNR-ALPHA  END-IF                           
091400     IF W-IDLEVNR-ALPHA = 'AHMPA'                                         
091500        MOVE '4172 ' TO W-IDLEVNR-ALPHA  END-IF                           
091600     IF W-IDLEVNR-ALPHA = 'BPFNA'                                         
091700        MOVE '4721 ' TO W-IDLEVNR-ALPHA  END-IF                           
091800     IF W-IDLEVNR-ALPHA = 'ATNNA'                                         
091900        MOVE '4724 ' TO W-IDLEVNR-ALPHA  END-IF                           
092000     IF W-IDLEVNR-ALPHA = 'BCJSA'                                         
092100        MOVE '4988 ' TO W-IDLEVNR-ALPHA  END-IF                           
092200     IF W-IDLEVNR-ALPHA = 'C7L2A'                                         
092300        MOVE '5281 ' TO W-IDLEVNR-ALPHA  END-IF                           
092400     IF W-IDLEVNR-ALPHA = 'E3B2B'                                         
092500        MOVE '6305 ' TO W-IDLEVNR-ALPHA  END-IF                           
092600     IF W-IDLEVNR-ALPHA = 'C68JA'                                         
092700        MOVE '6350 ' TO W-IDLEVNR-ALPHA  END-IF                           
092800     IF W-IDLEVNR-ALPHA = 'D0MGA'                                         
092900        MOVE '6554 ' TO W-IDLEVNR-ALPHA  END-IF                           
093000     IF W-IDLEVNR-ALPHA = 'BPX3B'                                         
093100        MOVE '6555 ' TO W-IDLEVNR-ALPHA  END-IF                           
093200     IF W-IDLEVNR-ALPHA = 'C8T1A'                                         
093300        MOVE '6556 ' TO W-IDLEVNR-ALPHA  END-IF                           
093400     IF W-IDLEVNR-ALPHA = 'C68JC'                                         
093500        MOVE '6597 ' TO W-IDLEVNR-ALPHA  END-IF                           
093600     IF W-IDLEVNR-ALPHA = 'G944E'                                         
093700        MOVE '6911 ' TO W-IDLEVNR-ALPHA  END-IF                           
093800     IF W-IDLEVNR-ALPHA = 'CDNXA'                                         
093900        MOVE '6958 ' TO W-IDLEVNR-ALPHA  END-IF                           
094000     IF W-IDLEVNR-ALPHA = 'D38KA'                                         
094100        MOVE '6961 ' TO W-IDLEVNR-ALPHA  END-IF                           
094200     IF W-IDLEVNR-ALPHA = 'D38KD'                                         
094300        MOVE '6962 ' TO W-IDLEVNR-ALPHA  END-IF                           
094400     IF W-IDLEVNR-ALPHA = 'MTSFA'                                         
094500        MOVE '7462 ' TO W-IDLEVNR-ALPHA  END-IF                           
094600     IF W-IDLEVNR-ALPHA = 'BP9ZC'                                         
094700        MOVE '13672' TO W-IDLEVNR-ALPHA  END-IF                           
094800     IF W-IDLEVNR-ALPHA = 'BP9ZB'                                         
094900        MOVE '13709' TO W-IDLEVNR-ALPHA  END-IF                           
095000     IF W-IDLEVNR-ALPHA = 'D38KF'                                         
095100        MOVE '16044' TO W-IDLEVNR-ALPHA  END-IF                           
095200     IF W-IDLEVNR-ALPHA = 'R7GNA'                                         
095300        MOVE '16092' TO W-IDLEVNR-ALPHA  END-IF                           
095400     IF W-IDLEVNR-ALPHA = 'T2GCC'                                         
095500        MOVE '16141' TO W-IDLEVNR-ALPHA  END-IF                           
095600     IF W-IDLEVNR-ALPHA = 'D0W5D'                                         
095700        MOVE '16142' TO W-IDLEVNR-ALPHA  END-IF                           
095800     IF W-IDLEVNR-ALPHA = 'BPX3C'                                         
095900        MOVE '16365' TO W-IDLEVNR-ALPHA  END-IF                           
096000     IF W-IDLEVNR-ALPHA = 'T2GCA'                                         
096100        MOVE '16406' TO W-IDLEVNR-ALPHA  END-IF                           
096200     IF W-IDLEVNR-ALPHA = 'AD3XA'                                         
096300        MOVE '21256' TO W-IDLEVNR-ALPHA  END-IF                           
096400     IF W-IDLEVNR-ALPHA = 'D38KG'                                         
096500        MOVE '25618' TO W-IDLEVNR-ALPHA  END-IF                           
096600     IF W-IDLEVNR-ALPHA = 'BVPDA'                                         
096700        MOVE '25920' TO W-IDLEVNR-ALPHA  END-IF                           
096800     IF W-IDLEVNR-ALPHA = 'C68JF'                                         
096900        MOVE '26350' TO W-IDLEVNR-ALPHA  END-IF                           
097000*************************************************                         
097100     IF W-IDLEVNR-ALPHA = 'BP8JB'                                         
097200        MOVE '912  ' TO W-IDLEVNR-ALPHA  END-IF                           
097300     IF W-IDLEVNR-ALPHA = 'BMJGA'                                         
097400        MOVE '1005 ' TO W-IDLEVNR-ALPHA  END-IF                           
097500     IF W-IDLEVNR-ALPHA = 'AY0CA'                                         
097600        MOVE '1720 ' TO W-IDLEVNR-ALPHA  END-IF                           
097700     IF W-IDLEVNR-ALPHA = 'BPUNA'                                         
097800        MOVE '2442 ' TO W-IDLEVNR-ALPHA  END-IF                           
097900     IF W-IDLEVNR-ALPHA = 'BKTXA'                                         
098000        MOVE '3074 ' TO W-IDLEVNR-ALPHA  END-IF                           
098100     IF W-IDLEVNR-ALPHA = 'BPA1A'                                         
098200        MOVE '3163 ' TO W-IDLEVNR-ALPHA  END-IF                           
098300     IF W-IDLEVNR-ALPHA = 'C62FA'                                         
098400        MOVE '3575 ' TO W-IDLEVNR-ALPHA  END-IF                           
098500     IF W-IDLEVNR-ALPHA = 'J3CQA'                                         
098600        MOVE '4319 ' TO W-IDLEVNR-ALPHA  END-IF                           
098700     IF W-IDLEVNR-ALPHA = 'D026P'                                         
098800        MOVE '4382 ' TO W-IDLEVNR-ALPHA  END-IF                           
098900     IF W-IDLEVNR-ALPHA = 'MLMZA'                                         
099000        MOVE '4448 ' TO W-IDLEVNR-ALPHA  END-IF                           
099100     IF W-IDLEVNR-ALPHA = 'BQ6WA'                                         
099200        MOVE '4585 ' TO W-IDLEVNR-ALPHA  END-IF                           
099300     IF W-IDLEVNR-ALPHA = 'BQ6WB'                                         
099400        MOVE '4628 ' TO W-IDLEVNR-ALPHA  END-IF                           
099500     IF W-IDLEVNR-ALPHA = 'A426K'                                         
099600        MOVE '4894 ' TO W-IDLEVNR-ALPHA  END-IF                           
099700     IF W-IDLEVNR-ALPHA = 'C0VAG'                                         
099800        MOVE '4965 ' TO W-IDLEVNR-ALPHA  END-IF                           
099900     IF W-IDLEVNR-ALPHA = 'AVG9A'                                         
100000        MOVE '5065 ' TO W-IDLEVNR-ALPHA  END-IF                           
100100     IF W-IDLEVNR-ALPHA = 'D24DA'                                         
100200        MOVE '5647 ' TO W-IDLEVNR-ALPHA  END-IF                           
100300     IF W-IDLEVNR-ALPHA = 'D0RYA'                                         
100400        MOVE '6030 ' TO W-IDLEVNR-ALPHA  END-IF                           
100500     IF W-IDLEVNR-ALPHA = 'BT7WA'                                         
100600        MOVE '6279 ' TO W-IDLEVNR-ALPHA  END-IF                           
100700     IF W-IDLEVNR-ALPHA = 'A426G'                                         
100800        MOVE '6840 ' TO W-IDLEVNR-ALPHA  END-IF                           
100900     IF W-IDLEVNR-ALPHA = 'F488A'                                         
101000        MOVE '6992 ' TO W-IDLEVNR-ALPHA  END-IF                           
101100     IF W-IDLEVNR-ALPHA = 'BP3HA'                                         
101200        MOVE '7349 ' TO W-IDLEVNR-ALPHA  END-IF                           
101300     IF W-IDLEVNR-ALPHA = 'CL3VA'                                         
101400        MOVE '10453' TO W-IDLEVNR-ALPHA  END-IF                           
101500     IF W-IDLEVNR-ALPHA = 'A426S'                                         
101600        MOVE '10814' TO W-IDLEVNR-ALPHA  END-IF                           
101700     IF W-IDLEVNR-ALPHA = 'CFT4B'                                         
101800        MOVE '12543' TO W-IDLEVNR-ALPHA  END-IF                           
101900     IF W-IDLEVNR-ALPHA = 'M09EA'                                         
102000        MOVE '14616' TO W-IDLEVNR-ALPHA  END-IF                           
102100     IF W-IDLEVNR-ALPHA = 'BQ6WC'                                         
102200        MOVE '14643' TO W-IDLEVNR-ALPHA  END-IF                           
102300     IF W-IDLEVNR-ALPHA = 'A426R'                                         
102400        MOVE '14647' TO W-IDLEVNR-ALPHA  END-IF                           
102500     IF W-IDLEVNR-ALPHA = 'N718D'                                         
102600        MOVE '14963' TO W-IDLEVNR-ALPHA  END-IF                           
102700     IF W-IDLEVNR-ALPHA = 'N718B'                                         
102800        MOVE '14988' TO W-IDLEVNR-ALPHA  END-IF                           
102900     IF W-IDLEVNR-ALPHA = 'B492C'                                         
103000        MOVE '16134' TO W-IDLEVNR-ALPHA  END-IF                           
103100     IF W-IDLEVNR-ALPHA = 'F488X'                                         
103200        MOVE '16137' TO W-IDLEVNR-ALPHA  END-IF                           
103300     IF W-IDLEVNR-ALPHA = 'AYZ4A'                                         
103400        MOVE '16210' TO W-IDLEVNR-ALPHA  END-IF                           
103500     IF W-IDLEVNR-ALPHA = 'C685Y'                                         
103600        MOVE '16211' TO W-IDLEVNR-ALPHA  END-IF                           
103700     IF W-IDLEVNR-ALPHA = 'C685C'                                         
103800        MOVE '16222' TO W-IDLEVNR-ALPHA  END-IF                           
103900     IF W-IDLEVNR-ALPHA = 'A426C'                                         
104000        MOVE '16237' TO W-IDLEVNR-ALPHA  END-IF                           
104100     IF W-IDLEVNR-ALPHA = 'A426M'                                         
104200        MOVE '16279' TO W-IDLEVNR-ALPHA  END-IF                           
104300     IF W-IDLEVNR-ALPHA = 'F488Z'                                         
104400        MOVE '16283' TO W-IDLEVNR-ALPHA  END-IF                           
104500     IF W-IDLEVNR-ALPHA = 'BA8YA'                                         
104600        MOVE '20094' TO W-IDLEVNR-ALPHA  END-IF                           
104700     IF W-IDLEVNR-ALPHA = 'AUE4A'                                         
104800        MOVE '21873' TO W-IDLEVNR-ALPHA  END-IF                           
104900     IF W-IDLEVNR-ALPHA = 'M09EB'                                         
105000        MOVE '25851' TO W-IDLEVNR-ALPHA  END-IF                           
105100     IF W-IDLEVNR-ALPHA = 'BARJA'                                         
105200        MOVE '25907' TO W-IDLEVNR-ALPHA  END-IF                           
105300     IF W-IDLEVNR-ALPHA = 'BARJB'                                         
105400        MOVE '25908' TO W-IDLEVNR-ALPHA  END-IF                           
105500     IF W-IDLEVNR-ALPHA = 'CUTBA'                                         
105600        MOVE '25909' TO W-IDLEVNR-ALPHA  END-IF                           
105700     IF W-IDLEVNR-ALPHA = 'A426U'                                         
105800        MOVE '26840' TO W-IDLEVNR-ALPHA  END-IF                           
105900     IF W-IDLEVNR-ALPHA = 'BQ7RB'                                         
106000        MOVE '24066' TO W-IDLEVNR-ALPHA  END-IF                           
106100     IF W-IDLEVNR-ALPHA = 'BQ7RC'                                         
106200        MOVE '25752' TO W-IDLEVNR-ALPHA  END-IF                           
106300*************************************************                         
106400     IF W-IDLEVNR-ALPHA = 'BMP1A'                                         
106500        MOVE '856  ' TO W-IDLEVNR-ALPHA  END-IF                           
106600     IF W-IDLEVNR-ALPHA = 'D07FA'                                         
106700        MOVE '3330 ' TO W-IDLEVNR-ALPHA  END-IF                           
106800     IF W-IDLEVNR-ALPHA = 'T2VGA'                                         
106900        MOVE '5453 ' TO W-IDLEVNR-ALPHA  END-IF                           
107000     IF W-IDLEVNR-ALPHA = 'D07FD'                                         
107100        MOVE '6013 ' TO W-IDLEVNR-ALPHA  END-IF                           
107200     IF W-IDLEVNR-ALPHA = 'E510D'                                         
107300        MOVE '6160 ' TO W-IDLEVNR-ALPHA  END-IF                           
107400     IF W-IDLEVNR-ALPHA = 'D07FF'                                         
107500        MOVE '6193 ' TO W-IDLEVNR-ALPHA  END-IF                           
107600     IF W-IDLEVNR-ALPHA = 'BQ7MA'                                         
107700        MOVE '6285 ' TO W-IDLEVNR-ALPHA  END-IF                           
107800     IF W-IDLEVNR-ALPHA = 'CGSWA'                                         
107900        MOVE '7622 ' TO W-IDLEVNR-ALPHA  END-IF                           
108000     IF W-IDLEVNR-ALPHA = 'BZMDA'                                         
108100        MOVE '10965' TO W-IDLEVNR-ALPHA  END-IF                           
108200     IF W-IDLEVNR-ALPHA = 'BEF1A'                                         
108300        MOVE '20303' TO W-IDLEVNR-ALPHA  END-IF                           
108400*************************************************                         
108500     IF W-IDLEVNR-ALPHA = 'BWTAA'                                         
108600        MOVE '520  ' TO W-IDLEVNR-ALPHA  END-IF                           
108700     IF W-IDLEVNR-ALPHA = 'BQ00A'                                         
108800        MOVE '546  ' TO W-IDLEVNR-ALPHA  END-IF                           
108900     IF W-IDLEVNR-ALPHA = 'AHFGA'                                         
109000        MOVE '547  ' TO W-IDLEVNR-ALPHA  END-IF                           
109100     IF W-IDLEVNR-ALPHA = 'BQ1BA'                                         
109200        MOVE '550  ' TO W-IDLEVNR-ALPHA  END-IF                           
109300     IF W-IDLEVNR-ALPHA = 'BQ3YA'                                         
109400        MOVE '1883 ' TO W-IDLEVNR-ALPHA  END-IF                           
109500     IF W-IDLEVNR-ALPHA = 'S3HXA'                                         
109600        MOVE '4610 ' TO W-IDLEVNR-ALPHA  END-IF                           
109700     IF W-IDLEVNR-ALPHA = 'AKTAD'                                         
109800        MOVE '6110 ' TO W-IDLEVNR-ALPHA  END-IF                           
109900     IF W-IDLEVNR-ALPHA = 'BQAWA'                                         
110000        MOVE '10087' TO W-IDLEVNR-ALPHA  END-IF                           
110100     IF W-IDLEVNR-ALPHA = 'BAM8D'                                         
110200        MOVE '11328' TO W-IDLEVNR-ALPHA  END-IF                           
110300     IF W-IDLEVNR-ALPHA = 'BAM8A'                                         
110400        MOVE '19956' TO W-IDLEVNR-ALPHA  END-IF                           
110500     IF W-IDLEVNR-ALPHA = 'CEPHA'                                         
110600        MOVE '19986' TO W-IDLEVNR-ALPHA  END-IF                           
110700*************************************************                         
110800     IF W-IDLEVNR-ALPHA = 'BQAHA'                                         
110900        MOVE '114  ' TO W-IDLEVNR-ALPHA  END-IF                           
111000     IF W-IDLEVNR-ALPHA = 'S3SQA'                                         
111100        MOVE '642  ' TO W-IDLEVNR-ALPHA  END-IF                           
111200     IF W-IDLEVNR-ALPHA = 'S3SQB'                                         
111300        MOVE '1297 ' TO W-IDLEVNR-ALPHA  END-IF                           
111400     IF W-IDLEVNR-ALPHA = 'BQAHD'                                         
111500        MOVE '2639 ' TO W-IDLEVNR-ALPHA  END-IF                           
111600     IF W-IDLEVNR-ALPHA = 'BT7RA'                                         
111700        MOVE '2647 ' TO W-IDLEVNR-ALPHA  END-IF                           
111800     IF W-IDLEVNR-ALPHA = 'BEFYA'                                         
111900        MOVE '3671 ' TO W-IDLEVNR-ALPHA  END-IF                           
112000     IF W-IDLEVNR-ALPHA = 'K0R6A'                                         
112100        MOVE '3787 ' TO W-IDLEVNR-ALPHA  END-IF                           
112200     IF W-IDLEVNR-ALPHA = 'C84QA'                                         
112300        MOVE '3883 ' TO W-IDLEVNR-ALPHA  END-IF                           
112400     IF W-IDLEVNR-ALPHA = 'P38NA'                                         
112500        MOVE '3988 ' TO W-IDLEVNR-ALPHA  END-IF                           
112600     IF W-IDLEVNR-ALPHA = 'MMV4A'                                         
112700        MOVE '4737 ' TO W-IDLEVNR-ALPHA  END-IF                           
112800     IF W-IDLEVNR-ALPHA = 'F842A'                                         
112900        MOVE '4961 ' TO W-IDLEVNR-ALPHA  END-IF                           
113000     IF W-IDLEVNR-ALPHA = 'E1SLB'                                         
113100        MOVE '5160 ' TO W-IDLEVNR-ALPHA  END-IF                           
113200     IF W-IDLEVNR-ALPHA = 'D2R5A'                                         
113300        MOVE '5162 ' TO W-IDLEVNR-ALPHA  END-IF                           
113400     IF W-IDLEVNR-ALPHA = 'C93SA'                                         
113500        MOVE '5171 ' TO W-IDLEVNR-ALPHA  END-IF                           
113600     IF W-IDLEVNR-ALPHA = 'Q749A'                                         
113700        MOVE '5240 ' TO W-IDLEVNR-ALPHA  END-IF                           
113800     IF W-IDLEVNR-ALPHA = 'H268X'                                         
113900        MOVE '5383 ' TO W-IDLEVNR-ALPHA  END-IF                           
114000     IF W-IDLEVNR-ALPHA = 'AXVNA'                                         
114100        MOVE '5662 ' TO W-IDLEVNR-ALPHA  END-IF                           
114200     IF W-IDLEVNR-ALPHA = 'D16AA'                                         
114300        MOVE '6083 ' TO W-IDLEVNR-ALPHA  END-IF                           
114400     IF W-IDLEVNR-ALPHA = 'E521A'                                         
114500        MOVE '6090 ' TO W-IDLEVNR-ALPHA  END-IF                           
114600     IF W-IDLEVNR-ALPHA = 'D04HA'                                         
114700        MOVE '6096 ' TO W-IDLEVNR-ALPHA  END-IF                           
114800     IF W-IDLEVNR-ALPHA = 'EGX2A'                                         
114900        MOVE '6146 ' TO W-IDLEVNR-ALPHA  END-IF                           
115000     IF W-IDLEVNR-ALPHA = 'C6S4A'                                         
115100        MOVE '6200 ' TO W-IDLEVNR-ALPHA  END-IF                           
115200     IF W-IDLEVNR-ALPHA = 'C9U3A'                                         
115300        MOVE '6226 ' TO W-IDLEVNR-ALPHA  END-IF                           
115400     IF W-IDLEVNR-ALPHA = 'D1C6A'                                         
115500        MOVE '6310 ' TO W-IDLEVNR-ALPHA  END-IF                           
115600     IF W-IDLEVNR-ALPHA = 'F6W7A'                                         
115700        MOVE '6321 ' TO W-IDLEVNR-ALPHA  END-IF                           
115800     IF W-IDLEVNR-ALPHA = 'CFTXA'                                         
115900        MOVE '6328 ' TO W-IDLEVNR-ALPHA  END-IF                           
116000     IF W-IDLEVNR-ALPHA = 'AECTA'                                         
116100        MOVE '6360 ' TO W-IDLEVNR-ALPHA  END-IF                           
116200     IF W-IDLEVNR-ALPHA = 'BPU8A'                                         
116300        MOVE '6421 ' TO W-IDLEVNR-ALPHA  END-IF                           
116400     IF W-IDLEVNR-ALPHA = 'X345A'                                         
116500        MOVE '6443 ' TO W-IDLEVNR-ALPHA  END-IF                           
116600     IF W-IDLEVNR-ALPHA = 'D16MA'                                         
116700        MOVE '6468 ' TO W-IDLEVNR-ALPHA  END-IF                           
116800     IF W-IDLEVNR-ALPHA = 'BB4SA'                                         
116900        MOVE '6488 ' TO W-IDLEVNR-ALPHA  END-IF                           
117000     IF W-IDLEVNR-ALPHA = 'BZFFA'                                         
117100        MOVE '6492 ' TO W-IDLEVNR-ALPHA  END-IF                           
117200     IF W-IDLEVNR-ALPHA = 'C9T9A'                                         
117300        MOVE '6764 ' TO W-IDLEVNR-ALPHA  END-IF                           
117400     IF W-IDLEVNR-ALPHA = 'D3L4A'                                         
117500        MOVE '6903 ' TO W-IDLEVNR-ALPHA  END-IF                           
117600     IF W-IDLEVNR-ALPHA = 'DNR5A'                                         
117700        MOVE '6968 ' TO W-IDLEVNR-ALPHA  END-IF                           
117800     IF W-IDLEVNR-ALPHA = 'D0DMA'                                         
117900        MOVE '8040 ' TO W-IDLEVNR-ALPHA  END-IF                           
118000     IF W-IDLEVNR-ALPHA = 'BPXEB'                                         
118100        MOVE '10103' TO W-IDLEVNR-ALPHA  END-IF                           
118200     IF W-IDLEVNR-ALPHA = 'R2FLG'                                         
118300        MOVE '11096' TO W-IDLEVNR-ALPHA  END-IF                           
118400     IF W-IDLEVNR-ALPHA = 'F432J'                                         
118500        MOVE '13594' TO W-IDLEVNR-ALPHA  END-IF                           
118600     IF W-IDLEVNR-ALPHA = 'F432B'                                         
118700        MOVE '13690' TO W-IDLEVNR-ALPHA  END-IF                           
118800     IF W-IDLEVNR-ALPHA = 'K0R6E'                                         
118900        MOVE '13701' TO W-IDLEVNR-ALPHA  END-IF                           
119000     IF W-IDLEVNR-ALPHA = 'E2L9A'                                         
119100        MOVE '16051' TO W-IDLEVNR-ALPHA  END-IF                           
119200     IF W-IDLEVNR-ALPHA = 'C6S4B'                                         
119300        MOVE '16115' TO W-IDLEVNR-ALPHA  END-IF                           
119400     IF W-IDLEVNR-ALPHA = 'H681K'                                         
119500        MOVE '16383' TO W-IDLEVNR-ALPHA  END-IF                           
119600     IF W-IDLEVNR-ALPHA = 'D0V6C'                                         
119700        MOVE '25047' TO W-IDLEVNR-ALPHA  END-IF                           
119800     IF W-IDLEVNR-ALPHA = 'E521J'                                         
119900        MOVE '25403' TO W-IDLEVNR-ALPHA  END-IF                           
120000     IF W-IDLEVNR-ALPHA = 'E019A'                                         
120100        MOVE '50535' TO W-IDLEVNR-ALPHA  END-IF                           
120200     IF W-IDLEVNR-ALPHA = 'CL3XA'                                         
120300        MOVE '63567' TO W-IDLEVNR-ALPHA  END-IF                           
120400*************************************************                         
120500     IF W-IDLEVNR-ALPHA = 'BSK5A'                                         
120600        MOVE '84   ' TO W-IDLEVNR-ALPHA  END-IF                           
120700     IF W-IDLEVNR-ALPHA = 'DJAYA'                                         
120800        MOVE '157  ' TO W-IDLEVNR-ALPHA  END-IF                           
120900     IF W-IDLEVNR-ALPHA = 'BV9NA'                                         
121000        MOVE '173  ' TO W-IDLEVNR-ALPHA  END-IF                           
121100     IF W-IDLEVNR-ALPHA = 'BU4GA'                                         
121200        MOVE '233  ' TO W-IDLEVNR-ALPHA  END-IF                           
121300     IF W-IDLEVNR-ALPHA = 'DJA8A'                                         
121400        MOVE '235  ' TO W-IDLEVNR-ALPHA  END-IF                           
121500     IF W-IDLEVNR-ALPHA = 'DL0KA'                                         
121600        MOVE '282  ' TO W-IDLEVNR-ALPHA  END-IF                           
121700     IF W-IDLEVNR-ALPHA = 'BXPRA'                                         
121800        MOVE '294  ' TO W-IDLEVNR-ALPHA  END-IF                           
121900     IF W-IDLEVNR-ALPHA = 'DJBBA'                                         
122000        MOVE '357  ' TO W-IDLEVNR-ALPHA  END-IF                           
122100     IF W-IDLEVNR-ALPHA = 'DL0LA'                                         
122200        MOVE '370  ' TO W-IDLEVNR-ALPHA  END-IF                           
122300     IF W-IDLEVNR-ALPHA = 'DJBCA'                                         
122400        MOVE '376  ' TO W-IDLEVNR-ALPHA  END-IF                           
122500     IF W-IDLEVNR-ALPHA = 'DL0MA'                                         
122600        MOVE '428  ' TO W-IDLEVNR-ALPHA  END-IF                           
122700     IF W-IDLEVNR-ALPHA = 'BRQFA'                                         
122800        MOVE '449  ' TO W-IDLEVNR-ALPHA  END-IF                           
122900     IF W-IDLEVNR-ALPHA = 'BSK2A'                                         
123000        MOVE '466  ' TO W-IDLEVNR-ALPHA  END-IF                           
123100     IF W-IDLEVNR-ALPHA = 'S6RPA'                                         
123200        MOVE '498  ' TO W-IDLEVNR-ALPHA  END-IF                           
123300     IF W-IDLEVNR-ALPHA = 'BLMNA'                                         
123400        MOVE '505  ' TO W-IDLEVNR-ALPHA  END-IF                           
123500     IF W-IDLEVNR-ALPHA = 'BJ9QA'                                         
123600        MOVE '523  ' TO W-IDLEVNR-ALPHA  END-IF                           
123700     IF W-IDLEVNR-ALPHA = 'DJBDA'                                         
123800        MOVE '524  ' TO W-IDLEVNR-ALPHA  END-IF                           
123900     IF W-IDLEVNR-ALPHA = 'CVESB'                                         
124000        MOVE '555  ' TO W-IDLEVNR-ALPHA  END-IF                           
124100     IF W-IDLEVNR-ALPHA = 'CFH3A'                                         
124200        MOVE '652  ' TO W-IDLEVNR-ALPHA  END-IF                           
124300     IF W-IDLEVNR-ALPHA = 'DJBEA'                                         
124400        MOVE '670  ' TO W-IDLEVNR-ALPHA  END-IF                           
124500     IF W-IDLEVNR-ALPHA = 'BMKTA'                                         
124600        MOVE '671  ' TO W-IDLEVNR-ALPHA  END-IF                           
124700     IF W-IDLEVNR-ALPHA = 'DJBFA'                                         
124800        MOVE '719  ' TO W-IDLEVNR-ALPHA  END-IF                           
124900     IF W-IDLEVNR-ALPHA = 'CDT1A'                                         
125000        MOVE '753  ' TO W-IDLEVNR-ALPHA  END-IF                           
125100     IF W-IDLEVNR-ALPHA = 'DJBGA'                                         
125200        MOVE '794  ' TO W-IDLEVNR-ALPHA  END-IF                           
125300     IF W-IDLEVNR-ALPHA = 'DJKLA'                                         
125400        MOVE '804  ' TO W-IDLEVNR-ALPHA  END-IF                           
125500     IF W-IDLEVNR-ALPHA = 'BN6ZA'                                         
125600        MOVE '944  ' TO W-IDLEVNR-ALPHA  END-IF                           
125700     IF W-IDLEVNR-ALPHA = 'DL0PA'                                         
125800        MOVE '948  ' TO W-IDLEVNR-ALPHA  END-IF                           
125900     IF W-IDLEVNR-ALPHA = 'BSKZA'                                         
126000        MOVE '971  ' TO W-IDLEVNR-ALPHA  END-IF                           
126100     IF W-IDLEVNR-ALPHA = 'DL0TA'                                         
126200        MOVE '1025 ' TO W-IDLEVNR-ALPHA  END-IF                           
126300     IF W-IDLEVNR-ALPHA = 'BZCEA'                                         
126400        MOVE '1068 ' TO W-IDLEVNR-ALPHA  END-IF                           
126500     IF W-IDLEVNR-ALPHA = 'DJKMA'                                         
126600        MOVE '1102 ' TO W-IDLEVNR-ALPHA  END-IF                           
126700     IF W-IDLEVNR-ALPHA = 'DL0UA'                                         
126800        MOVE '1160 ' TO W-IDLEVNR-ALPHA  END-IF                           
126900     IF W-IDLEVNR-ALPHA = 'BKMEA'                                         
127000        MOVE '1232 ' TO W-IDLEVNR-ALPHA  END-IF                           
127100     IF W-IDLEVNR-ALPHA = 'BJTTA'                                         
127200        MOVE '1293 ' TO W-IDLEVNR-ALPHA  END-IF                           
127300     IF W-IDLEVNR-ALPHA = 'BKMJA'                                         
127400        MOVE '1304 ' TO W-IDLEVNR-ALPHA  END-IF                           
127500     IF W-IDLEVNR-ALPHA = 'BMN7B'                                         
127600        MOVE '1331 ' TO W-IDLEVNR-ALPHA  END-IF                           
127700     IF W-IDLEVNR-ALPHA = 'BX4TA'                                         
127800        MOVE '1512 ' TO W-IDLEVNR-ALPHA  END-IF                           
127900     IF W-IDLEVNR-ALPHA = 'DJKNA'                                         
128000        MOVE '1523 ' TO W-IDLEVNR-ALPHA  END-IF                           
128100     IF W-IDLEVNR-ALPHA = 'DJKPA'                                         
128200        MOVE '1542 ' TO W-IDLEVNR-ALPHA  END-IF                           
128300     IF W-IDLEVNR-ALPHA = 'DJKQA'                                         
128400        MOVE '1606 ' TO W-IDLEVNR-ALPHA  END-IF                           
128500     IF W-IDLEVNR-ALPHA = 'BKEBA'                                         
128600        MOVE '1633 ' TO W-IDLEVNR-ALPHA  END-IF                           
128700     IF W-IDLEVNR-ALPHA = 'BJZQA'                                         
128800        MOVE '1699 ' TO W-IDLEVNR-ALPHA  END-IF                           
128900     IF W-IDLEVNR-ALPHA = 'DJKRA'                                         
129000        MOVE '1756 ' TO W-IDLEVNR-ALPHA  END-IF                           
129100     IF W-IDLEVNR-ALPHA = 'DJKSA'                                         
129200        MOVE '1764 ' TO W-IDLEVNR-ALPHA  END-IF                           
129300     IF W-IDLEVNR-ALPHA = 'BJ9ZA'                                         
129400        MOVE '1887 ' TO W-IDLEVNR-ALPHA  END-IF                           
129500     IF W-IDLEVNR-ALPHA = 'BKMUA'                                         
129600        MOVE '1916 ' TO W-IDLEVNR-ALPHA  END-IF                           
129700     IF W-IDLEVNR-ALPHA = 'DL0XA'                                         
129800        MOVE '2028 ' TO W-IDLEVNR-ALPHA  END-IF                           
129900     IF W-IDLEVNR-ALPHA = 'BLWMA'                                         
130000        MOVE '2055 ' TO W-IDLEVNR-ALPHA  END-IF                           
130100     IF W-IDLEVNR-ALPHA = 'BJQTA'                                         
130200        MOVE '2177 ' TO W-IDLEVNR-ALPHA  END-IF                           
130300     IF W-IDLEVNR-ALPHA = 'BJPDA'                                         
130400        MOVE '2229 ' TO W-IDLEVNR-ALPHA  END-IF                           
130500     IF W-IDLEVNR-ALPHA = 'CZ4BA'                                         
130600        MOVE '2243 ' TO W-IDLEVNR-ALPHA  END-IF                           
130700     IF W-IDLEVNR-ALPHA = 'DJKTA'                                         
130800        MOVE '2284 ' TO W-IDLEVNR-ALPHA  END-IF                           
130900     IF W-IDLEVNR-ALPHA = 'BK4LA'                                         
131000        MOVE '2322 ' TO W-IDLEVNR-ALPHA  END-IF                           
131100     IF W-IDLEVNR-ALPHA = 'DL0ZA'                                         
131200        MOVE '2344 ' TO W-IDLEVNR-ALPHA  END-IF                           
131300     IF W-IDLEVNR-ALPHA = 'DJKVA'                                         
131400        MOVE '2446 ' TO W-IDLEVNR-ALPHA  END-IF                           
131500     IF W-IDLEVNR-ALPHA = 'BJKNA'                                         
131600        MOVE '2451 ' TO W-IDLEVNR-ALPHA  END-IF                           
131700     IF W-IDLEVNR-ALPHA = 'CD2HA'                                         
131800        MOVE '2480 ' TO W-IDLEVNR-ALPHA  END-IF                           
131900     IF W-IDLEVNR-ALPHA = 'BJYVA'                                         
132000        MOVE '2619 ' TO W-IDLEVNR-ALPHA  END-IF                           
132100     IF W-IDLEVNR-ALPHA = 'BL3AA'                                         
132200        MOVE '2669 ' TO W-IDLEVNR-ALPHA  END-IF                           
132300     IF W-IDLEVNR-ALPHA = 'BLNLA'                                         
132400        MOVE '3105 ' TO W-IDLEVNR-ALPHA  END-IF                           
132500     IF W-IDLEVNR-ALPHA = 'DJKWA'                                         
132600        MOVE '3304 ' TO W-IDLEVNR-ALPHA  END-IF                           
132700     IF W-IDLEVNR-ALPHA = 'DL1RA'                                         
132800        MOVE '3310 ' TO W-IDLEVNR-ALPHA  END-IF                           
132900     IF W-IDLEVNR-ALPHA = 'DL1SA'                                         
133000        MOVE '3332 ' TO W-IDLEVNR-ALPHA  END-IF                           
133100     IF W-IDLEVNR-ALPHA = 'DEV4A'                                         
133200        MOVE '3342 ' TO W-IDLEVNR-ALPHA  END-IF                           
133300     IF W-IDLEVNR-ALPHA = 'DJKXA'                                         
133400        MOVE '3343 ' TO W-IDLEVNR-ALPHA  END-IF                           
133500     IF W-IDLEVNR-ALPHA = 'DJKYA'                                         
133600        MOVE '3345 ' TO W-IDLEVNR-ALPHA  END-IF                           
133700     IF W-IDLEVNR-ALPHA = 'DL1TA'                                         
133800        MOVE '3349 ' TO W-IDLEVNR-ALPHA  END-IF                           
133900     IF W-IDLEVNR-ALPHA = 'DL1UA'                                         
134000        MOVE '3354 ' TO W-IDLEVNR-ALPHA  END-IF                           
134100     IF W-IDLEVNR-ALPHA = 'DJKZA'                                         
134200        MOVE '3369 ' TO W-IDLEVNR-ALPHA  END-IF                           
134300     IF W-IDLEVNR-ALPHA = 'DJK0A'                                         
134400        MOVE '3375 ' TO W-IDLEVNR-ALPHA  END-IF                           
134500     IF W-IDLEVNR-ALPHA = 'DL1VA'                                         
134600        MOVE '3376 ' TO W-IDLEVNR-ALPHA  END-IF                           
134700     IF W-IDLEVNR-ALPHA = 'BQ6SA'                                         
134800        MOVE '3389 ' TO W-IDLEVNR-ALPHA  END-IF                           
134900     IF W-IDLEVNR-ALPHA = 'DJK9A'                                         
135000        MOVE '3401 ' TO W-IDLEVNR-ALPHA  END-IF                           
135100     IF W-IDLEVNR-ALPHA = 'CFJCA'                                         
135200        MOVE '3405 ' TO W-IDLEVNR-ALPHA  END-IF                           
135300     IF W-IDLEVNR-ALPHA = 'BK1EA'                                         
135400        MOVE '3421 ' TO W-IDLEVNR-ALPHA  END-IF                           
135500     IF W-IDLEVNR-ALPHA = 'DJLAA'                                         
135600        MOVE '3423 ' TO W-IDLEVNR-ALPHA  END-IF                           
135700     IF W-IDLEVNR-ALPHA = 'DL1XA'                                         
135800        MOVE '3424 ' TO W-IDLEVNR-ALPHA  END-IF                           
135900*************************************************                         
136000     IF W-IDLEVNR-ALPHA = 'BP8CA'                                         
136100        MOVE '1335 ' TO W-IDLEVNR-ALPHA  END-IF                           
136200     IF W-IDLEVNR-ALPHA = 'BWKFA'                                         
136300        MOVE '1649 ' TO W-IDLEVNR-ALPHA  END-IF                           
136400     IF W-IDLEVNR-ALPHA = 'G13FA'                                         
136500        MOVE '1797 ' TO W-IDLEVNR-ALPHA  END-IF                           
136600     IF W-IDLEVNR-ALPHA = 'BLMPA'                                         
136700        MOVE '2113 ' TO W-IDLEVNR-ALPHA  END-IF                           
136800     IF W-IDLEVNR-ALPHA = 'BQ7HA'                                         
136900        MOVE '2302 ' TO W-IDLEVNR-ALPHA  END-IF                           
137000     IF W-IDLEVNR-ALPHA = 'M279D'                                         
137100        MOVE '3556 ' TO W-IDLEVNR-ALPHA  END-IF                           
137200     IF W-IDLEVNR-ALPHA = 'AB7ZA'                                         
137300        MOVE '4760 ' TO W-IDLEVNR-ALPHA  END-IF                           
137400     IF W-IDLEVNR-ALPHA = 'D0UQA'                                         
137500        MOVE '6191 ' TO W-IDLEVNR-ALPHA  END-IF                           
137600     IF W-IDLEVNR-ALPHA = 'P511A'                                         
137700        MOVE '6354 ' TO W-IDLEVNR-ALPHA  END-IF                           
137800     IF W-IDLEVNR-ALPHA = 'ADBTA'                                         
137900        MOVE '6505 ' TO W-IDLEVNR-ALPHA  END-IF                           
138000     IF W-IDLEVNR-ALPHA = 'B42KA'                                         
138100        MOVE '6512 ' TO W-IDLEVNR-ALPHA  END-IF                           
138200     IF W-IDLEVNR-ALPHA = 'K1FWA'                                         
138300        MOVE '6589 ' TO W-IDLEVNR-ALPHA  END-IF                           
138400     IF W-IDLEVNR-ALPHA = 'M1F7A'                                         
138500        MOVE '6608 ' TO W-IDLEVNR-ALPHA  END-IF                           
138600     IF W-IDLEVNR-ALPHA = 'CX9XA'                                         
138700        MOVE '6650 ' TO W-IDLEVNR-ALPHA  END-IF                           
138800     IF W-IDLEVNR-ALPHA = 'P790A'                                         
138900        MOVE '6665 ' TO W-IDLEVNR-ALPHA  END-IF                           
139000     IF W-IDLEVNR-ALPHA = 'S106A'                                         
139100        MOVE '6669 ' TO W-IDLEVNR-ALPHA  END-IF                           
139200     IF W-IDLEVNR-ALPHA = 'J613A'                                         
139300        MOVE '6684 ' TO W-IDLEVNR-ALPHA  END-IF                           
139400     IF W-IDLEVNR-ALPHA = 'BZ02A'                                         
139500        MOVE '6692 ' TO W-IDLEVNR-ALPHA  END-IF                           
139600     IF W-IDLEVNR-ALPHA = 'B4W7A'                                         
139700        MOVE '6823 ' TO W-IDLEVNR-ALPHA  END-IF                           
139800     IF W-IDLEVNR-ALPHA = 'D0QWA'                                         
139900        MOVE '7277 ' TO W-IDLEVNR-ALPHA  END-IF                           
140000     IF W-IDLEVNR-ALPHA = 'BCSFA'                                         
140100        MOVE '7317 ' TO W-IDLEVNR-ALPHA  END-IF                           
140200     IF W-IDLEVNR-ALPHA = 'BPTMB'                                         
140300        MOVE '7900 ' TO W-IDLEVNR-ALPHA  END-IF                           
140400     IF W-IDLEVNR-ALPHA = 'BPTMD'                                         
140500        MOVE '10159' TO W-IDLEVNR-ALPHA  END-IF                           
140600     IF W-IDLEVNR-ALPHA = 'BPTMA'                                         
140700        MOVE '10160' TO W-IDLEVNR-ALPHA  END-IF                           
140800     IF W-IDLEVNR-ALPHA = 'BPTMC'                                         
140900        MOVE '11377' TO W-IDLEVNR-ALPHA  END-IF                           
141000     IF W-IDLEVNR-ALPHA = 'BP8DB'                                         
141100        MOVE '13585' TO W-IDLEVNR-ALPHA  END-IF                           
141200     IF W-IDLEVNR-ALPHA = 'BP8DD'                                         
141300        MOVE '13622' TO W-IDLEVNR-ALPHA  END-IF                           
141400     IF W-IDLEVNR-ALPHA = 'T8LLA'                                         
141500        MOVE '13801' TO W-IDLEVNR-ALPHA  END-IF                           
141600     IF W-IDLEVNR-ALPHA = 'D06MA'                                         
141700        MOVE '13849' TO W-IDLEVNR-ALPHA  END-IF                           
141800     IF W-IDLEVNR-ALPHA = 'B47PG'                                         
141900        MOVE '14280' TO W-IDLEVNR-ALPHA  END-IF                           
142000     IF W-IDLEVNR-ALPHA = 'CNXXA'                                         
142100        MOVE '14500' TO W-IDLEVNR-ALPHA  END-IF                           
142200     IF W-IDLEVNR-ALPHA = 'BPW0A'                                         
142300        MOVE '14519' TO W-IDLEVNR-ALPHA  END-IF                           
142400     IF W-IDLEVNR-ALPHA = 'C9D2A'                                         
142500        MOVE '14944' TO W-IDLEVNR-ALPHA  END-IF                           
142600     IF W-IDLEVNR-ALPHA = 'BPTME'                                         
142700        MOVE '16036' TO W-IDLEVNR-ALPHA  END-IF                           
142800     IF W-IDLEVNR-ALPHA = 'M279C'                                         
142900        MOVE '16144' TO W-IDLEVNR-ALPHA  END-IF                           
143000     IF W-IDLEVNR-ALPHA = 'M279E'                                         
143100        MOVE '16145' TO W-IDLEVNR-ALPHA  END-IF                           
143200     IF W-IDLEVNR-ALPHA = 'U2W5B'                                         
143300        MOVE '16274' TO W-IDLEVNR-ALPHA  END-IF                           
143400     IF W-IDLEVNR-ALPHA = 'K1FWB'                                         
143500        MOVE '16332' TO W-IDLEVNR-ALPHA  END-IF                           
143600     IF W-IDLEVNR-ALPHA = 'D059D'                                         
143700        MOVE '19564' TO W-IDLEVNR-ALPHA  END-IF                           
143800     IF W-IDLEVNR-ALPHA = 'BP8DC'                                         
143900        MOVE '19609' TO W-IDLEVNR-ALPHA  END-IF                           
144000     IF W-IDLEVNR-ALPHA = 'G1UHN'                                         
144100        MOVE '21590' TO W-IDLEVNR-ALPHA  END-IF                           
144200     IF W-IDLEVNR-ALPHA = 'D30FA'                                         
144300        MOVE '22419' TO W-IDLEVNR-ALPHA  END-IF                           
144400     IF W-IDLEVNR-ALPHA = 'D01QA'                                         
144500        MOVE '22420' TO W-IDLEVNR-ALPHA  END-IF                           
144600     IF W-IDLEVNR-ALPHA = 'D059E'                                         
144700        MOVE '23375' TO W-IDLEVNR-ALPHA  END-IF                           
144800     IF W-IDLEVNR-ALPHA = 'BQ1ZC'                                         
144900        MOVE '23919' TO W-IDLEVNR-ALPHA  END-IF                           
145000     IF W-IDLEVNR-ALPHA = 'D059F'                                         
145100        MOVE '23926' TO W-IDLEVNR-ALPHA  END-IF                           
145200     IF W-IDLEVNR-ALPHA = 'ABD3A'                                         
145300        MOVE '25944' TO W-IDLEVNR-ALPHA  END-IF                           
145400     IF W-IDLEVNR-ALPHA = 'P790M'                                         
145500        MOVE '16149' TO W-IDLEVNR-ALPHA  END-IF                           
145600*************************************************                         
145700     IF W-IDLEVNR-ALPHA = 'DL1YA'                                         
145800        MOVE '3445 ' TO W-IDLEVNR-ALPHA  END-IF                           
145900     IF W-IDLEVNR-ALPHA = 'DLH6A'                                         
146000        MOVE '3449 ' TO W-IDLEVNR-ALPHA  END-IF                           
146100     IF W-IDLEVNR-ALPHA = 'DLH7A'                                         
146200        MOVE '3463 ' TO W-IDLEVNR-ALPHA  END-IF                           
146300     IF W-IDLEVNR-ALPHA = 'DL1ZA'                                         
146400        MOVE '3470 ' TO W-IDLEVNR-ALPHA  END-IF                           
146500     IF W-IDLEVNR-ALPHA = 'DLJAA'                                         
146600        MOVE '3474 ' TO W-IDLEVNR-ALPHA  END-IF                           
146700     IF W-IDLEVNR-ALPHA = 'DLJBA'                                         
146800        MOVE '3482 ' TO W-IDLEVNR-ALPHA  END-IF                           
146900     IF W-IDLEVNR-ALPHA = 'DLJCA'                                         
147000        MOVE '3485 ' TO W-IDLEVNR-ALPHA  END-IF                           
147100     IF W-IDLEVNR-ALPHA = 'DLJDA'                                         
147200        MOVE '3487 ' TO W-IDLEVNR-ALPHA  END-IF                           
147300     IF W-IDLEVNR-ALPHA = 'DLJEA'                                         
147400        MOVE '3495 ' TO W-IDLEVNR-ALPHA  END-IF                           
147500     IF W-IDLEVNR-ALPHA = 'DL2BA'                                         
147600        MOVE '3505 ' TO W-IDLEVNR-ALPHA  END-IF                           
147700     IF W-IDLEVNR-ALPHA = 'BEFXA'                                         
147800        MOVE '3511 ' TO W-IDLEVNR-ALPHA  END-IF                           
147900     IF W-IDLEVNR-ALPHA = 'LYSDA'                                         
148000        MOVE '3537 ' TO W-IDLEVNR-ALPHA  END-IF                           
148100     IF W-IDLEVNR-ALPHA = 'DL2DA'                                         
148200        MOVE '3581 ' TO W-IDLEVNR-ALPHA  END-IF                           
148300     IF W-IDLEVNR-ALPHA = 'KYBHA'                                         
148400        MOVE '3634 ' TO W-IDLEVNR-ALPHA  END-IF                           
148500     IF W-IDLEVNR-ALPHA = 'DLJFA'                                         
148600        MOVE '3641 ' TO W-IDLEVNR-ALPHA  END-IF                           
148700     IF W-IDLEVNR-ALPHA = 'F745A'                                         
148800        MOVE '3660 ' TO W-IDLEVNR-ALPHA  END-IF                           
148900     IF W-IDLEVNR-ALPHA = 'E23VB'                                         
149000        MOVE '3663 ' TO W-IDLEVNR-ALPHA  END-IF                           
149100     IF W-IDLEVNR-ALPHA = 'DLJHA'                                         
149200        MOVE '3681 ' TO W-IDLEVNR-ALPHA  END-IF                           
149300     IF W-IDLEVNR-ALPHA = 'DL2EA'                                         
149400        MOVE '3723 ' TO W-IDLEVNR-ALPHA  END-IF                           
149500     IF W-IDLEVNR-ALPHA = 'DL2FA'                                         
149600        MOVE '3728 ' TO W-IDLEVNR-ALPHA  END-IF                           
149700     IF W-IDLEVNR-ALPHA = 'DL2GA'                                         
149800        MOVE '3732 ' TO W-IDLEVNR-ALPHA  END-IF                           
149900     IF W-IDLEVNR-ALPHA = 'CYMBD'                                         
150000        MOVE '3751 ' TO W-IDLEVNR-ALPHA  END-IF                           
150100     IF W-IDLEVNR-ALPHA = 'DL2HA'                                         
150200        MOVE '3771 ' TO W-IDLEVNR-ALPHA  END-IF                           
150300     IF W-IDLEVNR-ALPHA = 'DLJJA'                                         
150400        MOVE '3790 ' TO W-IDLEVNR-ALPHA  END-IF                           
150500     IF W-IDLEVNR-ALPHA = 'DL2JA'                                         
150600        MOVE '3793 ' TO W-IDLEVNR-ALPHA  END-IF                           
150700     IF W-IDLEVNR-ALPHA = 'DL2KA'                                         
150800        MOVE '3799 ' TO W-IDLEVNR-ALPHA  END-IF                           
150900     IF W-IDLEVNR-ALPHA = 'DL2LA'                                         
151000        MOVE '3814 ' TO W-IDLEVNR-ALPHA  END-IF                           
151100     IF W-IDLEVNR-ALPHA = 'DL2MA'                                         
151200        MOVE '3830 ' TO W-IDLEVNR-ALPHA  END-IF                           
151300     IF W-IDLEVNR-ALPHA = 'DL2NA'                                         
151400        MOVE '3833 ' TO W-IDLEVNR-ALPHA  END-IF                           
151500     IF W-IDLEVNR-ALPHA = 'DLJKA'                                         
151600        MOVE '3855 ' TO W-IDLEVNR-ALPHA  END-IF                           
151700     IF W-IDLEVNR-ALPHA = 'CP6JB'                                         
151800        MOVE '3861 ' TO W-IDLEVNR-ALPHA  END-IF                           
151900     IF W-IDLEVNR-ALPHA = 'DL2PA'                                         
152000        MOVE '3866 ' TO W-IDLEVNR-ALPHA  END-IF                           
152100     IF W-IDLEVNR-ALPHA = 'R57KA'                                         
152200        MOVE '3925 ' TO W-IDLEVNR-ALPHA  END-IF                           
152300     IF W-IDLEVNR-ALPHA = 'D0UCC'                                         
152400        MOVE '3933 ' TO W-IDLEVNR-ALPHA  END-IF                           
152500     IF W-IDLEVNR-ALPHA = 'DL4SA'                                         
152600        MOVE '3938 ' TO W-IDLEVNR-ALPHA  END-IF                           
152700     IF W-IDLEVNR-ALPHA = 'DLJMA'                                         
152800        MOVE '3941 ' TO W-IDLEVNR-ALPHA  END-IF                           
152900     IF W-IDLEVNR-ALPHA = 'DL4TA'                                         
153000        MOVE '3952 ' TO W-IDLEVNR-ALPHA  END-IF                           
153100     IF W-IDLEVNR-ALPHA = 'DLJPA'                                         
153200        MOVE '3957 ' TO W-IDLEVNR-ALPHA  END-IF                           
153300     IF W-IDLEVNR-ALPHA = 'DL5GB'                                         
153400        MOVE '3970 ' TO W-IDLEVNR-ALPHA  END-IF                           
153500     IF W-IDLEVNR-ALPHA = 'LEMWA'                                         
153600        MOVE '3977 ' TO W-IDLEVNR-ALPHA  END-IF                           
153700     IF W-IDLEVNR-ALPHA = 'S601E'                                         
153800        MOVE '4148 ' TO W-IDLEVNR-ALPHA  END-IF                           
153900     IF W-IDLEVNR-ALPHA = 'G8KDA'                                         
154000        MOVE '4256 ' TO W-IDLEVNR-ALPHA  END-IF                           
154100     IF W-IDLEVNR-ALPHA = 'LRT1A'                                         
154200        MOVE '4488 ' TO W-IDLEVNR-ALPHA  END-IF                           
154300     IF W-IDLEVNR-ALPHA = 'DL5HA'                                         
154400        MOVE '4542 ' TO W-IDLEVNR-ALPHA  END-IF                           
154500     IF W-IDLEVNR-ALPHA = 'CFJBA'                                         
154600        MOVE '4637 ' TO W-IDLEVNR-ALPHA  END-IF                           
154700     IF W-IDLEVNR-ALPHA = 'DLJRA'                                         
154800        MOVE '4700 ' TO W-IDLEVNR-ALPHA  END-IF                           
154900     IF W-IDLEVNR-ALPHA = 'JHZZA'                                         
155000        MOVE '4749 ' TO W-IDLEVNR-ALPHA  END-IF                           
155100     IF W-IDLEVNR-ALPHA = 'R6PFB'                                         
155200        MOVE '4845 ' TO W-IDLEVNR-ALPHA  END-IF                           
155300     IF W-IDLEVNR-ALPHA = 'D5Q3F'                                         
155400        MOVE '4934 ' TO W-IDLEVNR-ALPHA  END-IF                           
155500     IF W-IDLEVNR-ALPHA = 'DL6FA'                                         
155600        MOVE '4968 ' TO W-IDLEVNR-ALPHA  END-IF                           
155700     IF W-IDLEVNR-ALPHA = 'DL6JA'                                         
155800        MOVE '5122 ' TO W-IDLEVNR-ALPHA  END-IF                           
155900     IF W-IDLEVNR-ALPHA = 'LRVLA'                                         
156000        MOVE '5161 ' TO W-IDLEVNR-ALPHA  END-IF                           
156100     IF W-IDLEVNR-ALPHA = 'L3PGE'                                         
156200        MOVE '5182 ' TO W-IDLEVNR-ALPHA  END-IF                           
156300     IF W-IDLEVNR-ALPHA = 'D21XA'                                         
156400        MOVE '5233 ' TO W-IDLEVNR-ALPHA  END-IF                           
156500     IF W-IDLEVNR-ALPHA = 'C8W0A'                                         
156600        MOVE '5295 ' TO W-IDLEVNR-ALPHA  END-IF                           
156700     IF W-IDLEVNR-ALPHA = 'LHSYA'                                         
156800        MOVE '5354 ' TO W-IDLEVNR-ALPHA  END-IF                           
156900     IF W-IDLEVNR-ALPHA = 'AADLA'                                         
157000        MOVE '5436 ' TO W-IDLEVNR-ALPHA  END-IF                           
157100     IF W-IDLEVNR-ALPHA = 'DLJSA'                                         
157200        MOVE '5443 ' TO W-IDLEVNR-ALPHA  END-IF                           
157300     IF W-IDLEVNR-ALPHA = 'DLJTA'                                         
157400        MOVE '5637 ' TO W-IDLEVNR-ALPHA  END-IF                           
157500     IF W-IDLEVNR-ALPHA = 'D26YA'                                         
157600        MOVE '5671 ' TO W-IDLEVNR-ALPHA  END-IF                           
157700     IF W-IDLEVNR-ALPHA = 'MRZ4A'                                         
157800        MOVE '5678 ' TO W-IDLEVNR-ALPHA  END-IF                           
157900     IF W-IDLEVNR-ALPHA = 'DLK7A'                                         
158000        MOVE '6016 ' TO W-IDLEVNR-ALPHA  END-IF                           
158100     IF W-IDLEVNR-ALPHA = 'D1H7A'                                         
158200        MOVE '6024 ' TO W-IDLEVNR-ALPHA  END-IF                           
158300     IF W-IDLEVNR-ALPHA = 'DLLAA'                                         
158400        MOVE '6045 ' TO W-IDLEVNR-ALPHA  END-IF                           
158500     IF W-IDLEVNR-ALPHA = 'B42DA'                                         
158600        MOVE '6053 ' TO W-IDLEVNR-ALPHA  END-IF                           
158700     IF W-IDLEVNR-ALPHA = 'C8F4A'                                         
158800        MOVE '6061 ' TO W-IDLEVNR-ALPHA  END-IF                           
158900     IF W-IDLEVNR-ALPHA = 'EGX6K'                                         
159000        MOVE '6066 ' TO W-IDLEVNR-ALPHA  END-IF                           
159100     IF W-IDLEVNR-ALPHA = 'DLLBA'                                         
159200        MOVE '6069 ' TO W-IDLEVNR-ALPHA  END-IF                           
159300     IF W-IDLEVNR-ALPHA = 'B40WB'                                         
159400        MOVE '6080 ' TO W-IDLEVNR-ALPHA  END-IF                           
159500     IF W-IDLEVNR-ALPHA = 'D04DA'                                         
159600        MOVE '6098 ' TO W-IDLEVNR-ALPHA  END-IF                           
159700     IF W-IDLEVNR-ALPHA = 'D33QB'                                         
159800        MOVE '6120 ' TO W-IDLEVNR-ALPHA  END-IF                           
159900     IF W-IDLEVNR-ALPHA = 'D23JB'                                         
160000        MOVE '6151 ' TO W-IDLEVNR-ALPHA  END-IF                           
160100     IF W-IDLEVNR-ALPHA = 'DZK7A'                                         
160200        MOVE '6158 ' TO W-IDLEVNR-ALPHA  END-IF                           
160300     IF W-IDLEVNR-ALPHA = 'C8S2B'                                         
160400        MOVE '6202 ' TO W-IDLEVNR-ALPHA  END-IF                           
160500     IF W-IDLEVNR-ALPHA = 'D8NLJ'                                         
160600        MOVE '6228 ' TO W-IDLEVNR-ALPHA  END-IF                           
160700     IF W-IDLEVNR-ALPHA = 'DLLCA'                                         
160800        MOVE '6245 ' TO W-IDLEVNR-ALPHA  END-IF                           
160900     IF W-IDLEVNR-ALPHA = 'D0NNA'                                         
161000        MOVE '6269 ' TO W-IDLEVNR-ALPHA  END-IF                           
161100     IF W-IDLEVNR-ALPHA = 'D0SYA'                                         
161200        MOVE '6283 ' TO W-IDLEVNR-ALPHA  END-IF                           
161300*************************************************                         
161400     IF W-IDLEVNR-ALPHA = 'BQ8YA'                                         
161500        MOVE '80   ' TO W-IDLEVNR-ALPHA  END-IF                           
161600     IF W-IDLEVNR-ALPHA = 'BQ0FA'                                         
161700        MOVE '511  ' TO W-IDLEVNR-ALPHA  END-IF                           
161800     IF W-IDLEVNR-ALPHA = 'BQAGA'                                         
161900        MOVE '894  ' TO W-IDLEVNR-ALPHA  END-IF                           
162000     IF W-IDLEVNR-ALPHA = 'S5PQB'                                         
162100        MOVE '1345 ' TO W-IDLEVNR-ALPHA  END-IF                           
162200     IF W-IDLEVNR-ALPHA = 'LESLA'                                         
162300        MOVE '2333 ' TO W-IDLEVNR-ALPHA  END-IF                           
162400     IF W-IDLEVNR-ALPHA = 'BQ6MA'                                         
162500        MOVE '3050 ' TO W-IDLEVNR-ALPHA  END-IF                           
162600     IF W-IDLEVNR-ALPHA = 'L9GXB'                                         
162700        MOVE '3135 ' TO W-IDLEVNR-ALPHA  END-IF                           
162800     IF W-IDLEVNR-ALPHA = 'L9GXA'                                         
162900        MOVE '3594 ' TO W-IDLEVNR-ALPHA  END-IF                           
163000     IF W-IDLEVNR-ALPHA = 'C9A3A'                                         
163100        MOVE '3616 ' TO W-IDLEVNR-ALPHA  END-IF                           
163200     IF W-IDLEVNR-ALPHA = 'S356A'                                         
163300        MOVE '3752 ' TO W-IDLEVNR-ALPHA  END-IF                           
163400     IF W-IDLEVNR-ALPHA = 'AYSCB'                                         
163500        MOVE '3755 ' TO W-IDLEVNR-ALPHA  END-IF                           
163600     IF W-IDLEVNR-ALPHA = 'E23LA'                                         
163700        MOVE '3912 ' TO W-IDLEVNR-ALPHA  END-IF                           
163800     IF W-IDLEVNR-ALPHA = 'D04JA'                                         
163900        MOVE '4515 ' TO W-IDLEVNR-ALPHA  END-IF                           
164000     IF W-IDLEVNR-ALPHA = 'A628A'                                         
164100        MOVE '5049 ' TO W-IDLEVNR-ALPHA  END-IF                           
164200     IF W-IDLEVNR-ALPHA = 'C66SJ'                                         
164300        MOVE '5085 ' TO W-IDLEVNR-ALPHA  END-IF                           
164400     IF W-IDLEVNR-ALPHA = 'D3C3A'                                         
164500        MOVE '5093 ' TO W-IDLEVNR-ALPHA  END-IF                           
164600     IF W-IDLEVNR-ALPHA = 'C8V8A'                                         
164700        MOVE '5744 ' TO W-IDLEVNR-ALPHA  END-IF                           
164800     IF W-IDLEVNR-ALPHA = 'F4SWG'                                         
164900        MOVE '6022 ' TO W-IDLEVNR-ALPHA  END-IF                           
165000     IF W-IDLEVNR-ALPHA = 'AN3AA'                                         
165100        MOVE '6118 ' TO W-IDLEVNR-ALPHA  END-IF                           
165200     IF W-IDLEVNR-ALPHA = 'H518X'                                         
165300        MOVE '6215 ' TO W-IDLEVNR-ALPHA  END-IF                           
165400     IF W-IDLEVNR-ALPHA = 'CJ6EA'                                         
165500        MOVE '6345 ' TO W-IDLEVNR-ALPHA  END-IF                           
165600     IF W-IDLEVNR-ALPHA = 'B492E'                                         
165700        MOVE '6538 ' TO W-IDLEVNR-ALPHA  END-IF                           
165800     IF W-IDLEVNR-ALPHA = 'B492A'                                         
165900        MOVE '6587 ' TO W-IDLEVNR-ALPHA  END-IF                           
166000     IF W-IDLEVNR-ALPHA = 'C8W2A'                                         
166100        MOVE '7213 ' TO W-IDLEVNR-ALPHA  END-IF                           
166200     IF W-IDLEVNR-ALPHA = 'CFT5A'                                         
166300        MOVE '7609 ' TO W-IDLEVNR-ALPHA  END-IF                           
166400     IF W-IDLEVNR-ALPHA = 'BPLDA'                                         
166500        MOVE '10131' TO W-IDLEVNR-ALPHA  END-IF                           
166600     IF W-IDLEVNR-ALPHA = 'BPLDD'                                         
166700        MOVE '10138' TO W-IDLEVNR-ALPHA  END-IF                           
166800     IF W-IDLEVNR-ALPHA = 'CRK8A'                                         
166900        MOVE '11099' TO W-IDLEVNR-ALPHA  END-IF                           
167000     IF W-IDLEVNR-ALPHA = 'BPLDC'                                         
167100        MOVE '13633' TO W-IDLEVNR-ALPHA  END-IF                           
167200     IF W-IDLEVNR-ALPHA = 'C7U7A'                                         
167300        MOVE '14493' TO W-IDLEVNR-ALPHA  END-IF                           
167400     IF W-IDLEVNR-ALPHA = 'C685B'                                         
167500        MOVE '16213' TO W-IDLEVNR-ALPHA  END-IF                           
167600     IF W-IDLEVNR-ALPHA = 'R19YA'                                         
167700        MOVE '17779' TO W-IDLEVNR-ALPHA  END-IF                           
167800     IF W-IDLEVNR-ALPHA = 'BTTTA'                                         
167900        MOVE '23071' TO W-IDLEVNR-ALPHA  END-IF                           
168000     IF W-IDLEVNR-ALPHA = 'CYVBA'                                         
168100        MOVE '23937' TO W-IDLEVNR-ALPHA  END-IF                           
168200     IF W-IDLEVNR-ALPHA = 'DSKFA'                                         
168300        MOVE '26013' TO W-IDLEVNR-ALPHA  END-IF                           
168400*************************************************                         
168500     IF W-IDLEVNR-ALPHA = 'E520A'                                         
168600        MOVE '6293 ' TO W-IDLEVNR-ALPHA  END-IF                           
168700     IF W-IDLEVNR-ALPHA = 'DL6MA'                                         
168800        MOVE '6307 ' TO W-IDLEVNR-ALPHA  END-IF                           
168900     IF W-IDLEVNR-ALPHA = 'H8Z2A'                                         
169000        MOVE '6326 ' TO W-IDLEVNR-ALPHA  END-IF                           
169100     IF W-IDLEVNR-ALPHA = 'DL6PA'                                         
169200        MOVE '6357 ' TO W-IDLEVNR-ALPHA  END-IF                           
169300     IF W-IDLEVNR-ALPHA = 'DMZCA'                                         
169400        MOVE '6410 ' TO W-IDLEVNR-ALPHA  END-IF                           
169500     IF W-IDLEVNR-ALPHA = 'U0VSA'                                         
169600        MOVE '6425 ' TO W-IDLEVNR-ALPHA  END-IF                           
169700     IF W-IDLEVNR-ALPHA = 'C91WA'                                         
169800        MOVE '6429 ' TO W-IDLEVNR-ALPHA  END-IF                           
169900     IF W-IDLEVNR-ALPHA = 'DLLFA'                                         
170000        MOVE '6430 ' TO W-IDLEVNR-ALPHA  END-IF                           
170100     IF W-IDLEVNR-ALPHA = 'V04BA'                                         
170200        MOVE '6510 ' TO W-IDLEVNR-ALPHA  END-IF                           
170300     IF W-IDLEVNR-ALPHA = 'C7F4A'                                         
170400        MOVE '6524 ' TO W-IDLEVNR-ALPHA  END-IF                           
170500     IF W-IDLEVNR-ALPHA = 'DLLGA'                                         
170600        MOVE '6562 ' TO W-IDLEVNR-ALPHA  END-IF                           
170700     IF W-IDLEVNR-ALPHA = 'L9SYA'                                         
170800        MOVE '6595 ' TO W-IDLEVNR-ALPHA  END-IF                           
170900     IF W-IDLEVNR-ALPHA = 'DLLJA'                                         
171000        MOVE '6603 ' TO W-IDLEVNR-ALPHA  END-IF                           
171100     IF W-IDLEVNR-ALPHA = 'DLLKA'                                         
171200        MOVE '6641 ' TO W-IDLEVNR-ALPHA  END-IF                           
171300     IF W-IDLEVNR-ALPHA = 'DLLLA'                                         
171400        MOVE '6643 ' TO W-IDLEVNR-ALPHA  END-IF                           
171500     IF W-IDLEVNR-ALPHA = 'CKBRA'                                         
171600        MOVE '6656 ' TO W-IDLEVNR-ALPHA  END-IF                           
171700     IF W-IDLEVNR-ALPHA = 'JFYXA'                                         
171800        MOVE '6691 ' TO W-IDLEVNR-ALPHA  END-IF                           
171900     IF W-IDLEVNR-ALPHA = 'C8Q3A'                                         
172000        MOVE '6698 ' TO W-IDLEVNR-ALPHA  END-IF                           
172100     IF W-IDLEVNR-ALPHA = 'DL6QA'                                         
172200        MOVE '6714 ' TO W-IDLEVNR-ALPHA  END-IF                           
172300     IF W-IDLEVNR-ALPHA = 'DLLNA'                                         
172400        MOVE '6761 ' TO W-IDLEVNR-ALPHA  END-IF                           
172500     IF W-IDLEVNR-ALPHA = 'V0TAA'                                         
172600        MOVE '6783 ' TO W-IDLEVNR-ALPHA  END-IF                           
172700     IF W-IDLEVNR-ALPHA = 'C92KA'                                         
172800        MOVE '6812 ' TO W-IDLEVNR-ALPHA  END-IF                           
172900     IF W-IDLEVNR-ALPHA = 'D25WB'                                         
173000        MOVE '6825 ' TO W-IDLEVNR-ALPHA  END-IF                           
173100     IF W-IDLEVNR-ALPHA = 'P4WSA'                                         
173200        MOVE '6847 ' TO W-IDLEVNR-ALPHA  END-IF                           
173300     IF W-IDLEVNR-ALPHA = 'A708A'                                         
173400        MOVE '6869 ' TO W-IDLEVNR-ALPHA  END-IF                           
173500     IF W-IDLEVNR-ALPHA = 'S3C3A'                                         
173600        MOVE '6870 ' TO W-IDLEVNR-ALPHA  END-IF                           
173700     IF W-IDLEVNR-ALPHA = 'DL6TA'                                         
173800        MOVE '6885 ' TO W-IDLEVNR-ALPHA  END-IF                           
173900     IF W-IDLEVNR-ALPHA = 'DL6VA'                                         
174000        MOVE '6892 ' TO W-IDLEVNR-ALPHA  END-IF                           
174100     IF W-IDLEVNR-ALPHA = 'CN5FA'                                         
174200        MOVE '6899 ' TO W-IDLEVNR-ALPHA  END-IF                           
174300     IF W-IDLEVNR-ALPHA = 'N82PA'                                         
174400        MOVE '7038 ' TO W-IDLEVNR-ALPHA  END-IF                           
174500     IF W-IDLEVNR-ALPHA = 'DLLPA'                                         
174600        MOVE '7106 ' TO W-IDLEVNR-ALPHA  END-IF                           
174700     IF W-IDLEVNR-ALPHA = 'HCR1A'                                         
174800        MOVE '7115 ' TO W-IDLEVNR-ALPHA  END-IF                           
174900     IF W-IDLEVNR-ALPHA = 'DLLQA'                                         
175000        MOVE '7132 ' TO W-IDLEVNR-ALPHA  END-IF                           
175100     IF W-IDLEVNR-ALPHA = 'MTJFA'                                         
175200        MOVE '7134 ' TO W-IDLEVNR-ALPHA  END-IF                           
175300     IF W-IDLEVNR-ALPHA = 'MWAJB'                                         
175400        MOVE '7229 ' TO W-IDLEVNR-ALPHA  END-IF                           
175500     IF W-IDLEVNR-ALPHA = 'DL6WA'                                         
175600        MOVE '7235 ' TO W-IDLEVNR-ALPHA  END-IF                           
175700     IF W-IDLEVNR-ALPHA = 'DLLRA'                                         
175800        MOVE '7357 ' TO W-IDLEVNR-ALPHA  END-IF                           
175900     IF W-IDLEVNR-ALPHA = 'DL6YA'                                         
176000        MOVE '7831 ' TO W-IDLEVNR-ALPHA  END-IF                           
176100     IF W-IDLEVNR-ALPHA = 'MHQSA'                                         
176200        MOVE '7955 ' TO W-IDLEVNR-ALPHA  END-IF                           
176300     IF W-IDLEVNR-ALPHA = 'BLLMA'                                         
176400        MOVE '8004 ' TO W-IDLEVNR-ALPHA  END-IF                           
176500     IF W-IDLEVNR-ALPHA = 'BJW6A'                                         
176600        MOVE '8010 ' TO W-IDLEVNR-ALPHA  END-IF                           
176700     IF W-IDLEVNR-ALPHA = 'BKXUA'                                         
176800        MOVE '8023 ' TO W-IDLEVNR-ALPHA  END-IF                           
176900     IF W-IDLEVNR-ALPHA = 'BLMJA'                                         
177000        MOVE '8061 ' TO W-IDLEVNR-ALPHA  END-IF                           
177100     IF W-IDLEVNR-ALPHA = 'DLLTA'                                         
177200        MOVE '8086 ' TO W-IDLEVNR-ALPHA  END-IF                           
177300     IF W-IDLEVNR-ALPHA = 'BKYNA'                                         
177400        MOVE '8094 ' TO W-IDLEVNR-ALPHA  END-IF                           
177500     IF W-IDLEVNR-ALPHA = 'BK5LA'                                         
177600        MOVE '8103 ' TO W-IDLEVNR-ALPHA  END-IF                           
177700     IF W-IDLEVNR-ALPHA = 'DLLUA'                                         
177800        MOVE '8107 ' TO W-IDLEVNR-ALPHA  END-IF                           
177900     IF W-IDLEVNR-ALPHA = 'DL6ZA'                                         
178000        MOVE '8123 ' TO W-IDLEVNR-ALPHA  END-IF                           
178100     IF W-IDLEVNR-ALPHA = 'D13DA'                                         
178200        MOVE '8138 ' TO W-IDLEVNR-ALPHA  END-IF                           
178300     IF W-IDLEVNR-ALPHA = 'BSN7A'                                         
178400        MOVE '8150 ' TO W-IDLEVNR-ALPHA  END-IF                           
178500     IF W-IDLEVNR-ALPHA = 'DLLVA'                                         
178600        MOVE '8151 ' TO W-IDLEVNR-ALPHA  END-IF                           
178700     IF W-IDLEVNR-ALPHA = 'S6LPA'                                         
178800        MOVE '8168 ' TO W-IDLEVNR-ALPHA  END-IF                           
178900     IF W-IDLEVNR-ALPHA = 'CZTVA'                                         
179000        MOVE '8181 ' TO W-IDLEVNR-ALPHA  END-IF                           
179100     IF W-IDLEVNR-ALPHA = 'BSFPA'                                         
179200        MOVE '8190 ' TO W-IDLEVNR-ALPHA  END-IF                           
179300     IF W-IDLEVNR-ALPHA = 'DLLWA'                                         
179400        MOVE '8192 ' TO W-IDLEVNR-ALPHA  END-IF                           
179500     IF W-IDLEVNR-ALPHA = 'BSKXA'                                         
179600        MOVE '8199 ' TO W-IDLEVNR-ALPHA  END-IF                           
179700     IF W-IDLEVNR-ALPHA = 'DL7AA'                                         
179800        MOVE '8203 ' TO W-IDLEVNR-ALPHA  END-IF                           
179900     IF W-IDLEVNR-ALPHA = 'DLLXA'                                         
180000        MOVE '8212 ' TO W-IDLEVNR-ALPHA  END-IF                           
180100     IF W-IDLEVNR-ALPHA = 'S98JA'                                         
180200        MOVE '8216 ' TO W-IDLEVNR-ALPHA  END-IF                           
180300     IF W-IDLEVNR-ALPHA = 'DLLYA'                                         
180400        MOVE '8233 ' TO W-IDLEVNR-ALPHA  END-IF                           
180500     IF W-IDLEVNR-ALPHA = 'DLLZA'                                         
180600        MOVE '8234 ' TO W-IDLEVNR-ALPHA  END-IF                           
180700     IF W-IDLEVNR-ALPHA = 'BAG3A'                                         
180800        MOVE '8860 ' TO W-IDLEVNR-ALPHA  END-IF                           
180900     IF W-IDLEVNR-ALPHA = 'S7YJA'                                         
181000        MOVE '10139' TO W-IDLEVNR-ALPHA  END-IF                           
181100     IF W-IDLEVNR-ALPHA = 'DL7CA'                                         
181200        MOVE '10355' TO W-IDLEVNR-ALPHA  END-IF                           
181300     IF W-IDLEVNR-ALPHA = 'DLL0A'                                         
181400        MOVE '10803' TO W-IDLEVNR-ALPHA  END-IF                           
181500     IF W-IDLEVNR-ALPHA = 'CRJ8A'                                         
181600        MOVE '10910' TO W-IDLEVNR-ALPHA  END-IF                           
181700     IF W-IDLEVNR-ALPHA = 'Q5FPA'                                         
181800        MOVE '11117' TO W-IDLEVNR-ALPHA  END-IF                           
181900     IF W-IDLEVNR-ALPHA = 'D1K4E'                                         
182000        MOVE '12089' TO W-IDLEVNR-ALPHA  END-IF                           
182100     IF W-IDLEVNR-ALPHA = 'BA7ZA'                                         
182200        MOVE '12098' TO W-IDLEVNR-ALPHA  END-IF                           
182300     IF W-IDLEVNR-ALPHA = 'DLL5A'                                         
182400        MOVE '13311' TO W-IDLEVNR-ALPHA  END-IF                           
182500     IF W-IDLEVNR-ALPHA = 'DL7GA'                                         
182600        MOVE '13344' TO W-IDLEVNR-ALPHA  END-IF                           
182700     IF W-IDLEVNR-ALPHA = 'DL7HA'                                         
182800        MOVE '13346' TO W-IDLEVNR-ALPHA  END-IF                           
182900     IF W-IDLEVNR-ALPHA = 'DL7JA'                                         
183000        MOVE '13348' TO W-IDLEVNR-ALPHA  END-IF                           
183100     IF W-IDLEVNR-ALPHA = 'C62JC'                                         
183200        MOVE '13362' TO W-IDLEVNR-ALPHA  END-IF                           
183300     IF W-IDLEVNR-ALPHA = 'DLMEA'                                         
183400        MOVE '13374' TO W-IDLEVNR-ALPHA  END-IF                           
183500     IF W-IDLEVNR-ALPHA = 'DLMFA'                                         
183600        MOVE '13375' TO W-IDLEVNR-ALPHA  END-IF                           
183700     IF W-IDLEVNR-ALPHA = 'DLMGA'                                         
183800        MOVE '13376' TO W-IDLEVNR-ALPHA  END-IF                           
183900*************************************************                         
184000     IF W-IDLEVNR-ALPHA = 'DLMHA'                                         
184100        MOVE '13379' TO W-IDLEVNR-ALPHA  END-IF                           
184200     IF W-IDLEVNR-ALPHA = 'D3W5A'                                         
184300        MOVE '13381' TO W-IDLEVNR-ALPHA  END-IF                           
184400     IF W-IDLEVNR-ALPHA = 'CT3NA'                                         
184500        MOVE '13383' TO W-IDLEVNR-ALPHA  END-IF                           
184600     IF W-IDLEVNR-ALPHA = 'DLMJA'                                         
184700        MOVE '13384' TO W-IDLEVNR-ALPHA  END-IF                           
184800     IF W-IDLEVNR-ALPHA = 'CGECA'                                         
184900        MOVE '13385' TO W-IDLEVNR-ALPHA  END-IF                           
185000     IF W-IDLEVNR-ALPHA = 'DLMLA'                                         
185100        MOVE '13390' TO W-IDLEVNR-ALPHA  END-IF                           
185200     IF W-IDLEVNR-ALPHA = 'DLMMA'                                         
185300        MOVE '13392' TO W-IDLEVNR-ALPHA  END-IF                           
185400     IF W-IDLEVNR-ALPHA = 'DLMPA'                                         
185500        MOVE '13401' TO W-IDLEVNR-ALPHA  END-IF                           
185600     IF W-IDLEVNR-ALPHA = 'DL7MA'                                         
185700        MOVE '13402' TO W-IDLEVNR-ALPHA  END-IF                           
185800     IF W-IDLEVNR-ALPHA = 'CFJDA'                                         
185900        MOVE '13403' TO W-IDLEVNR-ALPHA  END-IF                           
186000     IF W-IDLEVNR-ALPHA = 'DLMSA'                                         
186100        MOVE '13408' TO W-IDLEVNR-ALPHA  END-IF                           
186200     IF W-IDLEVNR-ALPHA = 'DLMTA'                                         
186300        MOVE '13409' TO W-IDLEVNR-ALPHA  END-IF                           
186400     IF W-IDLEVNR-ALPHA = 'DL7NA'                                         
186500        MOVE '13411' TO W-IDLEVNR-ALPHA  END-IF                           
186600     IF W-IDLEVNR-ALPHA = 'DLMUA'                                         
186700        MOVE '13416' TO W-IDLEVNR-ALPHA  END-IF                           
186800     IF W-IDLEVNR-ALPHA = 'DLMVA'                                         
186900        MOVE '13463' TO W-IDLEVNR-ALPHA  END-IF                           
187000     IF W-IDLEVNR-ALPHA = 'DLMWA'                                         
187100        MOVE '13465' TO W-IDLEVNR-ALPHA  END-IF                           
187200     IF W-IDLEVNR-ALPHA = 'BQMJA'                                         
187300        MOVE '13467' TO W-IDLEVNR-ALPHA  END-IF                           
187400     IF W-IDLEVNR-ALPHA = 'DL7RA'                                         
187500        MOVE '13475' TO W-IDLEVNR-ALPHA  END-IF                           
187600     IF W-IDLEVNR-ALPHA = 'AMAAC'                                         
187700        MOVE '13529' TO W-IDLEVNR-ALPHA  END-IF                           
187800     IF W-IDLEVNR-ALPHA = 'LRZ7A'                                         
187900        MOVE '13561' TO W-IDLEVNR-ALPHA  END-IF                           
188000     IF W-IDLEVNR-ALPHA = 'DL7VA'                                         
188100        MOVE '13565' TO W-IDLEVNR-ALPHA  END-IF                           
188200     IF W-IDLEVNR-ALPHA = 'BVNUC'                                         
188300        MOVE '13576' TO W-IDLEVNR-ALPHA  END-IF                           
188400     IF W-IDLEVNR-ALPHA = 'DLMYA'                                         
188500        MOVE '13621' TO W-IDLEVNR-ALPHA  END-IF                           
188600     IF W-IDLEVNR-ALPHA = 'DL7XA'                                         
188700        MOVE '13776' TO W-IDLEVNR-ALPHA  END-IF                           
188800     IF W-IDLEVNR-ALPHA = 'Q99AA'                                         
188900        MOVE '14483' TO W-IDLEVNR-ALPHA  END-IF                           
189000     IF W-IDLEVNR-ALPHA = 'CFH6A'                                         
189100        MOVE '14578' TO W-IDLEVNR-ALPHA  END-IF                           
189200     IF W-IDLEVNR-ALPHA = 'DLMZA'                                         
189300        MOVE '14592' TO W-IDLEVNR-ALPHA  END-IF                           
189400     IF W-IDLEVNR-ALPHA = 'DLM1A'                                         
189500        MOVE '14921' TO W-IDLEVNR-ALPHA  END-IF                           
189600     IF W-IDLEVNR-ALPHA = 'DLM3A'                                         
189700        MOVE '14935' TO W-IDLEVNR-ALPHA  END-IF                           
189800     IF W-IDLEVNR-ALPHA = 'DLNBA'                                         
189900        MOVE '14935' TO W-IDLEVNR-ALPHA  END-IF                           
190000     IF W-IDLEVNR-ALPHA = 'MHLTA'                                         
190100        MOVE '15065' TO W-IDLEVNR-ALPHA  END-IF                           
190200     IF W-IDLEVNR-ALPHA = 'DLNCA'                                         
190300        MOVE '15180' TO W-IDLEVNR-ALPHA  END-IF                           
190400     IF W-IDLEVNR-ALPHA = 'DLNDA'                                         
190500        MOVE '15207' TO W-IDLEVNR-ALPHA  END-IF                           
190600     IF W-IDLEVNR-ALPHA = 'N6WEA'                                         
190700        MOVE '15227' TO W-IDLEVNR-ALPHA  END-IF                           
190800     IF W-IDLEVNR-ALPHA = 'N7381'                                         
190900        MOVE '15256' TO W-IDLEVNR-ALPHA  END-IF                           
191000     IF W-IDLEVNR-ALPHA = 'DLNEA'                                         
191100        MOVE '15260' TO W-IDLEVNR-ALPHA  END-IF                           
191200     IF W-IDLEVNR-ALPHA = 'DLNFA'                                         
191300        MOVE '15266' TO W-IDLEVNR-ALPHA  END-IF                           
191400     IF W-IDLEVNR-ALPHA = 'DLNGA'                                         
191500        MOVE '15310' TO W-IDLEVNR-ALPHA  END-IF                           
191600     IF W-IDLEVNR-ALPHA = 'DLNHA'                                         
191700        MOVE '15322' TO W-IDLEVNR-ALPHA  END-IF                           
191800     IF W-IDLEVNR-ALPHA = 'DL8BA'                                         
191900        MOVE '15325' TO W-IDLEVNR-ALPHA  END-IF                           
192000     IF W-IDLEVNR-ALPHA = 'DL8CA'                                         
192100        MOVE '16087' TO W-IDLEVNR-ALPHA  END-IF                           
192200     IF W-IDLEVNR-ALPHA = 'S3MZA'                                         
192300        MOVE '16090' TO W-IDLEVNR-ALPHA  END-IF                           
192400     IF W-IDLEVNR-ALPHA = 'CN5PA'                                         
192500        MOVE '16116' TO W-IDLEVNR-ALPHA  END-IF                           
192600     IF W-IDLEVNR-ALPHA = 'T93AB'                                         
192700        MOVE '16123' TO W-IDLEVNR-ALPHA  END-IF                           
192800     IF W-IDLEVNR-ALPHA = 'K760B'                                         
192900        MOVE '16367' TO W-IDLEVNR-ALPHA  END-IF                           
193000     IF W-IDLEVNR-ALPHA = 'S5S2A'                                         
193100        MOVE '16466' TO W-IDLEVNR-ALPHA  END-IF                           
193200     IF W-IDLEVNR-ALPHA = 'D3R3A'                                         
193300        MOVE '17782' TO W-IDLEVNR-ALPHA  END-IF                           
193400     IF W-IDLEVNR-ALPHA = 'C9G4A'                                         
193500        MOVE '17784' TO W-IDLEVNR-ALPHA  END-IF                           
193600     IF W-IDLEVNR-ALPHA = 'DLNLA'                                         
193700        MOVE '17785' TO W-IDLEVNR-ALPHA  END-IF                           
193800     IF W-IDLEVNR-ALPHA = 'DLNMA'                                         
193900        MOVE '17792' TO W-IDLEVNR-ALPHA  END-IF                           
194000     IF W-IDLEVNR-ALPHA = 'DLNNA'                                         
194100        MOVE '17813' TO W-IDLEVNR-ALPHA  END-IF                           
194200     IF W-IDLEVNR-ALPHA = 'BKPQA'                                         
194300        MOVE '18000' TO W-IDLEVNR-ALPHA  END-IF                           
194400     IF W-IDLEVNR-ALPHA = 'BKHYA'                                         
194500        MOVE '18012' TO W-IDLEVNR-ALPHA  END-IF                           
194600     IF W-IDLEVNR-ALPHA = 'BSK0A'                                         
194700        MOVE '18060' TO W-IDLEVNR-ALPHA  END-IF                           
194800     IF W-IDLEVNR-ALPHA = 'DLNQA'                                         
194900        MOVE '18067' TO W-IDLEVNR-ALPHA  END-IF                           
195000     IF W-IDLEVNR-ALPHA = 'DLNRA'                                         
195100        MOVE '18075' TO W-IDLEVNR-ALPHA  END-IF                           
195200     IF W-IDLEVNR-ALPHA = 'CXC8A'                                         
195300        MOVE '18120' TO W-IDLEVNR-ALPHA  END-IF                           
195400     IF W-IDLEVNR-ALPHA = 'DLNSA'                                         
195500        MOVE '18977' TO W-IDLEVNR-ALPHA  END-IF                           
195600     IF W-IDLEVNR-ALPHA = 'DLNTA'                                         
195700        MOVE '19052' TO W-IDLEVNR-ALPHA  END-IF                           
195800     IF W-IDLEVNR-ALPHA = 'BK4FA'                                         
195900        MOVE '19235' TO W-IDLEVNR-ALPHA  END-IF                           
196200     IF W-IDLEVNR-ALPHA = 'AYGHA'                                         
196300        MOVE '20895' TO W-IDLEVNR-ALPHA  END-IF                           
196400     IF W-IDLEVNR-ALPHA = 'CECDA'                                         
196500        MOVE '21580' TO W-IDLEVNR-ALPHA  END-IF                           
196600     IF W-IDLEVNR-ALPHA = 'CGSCA'                                         
196700        MOVE '23939' TO W-IDLEVNR-ALPHA  END-IF                           
196800     IF W-IDLEVNR-ALPHA = 'DL8EA'                                         
196900        MOVE '23941' TO W-IDLEVNR-ALPHA  END-IF                           
197000     IF W-IDLEVNR-ALPHA = 'ATPUB'                                         
197100        MOVE '24633' TO W-IDLEVNR-ALPHA  END-IF                           
197200     IF W-IDLEVNR-ALPHA = 'DL8FA'                                         
197300        MOVE '24837' TO W-IDLEVNR-ALPHA  END-IF                           
197400     IF W-IDLEVNR-ALPHA = 'DLNWA'                                         
197500        MOVE '25012' TO W-IDLEVNR-ALPHA  END-IF                           
197600     IF W-IDLEVNR-ALPHA = 'BLWQA'                                         
197700        MOVE '25745' TO W-IDLEVNR-ALPHA  END-IF                           
197800     IF W-IDLEVNR-ALPHA = 'DN6EA'                                         
197900        MOVE '25955' TO W-IDLEVNR-ALPHA  END-IF                           
198000     IF W-IDLEVNR-ALPHA = 'A224A'                                         
198100        MOVE '50049' TO W-IDLEVNR-ALPHA  END-IF                           
198200     IF W-IDLEVNR-ALPHA = 'G8VUE'                                         
198300        MOVE '55005' TO W-IDLEVNR-ALPHA  END-IF                           
198400     IF W-IDLEVNR-ALPHA = 'CZ6AB'                                         
198500        MOVE '80096' TO W-IDLEVNR-ALPHA  END-IF                           
198600*************************************************                         
198700     IF W-IDLEVNR-ALPHA = 'BQYJA'                                         
198800        MOVE '87   ' TO W-IDLEVNR-ALPHA  END-IF                           
198900     IF W-IDLEVNR-ALPHA = 'N81ZA'                                         
199000        MOVE '1186 ' TO W-IDLEVNR-ALPHA  END-IF                           
199100     IF W-IDLEVNR-ALPHA = 'S6NKA'                                         
199200        MOVE '1447 ' TO W-IDLEVNR-ALPHA  END-IF                           
199300     IF W-IDLEVNR-ALPHA = 'BVNQA'                                         
199400        MOVE '2318 ' TO W-IDLEVNR-ALPHA  END-IF                           
199500     IF W-IDLEVNR-ALPHA = 'BQEKA'                                         
199600        MOVE '2356 ' TO W-IDLEVNR-ALPHA  END-IF                           
199700     IF W-IDLEVNR-ALPHA = 'BWKTA'                                         
199800        MOVE '3169 ' TO W-IDLEVNR-ALPHA  END-IF                           
199900     IF W-IDLEVNR-ALPHA = 'T2FBB'                                         
200000        MOVE '3803 ' TO W-IDLEVNR-ALPHA  END-IF                           
200100     IF W-IDLEVNR-ALPHA = 'D0DAA'                                         
200200        MOVE '3894 ' TO W-IDLEVNR-ALPHA  END-IF                           
200300     IF W-IDLEVNR-ALPHA = 'S17VA'                                         
200400        MOVE '3989 ' TO W-IDLEVNR-ALPHA  END-IF                           
200500     IF W-IDLEVNR-ALPHA = 'J3ZLA'                                         
200600        MOVE '4618 ' TO W-IDLEVNR-ALPHA  END-IF                           
200700     IF W-IDLEVNR-ALPHA = 'R8LDA'                                         
200800        MOVE '5212 ' TO W-IDLEVNR-ALPHA  END-IF                           
200900     IF W-IDLEVNR-ALPHA = 'V0QEA'                                         
201000        MOVE '5684 ' TO W-IDLEVNR-ALPHA  END-IF                           
201100     IF W-IDLEVNR-ALPHA = 'G769B'                                         
201200        MOVE '6063 ' TO W-IDLEVNR-ALPHA  END-IF                           
201300     IF W-IDLEVNR-ALPHA = 'C99ZA'                                         
201400        MOVE '6119 ' TO W-IDLEVNR-ALPHA  END-IF                           
201500     IF W-IDLEVNR-ALPHA = 'D1E4A'                                         
201600        MOVE '6166 ' TO W-IDLEVNR-ALPHA  END-IF                           
201700     IF W-IDLEVNR-ALPHA = 'D2H8A'                                         
201800        MOVE '6437 ' TO W-IDLEVNR-ALPHA  END-IF                           
201900     IF W-IDLEVNR-ALPHA = 'G952A'                                         
202000        MOVE '6515 ' TO W-IDLEVNR-ALPHA  END-IF                           
202100     IF W-IDLEVNR-ALPHA = 'Q520A'                                         
202200        MOVE '6522 ' TO W-IDLEVNR-ALPHA  END-IF                           
202300     IF W-IDLEVNR-ALPHA = 'EFR5A'                                         
202400        MOVE '6719 ' TO W-IDLEVNR-ALPHA  END-IF                           
202500     IF W-IDLEVNR-ALPHA = 'L905D'                                         
202600        MOVE '6771 ' TO W-IDLEVNR-ALPHA  END-IF                           
202700     IF W-IDLEVNR-ALPHA = 'BQ7RA'                                         
202800        MOVE '6828 ' TO W-IDLEVNR-ALPHA  END-IF                           
202900     IF W-IDLEVNR-ALPHA = 'U5P7A'                                         
203000        MOVE '6836 ' TO W-IDLEVNR-ALPHA  END-IF                           
203100     IF W-IDLEVNR-ALPHA = 'U1LFD'                                         
203200        MOVE '6857 ' TO W-IDLEVNR-ALPHA  END-IF                           
203300     IF W-IDLEVNR-ALPHA = 'ABYHA'                                         
203400        MOVE '6950 ' TO W-IDLEVNR-ALPHA  END-IF                           
203500     IF W-IDLEVNR-ALPHA = 'BPXDA'                                         
203600        MOVE '7040 ' TO W-IDLEVNR-ALPHA  END-IF                           
203700     IF W-IDLEVNR-ALPHA = 'T4UVA'                                         
203800        MOVE '7258 ' TO W-IDLEVNR-ALPHA  END-IF                           
203900     IF W-IDLEVNR-ALPHA = 'J5VHA'                                         
204000        MOVE '7919 ' TO W-IDLEVNR-ALPHA  END-IF                           
204100     IF W-IDLEVNR-ALPHA = 'N5LQA'                                         
204200        MOVE '7927 ' TO W-IDLEVNR-ALPHA  END-IF                           
204300     IF W-IDLEVNR-ALPHA = 'BQJDD'                                         
204400        MOVE '7954 ' TO W-IDLEVNR-ALPHA  END-IF                           
204500     IF W-IDLEVNR-ALPHA = 'BKAMB'                                         
204600        MOVE '10056' TO W-IDLEVNR-ALPHA  END-IF                           
204700     IF W-IDLEVNR-ALPHA = 'BPEVA'                                         
204800        MOVE '11369' TO W-IDLEVNR-ALPHA  END-IF                           
204900     IF W-IDLEVNR-ALPHA = 'T3DVA'                                         
205000        MOVE '13619' TO W-IDLEVNR-ALPHA  END-IF                           
205100     IF W-IDLEVNR-ALPHA = 'D797B'                                         
205200        MOVE '16096' TO W-IDLEVNR-ALPHA  END-IF                           
205300     IF W-IDLEVNR-ALPHA = 'ALTXA'                                         
205400        MOVE '16238' TO W-IDLEVNR-ALPHA  END-IF                           
205500     IF W-IDLEVNR-ALPHA = 'B45JC'                                         
205600        MOVE '16275' TO W-IDLEVNR-ALPHA  END-IF                           
205700     IF W-IDLEVNR-ALPHA = 'DP5UA'                                         
205800        MOVE '16301' TO W-IDLEVNR-ALPHA  END-IF                           
205900     IF W-IDLEVNR-ALPHA = 'BH0ZA'                                         
206000        MOVE '17253' TO W-IDLEVNR-ALPHA  END-IF                           
206100     IF W-IDLEVNR-ALPHA = 'H82NE'                                         
206200        MOVE '17825' TO W-IDLEVNR-ALPHA  END-IF                           
206300     IF W-IDLEVNR-ALPHA = 'H137P'                                         
206400        MOVE '17826' TO W-IDLEVNR-ALPHA  END-IF                           
206500     IF W-IDLEVNR-ALPHA = 'N508A'                                         
206600        MOVE '18127' TO W-IDLEVNR-ALPHA  END-IF                           
206700     IF W-IDLEVNR-ALPHA = 'AMMCB'                                         
206800        MOVE '19959' TO W-IDLEVNR-ALPHA  END-IF                           
206900     IF W-IDLEVNR-ALPHA = 'CF9JA'                                         
207000        MOVE '23271' TO W-IDLEVNR-ALPHA  END-IF                           
207100     IF W-IDLEVNR-ALPHA = 'CZQ7A'                                         
207200        MOVE '23914' TO W-IDLEVNR-ALPHA  END-IF                           
207300     IF W-IDLEVNR-ALPHA = 'H5PLA'                                         
207400        MOVE '23947' TO W-IDLEVNR-ALPHA  END-IF                           
207500     IF W-IDLEVNR-ALPHA = 'CU8WB'                                         
207600        MOVE '24374' TO W-IDLEVNR-ALPHA  END-IF                           
207700     IF W-IDLEVNR-ALPHA = 'AYSPA'                                         
207800        MOVE '24630' TO W-IDLEVNR-ALPHA  END-IF                           
207900     IF W-IDLEVNR-ALPHA = 'BHD8A'                                         
208000        MOVE '25763' TO W-IDLEVNR-ALPHA  END-IF                           
208100     IF W-IDLEVNR-ALPHA = 'DLKCA'                                         
208200        MOVE '1525 ' TO W-IDLEVNR-ALPHA  END-IF                           
208300     IF W-IDLEVNR-ALPHA = 'A76VA'                                         
208400        MOVE '1592 ' TO W-IDLEVNR-ALPHA  END-IF                           
208500     IF W-IDLEVNR-ALPHA = 'D33HE'                                         
208600        MOVE '3963 ' TO W-IDLEVNR-ALPHA  END-IF                           
208700     IF W-IDLEVNR-ALPHA = 'C69HA'                                         
208800        MOVE '6386 ' TO W-IDLEVNR-ALPHA  END-IF                           
208900     IF W-IDLEVNR-ALPHA = 'D0MNA'                                         
209000        MOVE '6808 ' TO W-IDLEVNR-ALPHA  END-IF                           
209100     IF W-IDLEVNR-ALPHA = 'C97RA'                                         
209200        MOVE '6914 ' TO W-IDLEVNR-ALPHA  END-IF                           
209300     IF W-IDLEVNR-ALPHA = 'BK2EA'                                         
209400        MOVE '8186 ' TO W-IDLEVNR-ALPHA  END-IF                           
209500     IF W-IDLEVNR-ALPHA = 'BQ8SA'                                         
209600        MOVE '10221' TO W-IDLEVNR-ALPHA  END-IF                           
209700     IF W-IDLEVNR-ALPHA = 'BUWJA'                                         
209800        MOVE '10511' TO W-IDLEVNR-ALPHA  END-IF                           
209900     IF W-IDLEVNR-ALPHA = 'BEJZA'                                         
210000        MOVE '10659' TO W-IDLEVNR-ALPHA  END-IF                           
210100     IF W-IDLEVNR-ALPHA = 'BNSRA'                                         
210200        MOVE '10813' TO W-IDLEVNR-ALPHA  END-IF                           
210300     IF W-IDLEVNR-ALPHA = 'D0BEB'                                         
210400        MOVE '11332' TO W-IDLEVNR-ALPHA  END-IF                           
210500     IF W-IDLEVNR-ALPHA = 'DL7SA'                                         
210600        MOVE '13484' TO W-IDLEVNR-ALPHA  END-IF                           
210700     IF W-IDLEVNR-ALPHA = 'D0MNE'                                         
210800        MOVE '13508' TO W-IDLEVNR-ALPHA  END-IF                           
210900     IF W-IDLEVNR-ALPHA = 'N0KFA'                                         
211000        MOVE '14465' TO W-IDLEVNR-ALPHA  END-IF                           
211100     IF W-IDLEVNR-ALPHA = 'MWZFA'                                         
211200        MOVE '14658' TO W-IDLEVNR-ALPHA  END-IF                           
211300     IF W-IDLEVNR-ALPHA = 'AYG1A'                                         
211400        MOVE '14756' TO W-IDLEVNR-ALPHA  END-IF                           
211500     IF W-IDLEVNR-ALPHA = 'BQ9RA'                                         
211600        MOVE '15053' TO W-IDLEVNR-ALPHA  END-IF                           
211700     IF W-IDLEVNR-ALPHA = 'D0MND'                                         
211800        MOVE '16049' TO W-IDLEVNR-ALPHA  END-IF                           
211900     IF W-IDLEVNR-ALPHA = 'D0MNF'                                         
212000        MOVE '16075' TO W-IDLEVNR-ALPHA  END-IF                           
212100     IF W-IDLEVNR-ALPHA = 'D33HA'                                         
212200        MOVE '17789' TO W-IDLEVNR-ALPHA  END-IF                           
212300     IF W-IDLEVNR-ALPHA = 'D0MNG'                                         
212400        MOVE '17945' TO W-IDLEVNR-ALPHA  END-IF                           
212500     IF W-IDLEVNR-ALPHA = 'S4HWB'                                         
212600        MOVE '19739' TO W-IDLEVNR-ALPHA  END-IF                           
212700     IF W-IDLEVNR-ALPHA = 'AJ2AA'                                         
212800        MOVE '21004' TO W-IDLEVNR-ALPHA  END-IF                           
212900     IF W-IDLEVNR-ALPHA = 'C8Q8A'                                         
213000        MOVE '22389' TO W-IDLEVNR-ALPHA  END-IF                           
213100     IF W-IDLEVNR-ALPHA = 'C94MA'                                         
213200        MOVE '23335' TO W-IDLEVNR-ALPHA  END-IF                           
213300     IF W-IDLEVNR-ALPHA = 'F477B'                                         
213400        MOVE '23862' TO W-IDLEVNR-ALPHA  END-IF                           
213500     IF W-IDLEVNR-ALPHA = 'P8TWA'                                         
213600        MOVE '25916' TO W-IDLEVNR-ALPHA  END-IF                           
213700     IF W-IDLEVNR-ALPHA = 'DNK9A'                                         
213800        MOVE '25931' TO W-IDLEVNR-ALPHA  END-IF                           
213900*************************************************                         
214000*************************************************                         
214100     IF W-IDLEVNR-ALPHA = 'BUPDC'                                         
214200        MOVE '31138' TO W-IDLEVNR-ALPHA  END-IF                           
214300     IF W-IDLEVNR-ALPHA = 'BUPDD'                                         
214400        MOVE '41138' TO W-IDLEVNR-ALPHA  END-IF                           
214500     IF W-IDLEVNR-ALPHA = 'L8K5H'                                         
214600        MOVE '11138' TO W-IDLEVNR-ALPHA  END-IF                           
214700*************************************************                         
214800     IF W-IDLEVNR-ALPHA = 'Q99AA'                                         
214900        MOVE '14483' TO W-IDLEVNR-ALPHA  END-IF                           
215000     IF W-IDLEVNR-ALPHA = 'D38KF'                                         
215100        MOVE '16044' TO W-IDLEVNR-ALPHA  END-IF                           
215200     IF W-IDLEVNR-ALPHA = 'S3MZA'                                         
215300        MOVE '16090' TO W-IDLEVNR-ALPHA  END-IF                           
215400     IF W-IDLEVNR-ALPHA = 'C95YC'                                         
215500        MOVE '16284' TO W-IDLEVNR-ALPHA  END-IF                           
215600     IF W-IDLEVNR-ALPHA = 'C95YB'                                         
215700        MOVE '16378' TO W-IDLEVNR-ALPHA  END-IF                           
215800     IF W-IDLEVNR-ALPHA = 'C95YJ'                                         
215900        MOVE '20501' TO W-IDLEVNR-ALPHA  END-IF                           
216000     IF W-IDLEVNR-ALPHA = 'D02KA'                                         
216100        MOVE '26942' TO W-IDLEVNR-ALPHA  END-IF                           
216200     IF W-IDLEVNR-ALPHA = 'D0UCA'                                         
216300        MOVE '3742 ' TO W-IDLEVNR-ALPHA  END-IF                           
216400     IF W-IDLEVNR-ALPHA = 'B4V0A'                                         
216500        MOVE '6685 ' TO W-IDLEVNR-ALPHA  END-IF                           
216600     IF W-IDLEVNR-ALPHA = 'C95YA'                                         
216700        MOVE '6942 ' TO W-IDLEVNR-ALPHA  END-IF                           
216800     IF W-IDLEVNR-ALPHA = 'S63MA'                                         
216900        MOVE '7683 ' TO W-IDLEVNR-ALPHA  END-IF                           
217000     IF W-IDLEVNR-ALPHA = 'BPFNB'                                         
217100        MOVE '34666' TO W-IDLEVNR-ALPHA  END-IF                           
217200     IF W-IDLEVNR-ALPHA = 'CDCHA'                                         
217300        MOVE '10030' TO W-IDLEVNR-ALPHA  END-IF                           
217400     IF W-IDLEVNR-ALPHA = 'L8K5W'                                         
217500        MOVE '10181' TO W-IDLEVNR-ALPHA  END-IF                           
217600     IF W-IDLEVNR-ALPHA = 'CDV5A'                                         
217700        MOVE '10347' TO W-IDLEVNR-ALPHA  END-IF                           
217800     IF W-IDLEVNR-ALPHA = 'S4MZD'                                         
217900        MOVE '11518' TO W-IDLEVNR-ALPHA  END-IF                           
218000     IF W-IDLEVNR-ALPHA = 'S1XMC'                                         
218100        MOVE '11753' TO W-IDLEVNR-ALPHA  END-IF                           
218200     IF W-IDLEVNR-ALPHA = 'ALRHA'                                         
218300        MOVE '12539' TO W-IDLEVNR-ALPHA  END-IF                           
218400     IF W-IDLEVNR-ALPHA = 'BQ9FA'                                         
218500        MOVE '13444' TO W-IDLEVNR-ALPHA  END-IF                           
218600     IF W-IDLEVNR-ALPHA = 'S1XMD'                                         
218700        MOVE '13543' TO W-IDLEVNR-ALPHA  END-IF                           
218800     IF W-IDLEVNR-ALPHA = 'BUPDA'                                         
218900        MOVE '13635' TO W-IDLEVNR-ALPHA  END-IF                           
219000     IF W-IDLEVNR-ALPHA = 'BQ9PA'                                         
219100        MOVE '14599' TO W-IDLEVNR-ALPHA  END-IF                           
219200     IF W-IDLEVNR-ALPHA = 'P9J4A'                                         
219300        MOVE '14924' TO W-IDLEVNR-ALPHA  END-IF                           
219400     IF W-IDLEVNR-ALPHA = 'D11JA'                                         
219500        MOVE '15442' TO W-IDLEVNR-ALPHA  END-IF                           
219600     IF W-IDLEVNR-ALPHA = 'Q42PA'                                         
219700        MOVE '16062' TO W-IDLEVNR-ALPHA  END-IF                           
219800     IF W-IDLEVNR-ALPHA = 'B4X4A'                                         
219900        MOVE '16071' TO W-IDLEVNR-ALPHA  END-IF                           
220000     IF W-IDLEVNR-ALPHA = 'BPGQA'                                         
220100        MOVE '171  ' TO W-IDLEVNR-ALPHA  END-IF                           
220200     IF W-IDLEVNR-ALPHA = 'DJPSA'                                         
220300        MOVE '17255' TO W-IDLEVNR-ALPHA  END-IF                           
220400     IF W-IDLEVNR-ALPHA = 'AZYXA'                                         
220500        MOVE '18609' TO W-IDLEVNR-ALPHA  END-IF                           
220600     IF W-IDLEVNR-ALPHA = 'R0PRA'                                         
220700        MOVE '19733' TO W-IDLEVNR-ALPHA  END-IF                           
220800     IF W-IDLEVNR-ALPHA = 'BPGQD'                                         
220900        MOVE '2036 ' TO W-IDLEVNR-ALPHA  END-IF                           
221000     IF W-IDLEVNR-ALPHA = 'N2D2E'                                         
221100        MOVE '21756' TO W-IDLEVNR-ALPHA  END-IF                           
221200     IF W-IDLEVNR-ALPHA = 'CTYHA'                                         
221300        MOVE '22396' TO W-IDLEVNR-ALPHA  END-IF                           
221400     IF W-IDLEVNR-ALPHA = 'N81QA'                                         
221500        MOVE '230  ' TO W-IDLEVNR-ALPHA  END-IF                           
221600     IF W-IDLEVNR-ALPHA = 'BJHYA'                                         
221700        MOVE '23245' TO W-IDLEVNR-ALPHA  END-IF                           
221800     IF W-IDLEVNR-ALPHA = 'BQEKA'                                         
221900        MOVE '2356 ' TO W-IDLEVNR-ALPHA  END-IF                           
222000     IF W-IDLEVNR-ALPHA = 'BRTUA'                                         
222100        MOVE '23705' TO W-IDLEVNR-ALPHA  END-IF                           
222200     IF W-IDLEVNR-ALPHA = 'CLDQA'                                         
222300        MOVE '23799' TO W-IDLEVNR-ALPHA  END-IF                           
222400     IF W-IDLEVNR-ALPHA = 'BUPDA'                                         
222500        MOVE '24010' TO W-IDLEVNR-ALPHA  END-IF                           
222600     IF W-IDLEVNR-ALPHA = 'CLMPA'                                         
222700        MOVE '25669' TO W-IDLEVNR-ALPHA  END-IF                           
222800     IF W-IDLEVNR-ALPHA = 'BQ9PB'                                         
222900        MOVE '25691' TO W-IDLEVNR-ALPHA  END-IF                           
223000     IF W-IDLEVNR-ALPHA = 'DA5NA'                                         
223100        MOVE '25764' TO W-IDLEVNR-ALPHA  END-IF                           
223200     IF W-IDLEVNR-ALPHA = 'AQHKA'                                         
223300        MOVE '25874' TO W-IDLEVNR-ALPHA  END-IF                           
223400     IF W-IDLEVNR-ALPHA = 'L8K50'                                         
223500        MOVE '25890' TO W-IDLEVNR-ALPHA  END-IF                           
223600     IF W-IDLEVNR-ALPHA = 'L8K5Z'                                         
223700        MOVE '25891' TO W-IDLEVNR-ALPHA  END-IF                           
223800     IF W-IDLEVNR-ALPHA = 'L8K5Y'                                         
223900        MOVE '25892' TO W-IDLEVNR-ALPHA  END-IF                           
224000     IF W-IDLEVNR-ALPHA = 'L8K5X'                                         
224100        MOVE '25893' TO W-IDLEVNR-ALPHA  END-IF                           
224200     IF W-IDLEVNR-ALPHA = 'DNH1A'                                         
224300        MOVE '25930' TO W-IDLEVNR-ALPHA  END-IF                           
224400     IF W-IDLEVNR-ALPHA = 'CQPRA'                                         
224500        MOVE '2662 ' TO W-IDLEVNR-ALPHA  END-IF                           
224600     IF W-IDLEVNR-ALPHA = 'D0N8A'                                         
224700        MOVE '3350 ' TO W-IDLEVNR-ALPHA  END-IF                           
224800     IF W-IDLEVNR-ALPHA = 'S1XMB'                                         
224900        MOVE '3656 ' TO W-IDLEVNR-ALPHA  END-IF                           
225000     IF W-IDLEVNR-ALPHA = 'S1XMA'                                         
225100        MOVE '3964 ' TO W-IDLEVNR-ALPHA  END-IF                           
225200     IF W-IDLEVNR-ALPHA = 'BPGQB'                                         
225300        MOVE '402  ' TO W-IDLEVNR-ALPHA  END-IF                           
225400     IF W-IDLEVNR-ALPHA = 'S41GF'                                         
225500        MOVE '4234 ' TO W-IDLEVNR-ALPHA  END-IF                           
225600     IF W-IDLEVNR-ALPHA = 'C62JA'                                         
225700        MOVE '5191 ' TO W-IDLEVNR-ALPHA  END-IF                           
225800     IF W-IDLEVNR-ALPHA = 'C9S2A'                                         
225900        MOVE '5670 ' TO W-IDLEVNR-ALPHA  END-IF                           
226000     IF W-IDLEVNR-ALPHA = 'B050B'                                         
226100        MOVE '5674 ' TO W-IDLEVNR-ALPHA  END-IF                           
226200     IF W-IDLEVNR-ALPHA = 'AZYXB'                                         
226300        MOVE '6229 ' TO W-IDLEVNR-ALPHA  END-IF                           
226400     IF W-IDLEVNR-ALPHA = 'CFUDB'                                         
226500        MOVE '6268 ' TO W-IDLEVNR-ALPHA  END-IF                           
226600     IF W-IDLEVNR-ALPHA = 'D3P7B'                                         
226700        MOVE '6282 ' TO W-IDLEVNR-ALPHA  END-IF                           
226800     IF W-IDLEVNR-ALPHA = 'C5408'                                         
226900        MOVE '63058' TO W-IDLEVNR-ALPHA  END-IF                           
227000     IF W-IDLEVNR-ALPHA = 'D1K4A'                                         
227100        MOVE '6598 ' TO W-IDLEVNR-ALPHA  END-IF                           
227200     IF W-IDLEVNR-ALPHA = 'R151A'                                         
227300        MOVE '6666 ' TO W-IDLEVNR-ALPHA  END-IF                           
227400     IF W-IDLEVNR-ALPHA = 'S4MZA'                                         
227500        MOVE '6678 ' TO W-IDLEVNR-ALPHA  END-IF                           
227600     IF W-IDLEVNR-ALPHA = 'ENB6A'                                         
227700        MOVE '6809 ' TO W-IDLEVNR-ALPHA  END-IF                           
227800     IF W-IDLEVNR-ALPHA = 'ABYHA'                                         
227900        MOVE '6950 ' TO W-IDLEVNR-ALPHA  END-IF                           
228000     IF W-IDLEVNR-ALPHA = 'C86HA'                                         
228100        MOVE '6972 ' TO W-IDLEVNR-ALPHA  END-IF                           
228200     IF W-IDLEVNR-ALPHA = 'N2D2A'                                         
228300        MOVE '7847 ' TO W-IDLEVNR-ALPHA  END-IF                           
228400     IF W-IDLEVNR-ALPHA = 'D0GWA'                                         
228500        MOVE '7915 ' TO W-IDLEVNR-ALPHA  END-IF                           
228600     IF W-IDLEVNR-ALPHA = 'BP3JB'                                         
228700        MOVE '843  ' TO W-IDLEVNR-ALPHA  END-IF                           
228800     IF W-IDLEVNR-ALPHA = 'C9P1B'                                         
228900        MOVE '10490' TO W-IDLEVNR-ALPHA  END-IF                           
229000     IF W-IDLEVNR-ALPHA = 'CTX0A'                                         
229100        MOVE '11062' TO W-IDLEVNR-ALPHA  END-IF                           
229200     IF W-IDLEVNR-ALPHA = 'BP8AA'                                         
229300        MOVE '11086' TO W-IDLEVNR-ALPHA  END-IF                           
229400     IF W-IDLEVNR-ALPHA = 'BUWLA'                                         
229500        MOVE '11403' TO W-IDLEVNR-ALPHA  END-IF                           
229600     IF W-IDLEVNR-ALPHA = 'BQH8A'                                         
229700        MOVE '1148 ' TO W-IDLEVNR-ALPHA  END-IF                           
229800     IF W-IDLEVNR-ALPHA = 'CN5NA'                                         
229900        MOVE '11708' TO W-IDLEVNR-ALPHA  END-IF                           
230000     IF W-IDLEVNR-ALPHA = 'BJQNA'                                         
230100        MOVE '12055' TO W-IDLEVNR-ALPHA  END-IF                           
230200     IF W-IDLEVNR-ALPHA = 'Q98ZA'                                         
230300        MOVE '13241' TO W-IDLEVNR-ALPHA  END-IF                           
230400     IF W-IDLEVNR-ALPHA = 'CFUBA'                                         
230500        MOVE '13517' TO W-IDLEVNR-ALPHA  END-IF                           
230600     IF W-IDLEVNR-ALPHA = 'BJPMA'                                         
230700        MOVE '13527' TO W-IDLEVNR-ALPHA  END-IF                           
230800     IF W-IDLEVNR-ALPHA = 'BJPMB'                                         
230900        MOVE '13603' TO W-IDLEVNR-ALPHA  END-IF                           
231000     IF W-IDLEVNR-ALPHA = 'BC3EA'                                         
231100        MOVE '14668' TO W-IDLEVNR-ALPHA  END-IF                           
231200     IF W-IDLEVNR-ALPHA = 'BQ2ZA'                                         
231300        MOVE '1480 ' TO W-IDLEVNR-ALPHA  END-IF                           
231400     IF W-IDLEVNR-ALPHA = 'BQ8DA'                                         
231500        MOVE '1528 ' TO W-IDLEVNR-ALPHA  END-IF                           
231600     IF W-IDLEVNR-ALPHA = 'R9Q4A'                                         
231700        MOVE '16081' TO W-IDLEVNR-ALPHA  END-IF                           
231800     IF W-IDLEVNR-ALPHA = 'J17KA'                                         
231900        MOVE '16358' TO W-IDLEVNR-ALPHA  END-IF                           
232000     IF W-IDLEVNR-ALPHA = 'R3U8A'                                         
232100        MOVE '16371' TO W-IDLEVNR-ALPHA  END-IF                           
232200     IF W-IDLEVNR-ALPHA = 'D0N0E'                                         
232300        MOVE '16735' TO W-IDLEVNR-ALPHA  END-IF                           
232400     IF W-IDLEVNR-ALPHA = 'BQYKB'                                         
232500        MOVE '1689 ' TO W-IDLEVNR-ALPHA  END-IF                           
232600     IF W-IDLEVNR-ALPHA = 'BJPMD'                                         
232700        MOVE '21327' TO W-IDLEVNR-ALPHA  END-IF                           
232800     IF W-IDLEVNR-ALPHA = 'N7WWA'                                         
232900        MOVE '214  ' TO W-IDLEVNR-ALPHA  END-IF                           
233000     IF W-IDLEVNR-ALPHA = 'BPF3A'                                         
233100        MOVE '2213 ' TO W-IDLEVNR-ALPHA  END-IF                           
233200     IF W-IDLEVNR-ALPHA = 'BQ5QA'                                         
233300        MOVE '2222 ' TO W-IDLEVNR-ALPHA  END-IF                           
233400     IF W-IDLEVNR-ALPHA = 'BLNMA'                                         
233500        MOVE '2227 ' TO W-IDLEVNR-ALPHA  END-IF                           
233600     IF W-IDLEVNR-ALPHA = 'H7G6A'                                         
233700        MOVE '2312 ' TO W-IDLEVNR-ALPHA  END-IF                           
233800     IF W-IDLEVNR-ALPHA = 'AXQ1A'                                         
233900        MOVE '24660' TO W-IDLEVNR-ALPHA  END-IF                           
234000     IF W-IDLEVNR-ALPHA = 'AQYSA'                                         
234100        MOVE '25759' TO W-IDLEVNR-ALPHA  END-IF                           
234200     IF W-IDLEVNR-ALPHA = 'BQ6LA'                                         
234300        MOVE '25775' TO W-IDLEVNR-ALPHA  END-IF                           
234400     IF W-IDLEVNR-ALPHA = 'DDCLA'                                         
234500        MOVE '25815' TO W-IDLEVNR-ALPHA  END-IF                           
234600     IF W-IDLEVNR-ALPHA = 'CW0XA'                                         
234700        MOVE '26031' TO W-IDLEVNR-ALPHA  END-IF                           
234800     IF W-IDLEVNR-ALPHA = 'BQ6FA'                                         
234900        MOVE '2634 ' TO W-IDLEVNR-ALPHA  END-IF                           
235000     IF W-IDLEVNR-ALPHA = 'BQYLA'                                         
235100        MOVE '266  ' TO W-IDLEVNR-ALPHA  END-IF                           
235200     IF W-IDLEVNR-ALPHA = 'S9B0B'                                         
235300        MOVE '2670 ' TO W-IDLEVNR-ALPHA  END-IF                           
235400     IF W-IDLEVNR-ALPHA = 'BQ6LA'                                         
235500        MOVE '3043 ' TO W-IDLEVNR-ALPHA  END-IF                           
235600     IF W-IDLEVNR-ALPHA = 'CL3YA'                                         
235700        MOVE '3054 ' TO W-IDLEVNR-ALPHA  END-IF                           
235800     IF W-IDLEVNR-ALPHA = 'BKNKA'                                         
235900        MOVE '3096 ' TO W-IDLEVNR-ALPHA  END-IF                           
236000     IF W-IDLEVNR-ALPHA = 'B34SG'                                         
236100        MOVE '3653 ' TO W-IDLEVNR-ALPHA  END-IF                           
236200     IF W-IDLEVNR-ALPHA = 'AKUMA'                                         
236300        MOVE '3666 ' TO W-IDLEVNR-ALPHA  END-IF                           
236400     IF W-IDLEVNR-ALPHA = 'BQ6TA'                                         
236500        MOVE '3832 ' TO W-IDLEVNR-ALPHA  END-IF                           
236600     IF W-IDLEVNR-ALPHA = 'BP3CA'                                         
236700        MOVE '393  ' TO W-IDLEVNR-ALPHA  END-IF                           
236800     IF W-IDLEVNR-ALPHA = 'BQ6UA'                                         
236900        MOVE '3934 ' TO W-IDLEVNR-ALPHA  END-IF                           
237000     IF W-IDLEVNR-ALPHA = 'Q3MXA'                                         
237100        MOVE '4235 ' TO W-IDLEVNR-ALPHA  END-IF                           
237200     IF W-IDLEVNR-ALPHA = 'A7G7A'                                         
237300        MOVE '4253 ' TO W-IDLEVNR-ALPHA  END-IF                           
237400     IF W-IDLEVNR-ALPHA = 'S9B0A'                                         
237500        MOVE '4597 ' TO W-IDLEVNR-ALPHA  END-IF                           
237600     IF W-IDLEVNR-ALPHA = 'E7R9D'                                         
237700        MOVE '4985 ' TO W-IDLEVNR-ALPHA  END-IF                           
237800     IF W-IDLEVNR-ALPHA = 'BJ6BA'                                         
237900        MOVE '507  ' TO W-IDLEVNR-ALPHA  END-IF                           
238000     IF W-IDLEVNR-ALPHA = 'CN4SA'                                         
238100        MOVE '510  ' TO W-IDLEVNR-ALPHA  END-IF                           
238200     IF W-IDLEVNR-ALPHA = 'E9VKB'                                         
238300        MOVE '5177 ' TO W-IDLEVNR-ALPHA  END-IF                           
238400     IF W-IDLEVNR-ALPHA = 'BLHTA'                                         
238500        MOVE '521  ' TO W-IDLEVNR-ALPHA  END-IF                           
238600     IF W-IDLEVNR-ALPHA = 'F610B'                                         
238700        MOVE '5269 ' TO W-IDLEVNR-ALPHA  END-IF                           
238800     IF W-IDLEVNR-ALPHA = 'C8D2A'                                         
238900        MOVE '6057 ' TO W-IDLEVNR-ALPHA  END-IF                           
239000     IF W-IDLEVNR-ALPHA = 'LBEKA'                                         
239100        MOVE '6154 ' TO W-IDLEVNR-ALPHA  END-IF                           
239200     IF W-IDLEVNR-ALPHA = 'V022A'                                         
239300        MOVE '6175 ' TO W-IDLEVNR-ALPHA  END-IF                           
239400     IF W-IDLEVNR-ALPHA = 'ADSMA'                                         
239500        MOVE '6199 ' TO W-IDLEVNR-ALPHA  END-IF                           
239600     IF W-IDLEVNR-ALPHA = 'D04BB'                                         
239700        MOVE '6244 ' TO W-IDLEVNR-ALPHA  END-IF                           
239800     IF W-IDLEVNR-ALPHA = 'S2Z6B'                                         
239900        MOVE '6330 ' TO W-IDLEVNR-ALPHA  END-IF                           
240000     IF W-IDLEVNR-ALPHA = 'B40XA'                                         
240100        MOVE '6474 ' TO W-IDLEVNR-ALPHA  END-IF                           
240200     IF W-IDLEVNR-ALPHA = 'BQ7QA'                                         
240300        MOVE '6649 ' TO W-IDLEVNR-ALPHA  END-IF                           
240400     IF W-IDLEVNR-ALPHA = 'D0N0A'                                         
240500        MOVE '6849 ' TO W-IDLEVNR-ALPHA  END-IF                           
240600     IF W-IDLEVNR-ALPHA = 'B34SA'                                         
240700        MOVE '7785 ' TO W-IDLEVNR-ALPHA  END-IF                           
240800     IF W-IDLEVNR-ALPHA = 'BUTZA'                                         
240900        MOVE '7958 ' TO W-IDLEVNR-ALPHA  END-IF                           
241000     IF W-IDLEVNR-ALPHA = 'BQYKA'                                         
241100        MOVE '88   ' TO W-IDLEVNR-ALPHA  END-IF                           
241200     IF W-IDLEVNR-ALPHA = 'BKNQA'                                         
241300        MOVE '8888 ' TO W-IDLEVNR-ALPHA  END-IF                           
241400     IF W-IDLEVNR-ALPHA = 'BQ0GA'                                         
241500        MOVE '996  ' TO W-IDLEVNR-ALPHA  END-IF                           
241600*************************************************                         
241700     IF W-IDLEVNR-ALPHA = 'CFNFA'                                         
241800        MOVE '848  ' TO W-IDLEVNR-ALPHA  END-IF                           
241900     IF W-IDLEVNR-ALPHA = 'BX3PA'                                         
242000        MOVE '883  ' TO W-IDLEVNR-ALPHA  END-IF                           
242100     IF W-IDLEVNR-ALPHA = 'BJQRA'                                         
242200        MOVE '1203 ' TO W-IDLEVNR-ALPHA  END-IF                           
242300     IF W-IDLEVNR-ALPHA = 'BQ5SA'                                         
242400        MOVE '2346 ' TO W-IDLEVNR-ALPHA  END-IF                           
242500     IF W-IDLEVNR-ALPHA = 'BQEBA'                                         
242600        MOVE '2363 ' TO W-IDLEVNR-ALPHA  END-IF                           
242700     IF W-IDLEVNR-ALPHA = 'BQ5TA'                                         
242800        MOVE '2368 ' TO W-IDLEVNR-ALPHA  END-IF                           
242900     IF W-IDLEVNR-ALPHA = 'BP3EB'                                         
243000        MOVE '2419 ' TO W-IDLEVNR-ALPHA  END-IF                           
243100     IF W-IDLEVNR-ALPHA = 'AY2QA'                                         
243200        MOVE '3036 ' TO W-IDLEVNR-ALPHA  END-IF                           
243300     IF W-IDLEVNR-ALPHA = 'K3D7A'                                         
243400        MOVE '3712 ' TO W-IDLEVNR-ALPHA  END-IF                           
243500     IF W-IDLEVNR-ALPHA = 'X448A'                                         
243600        MOVE '5249 ' TO W-IDLEVNR-ALPHA  END-IF                           
243700     IF W-IDLEVNR-ALPHA = 'D22FA'                                         
243800        MOVE '6040 ' TO W-IDLEVNR-ALPHA  END-IF                           
243900     IF W-IDLEVNR-ALPHA = 'KKM2A'                                         
244000        MOVE '6174 ' TO W-IDLEVNR-ALPHA  END-IF                           
244100     IF W-IDLEVNR-ALPHA = 'BCK8A'                                         
244200        MOVE '6385 ' TO W-IDLEVNR-ALPHA  END-IF                           
244300     IF W-IDLEVNR-ALPHA = 'T9UZA'                                         
244400        MOVE '6485 ' TO W-IDLEVNR-ALPHA  END-IF                           
244500     IF W-IDLEVNR-ALPHA = 'BQ7NA'                                         
244600        MOVE '6513 ' TO W-IDLEVNR-ALPHA  END-IF                           
244700     IF W-IDLEVNR-ALPHA = 'M279A'                                         
244800        MOVE '6600 ' TO W-IDLEVNR-ALPHA  END-IF                           
244900     IF W-IDLEVNR-ALPHA = 'KTY7A'                                         
245000        MOVE '6614 ' TO W-IDLEVNR-ALPHA  END-IF                           
245100     IF W-IDLEVNR-ALPHA = 'D21UA'                                         
245200        MOVE '6701 ' TO W-IDLEVNR-ALPHA  END-IF                           
245300     IF W-IDLEVNR-ALPHA = 'CFTZA'                                         
245400        MOVE '6952 ' TO W-IDLEVNR-ALPHA  END-IF                           
245500     IF W-IDLEVNR-ALPHA = 'D3T6A'                                         
245600        MOVE '7042 ' TO W-IDLEVNR-ALPHA  END-IF                           
245700     IF W-IDLEVNR-ALPHA = 'BQ7TA'                                         
245800        MOVE '7052 ' TO W-IDLEVNR-ALPHA  END-IF                           
245900     IF W-IDLEVNR-ALPHA = 'R742A'                                         
246000        MOVE '7179 ' TO W-IDLEVNR-ALPHA  END-IF                           
246100     IF W-IDLEVNR-ALPHA = 'CXA0A'                                         
246200        MOVE '7189 ' TO W-IDLEVNR-ALPHA  END-IF                           
246300     IF W-IDLEVNR-ALPHA = 'CXA1A'                                         
246400        MOVE '7190 ' TO W-IDLEVNR-ALPHA  END-IF                           
246500     IF W-IDLEVNR-ALPHA = 'Q725B'                                         
246600        MOVE '7246 ' TO W-IDLEVNR-ALPHA  END-IF                           
246700     IF W-IDLEVNR-ALPHA = 'S35LA'                                         
246800        MOVE '7253 ' TO W-IDLEVNR-ALPHA  END-IF                           
246900     IF W-IDLEVNR-ALPHA = 'G1UHS'                                         
247000        MOVE '7255 ' TO W-IDLEVNR-ALPHA  END-IF                           
247100     IF W-IDLEVNR-ALPHA = 'BP3KA'                                         
247200        MOVE '7470 ' TO W-IDLEVNR-ALPHA  END-IF                           
247300     IF W-IDLEVNR-ALPHA = 'CEFPA'                                         
247400        MOVE '7787 ' TO W-IDLEVNR-ALPHA  END-IF                           
247500     IF W-IDLEVNR-ALPHA = 'CDHVA'                                         
247600        MOVE '10024' TO W-IDLEVNR-ALPHA  END-IF                           
247700     IF W-IDLEVNR-ALPHA = 'BP3EA'                                         
247800        MOVE '10121' TO W-IDLEVNR-ALPHA  END-IF                           
247900     IF W-IDLEVNR-ALPHA = 'CXMTA'                                         
248000        MOVE '10290' TO W-IDLEVNR-ALPHA  END-IF                           
248100     IF W-IDLEVNR-ALPHA = 'BJQRD'                                         
248200        MOVE '10324' TO W-IDLEVNR-ALPHA  END-IF                           
248300     IF W-IDLEVNR-ALPHA = 'C7C5A'                                         
248400        MOVE '10510' TO W-IDLEVNR-ALPHA  END-IF                           
248500     IF W-IDLEVNR-ALPHA = 'BJQRB'                                         
248600        MOVE '10657' TO W-IDLEVNR-ALPHA  END-IF                           
248700     IF W-IDLEVNR-ALPHA = 'V4B1B'                                         
248800        MOVE '11100' TO W-IDLEVNR-ALPHA  END-IF                           
248900     IF W-IDLEVNR-ALPHA = 'G8VTA'                                         
249000        MOVE '11101' TO W-IDLEVNR-ALPHA  END-IF                           
249100     IF W-IDLEVNR-ALPHA = 'BCK8B'                                         
249200        MOVE '11137' TO W-IDLEVNR-ALPHA  END-IF                           
249300     IF W-IDLEVNR-ALPHA = 'BJQRC'                                         
249400        MOVE '11139' TO W-IDLEVNR-ALPHA  END-IF                           
249500     IF W-IDLEVNR-ALPHA = 'BQEBB'                                         
249600        MOVE '11155' TO W-IDLEVNR-ALPHA  END-IF                           
249700     IF W-IDLEVNR-ALPHA = 'E521H'                                         
249800        MOVE '11388' TO W-IDLEVNR-ALPHA  END-IF                           
249900     IF W-IDLEVNR-ALPHA = 'B491E'                                         
250000        MOVE '12637' TO W-IDLEVNR-ALPHA  END-IF                           
250100     IF W-IDLEVNR-ALPHA = 'T3WQA'                                         
250200        MOVE '13537' TO W-IDLEVNR-ALPHA  END-IF                           
250300     IF W-IDLEVNR-ALPHA = 'BPTRA'                                         
250400        MOVE '13550' TO W-IDLEVNR-ALPHA  END-IF                           
250500     IF W-IDLEVNR-ALPHA = 'BPK4A'                                         
250600        MOVE '13777' TO W-IDLEVNR-ALPHA  END-IF                           
250700     IF W-IDLEVNR-ALPHA = 'C79MA'                                         
250800        MOVE '14926' TO W-IDLEVNR-ALPHA  END-IF                           
250900     IF W-IDLEVNR-ALPHA = 'D5E1C'                                         
251000        MOVE '14927' TO W-IDLEVNR-ALPHA  END-IF                           
251100     IF W-IDLEVNR-ALPHA = 'C79ME'                                         
251200        MOVE '14941' TO W-IDLEVNR-ALPHA  END-IF                           
251300     IF W-IDLEVNR-ALPHA = 'T9UZB'                                         
251400        MOVE '16021' TO W-IDLEVNR-ALPHA  END-IF                           
251500     IF W-IDLEVNR-ALPHA = 'B40YA'                                         
251600        MOVE '16084' TO W-IDLEVNR-ALPHA  END-IF                           
251700     IF W-IDLEVNR-ALPHA = 'D38CA'                                         
251800        MOVE '16385' TO W-IDLEVNR-ALPHA  END-IF                           
251900     IF W-IDLEVNR-ALPHA = 'MKB2A'                                         
252000        MOVE '16386' TO W-IDLEVNR-ALPHA  END-IF                           
252100     IF W-IDLEVNR-ALPHA = 'G0MMA'                                         
252200        MOVE '17777' TO W-IDLEVNR-ALPHA  END-IF                           
252300     IF W-IDLEVNR-ALPHA = 'T43NA'                                         
252400        MOVE '17801' TO W-IDLEVNR-ALPHA  END-IF                           
252500     IF W-IDLEVNR-ALPHA = 'S5VSA'                                         
252600        MOVE '18038' TO W-IDLEVNR-ALPHA  END-IF                           
252700     IF W-IDLEVNR-ALPHA = 'CBBNA'                                         
252800        MOVE '18059' TO W-IDLEVNR-ALPHA  END-IF                           
252900     IF W-IDLEVNR-ALPHA = 'B45VD'                                         
253000        MOVE '19725' TO W-IDLEVNR-ALPHA  END-IF                           
253100     IF W-IDLEVNR-ALPHA = 'MKB2B'                                         
253200        MOVE '19995' TO W-IDLEVNR-ALPHA  END-IF                           
253300     IF W-IDLEVNR-ALPHA = 'Q9K3B'                                         
253400        MOVE '23511' TO W-IDLEVNR-ALPHA  END-IF                           
253500     IF W-IDLEVNR-ALPHA = 'S35LB'                                         
253600        MOVE '24248' TO W-IDLEVNR-ALPHA  END-IF                           
253700     IF W-IDLEVNR-ALPHA = 'R742B'                                         
253800        MOVE '24332' TO W-IDLEVNR-ALPHA  END-IF                           
253900     IF W-IDLEVNR-ALPHA = 'ND04W'                                         
254000        MOVE '24967' TO W-IDLEVNR-ALPHA  END-IF                           
254100     IF W-IDLEVNR-ALPHA = 'N8U4A'                                         
254200        MOVE '25784' TO W-IDLEVNR-ALPHA  END-IF                           
254300     IF W-IDLEVNR-ALPHA = 'BPTQC'                                         
254400        MOVE '25809' TO W-IDLEVNR-ALPHA  END-IF                           
254500     IF W-IDLEVNR-ALPHA = 'D08GJ'                                         
254600        MOVE '25889' TO W-IDLEVNR-ALPHA  END-IF                           
254700     IF W-IDLEVNR-ALPHA = 'S044X'                                         
254800        MOVE '25895' TO W-IDLEVNR-ALPHA  END-IF                           
254900     IF W-IDLEVNR-ALPHA = 'B46ZA'                                         
255000        MOVE '25919' TO W-IDLEVNR-ALPHA  END-IF                           
255100     IF W-IDLEVNR-ALPHA = 'KTY7D'                                         
255200        MOVE '25949' TO W-IDLEVNR-ALPHA  END-IF                           
255300*************************************************                         
255400     IF W-IDLEVNR-ALPHA = 'DRDGA'                                         
255500        MOVE '15   ' TO W-IDLEVNR-ALPHA  END-IF                           
255600     IF W-IDLEVNR-ALPHA = 'S69YA'                                         
255700        MOVE '19   ' TO W-IDLEVNR-ALPHA  END-IF                           
255800     IF W-IDLEVNR-ALPHA = 'BLJ6A'                                         
255900        MOVE '177  ' TO W-IDLEVNR-ALPHA  END-IF                           
256000     IF W-IDLEVNR-ALPHA = 'BQ6HA'                                         
256100        MOVE '527  ' TO W-IDLEVNR-ALPHA  END-IF                           
256200     IF W-IDLEVNR-ALPHA = 'AY0MA'                                         
256300        MOVE '745  ' TO W-IDLEVNR-ALPHA  END-IF                           
256400     IF W-IDLEVNR-ALPHA = 'BLZWA'                                         
256500        MOVE '870  ' TO W-IDLEVNR-ALPHA  END-IF                           
256600     IF W-IDLEVNR-ALPHA = 'BQ2BA'                                         
256700        MOVE '1049 ' TO W-IDLEVNR-ALPHA  END-IF                           
256800     IF W-IDLEVNR-ALPHA = 'CFNJA'                                         
256900        MOVE '1456 ' TO W-IDLEVNR-ALPHA  END-IF                           
257000     IF W-IDLEVNR-ALPHA = 'CN4VA'                                         
257100        MOVE '1787 ' TO W-IDLEVNR-ALPHA  END-IF                           
257200     IF W-IDLEVNR-ALPHA = 'BQ3XA'                                         
257300        MOVE '1865 ' TO W-IDLEVNR-ALPHA  END-IF                           
257400     IF W-IDLEVNR-ALPHA = 'CFTNA'                                         
257500        MOVE '2054 ' TO W-IDLEVNR-ALPHA  END-IF                           
257600     IF W-IDLEVNR-ALPHA = 'BX9LA'                                         
257700        MOVE '2103 ' TO W-IDLEVNR-ALPHA  END-IF                           
257800     IF W-IDLEVNR-ALPHA = 'S6R1A'                                         
257900        MOVE '2261 ' TO W-IDLEVNR-ALPHA  END-IF                           
258000     IF W-IDLEVNR-ALPHA = 'BJ6HA'                                         
258100        MOVE '2406 ' TO W-IDLEVNR-ALPHA  END-IF                           
258200     IF W-IDLEVNR-ALPHA = 'BQ5WA'                                         
258300        MOVE '2417 ' TO W-IDLEVNR-ALPHA  END-IF                           
258400     IF W-IDLEVNR-ALPHA = 'BQ5XA'                                         
258500        MOVE '2423 ' TO W-IDLEVNR-ALPHA  END-IF                           
258600     IF W-IDLEVNR-ALPHA = 'CFNUA'                                         
258700        MOVE '2457 ' TO W-IDLEVNR-ALPHA  END-IF                           
258800     IF W-IDLEVNR-ALPHA = 'BYL8A'                                         
258900        MOVE '2490 ' TO W-IDLEVNR-ALPHA  END-IF                           
259000     IF W-IDLEVNR-ALPHA = 'DL1WA'                                         
259100        MOVE '3404 ' TO W-IDLEVNR-ALPHA  END-IF                           
259200     IF W-IDLEVNR-ALPHA = 'K4STA'                                         
259300        MOVE '3572 ' TO W-IDLEVNR-ALPHA  END-IF                           
259400     IF W-IDLEVNR-ALPHA = 'A0VWA'                                         
259500        MOVE '3747 ' TO W-IDLEVNR-ALPHA  END-IF                           
259600     IF W-IDLEVNR-ALPHA = 'R39QA'                                         
259700        MOVE '3815 ' TO W-IDLEVNR-ALPHA  END-IF                           
259800     IF W-IDLEVNR-ALPHA = 'DLJLA'                                         
259900        MOVE '3898 ' TO W-IDLEVNR-ALPHA  END-IF                           
260000     IF W-IDLEVNR-ALPHA = 'BWMAA'                                         
260100        MOVE '3902 ' TO W-IDLEVNR-ALPHA  END-IF                           
260200     IF W-IDLEVNR-ALPHA = 'CQPSA'                                         
260300        MOVE '3918 ' TO W-IDLEVNR-ALPHA  END-IF                           
260400     IF W-IDLEVNR-ALPHA = 'K3D7B'                                         
260500        MOVE '3965 ' TO W-IDLEVNR-ALPHA  END-IF                           
260600     IF W-IDLEVNR-ALPHA = 'G273T'                                         
260700        MOVE '4164 ' TO W-IDLEVNR-ALPHA  END-IF                           
260800     IF W-IDLEVNR-ALPHA = 'G255C'                                         
260900        MOVE '4239 ' TO W-IDLEVNR-ALPHA  END-IF                           
261000     IF W-IDLEVNR-ALPHA = 'C212A'                                         
261100        MOVE '4274 ' TO W-IDLEVNR-ALPHA  END-IF                           
261200     IF W-IDLEVNR-ALPHA = 'EQ17A'                                         
261300        MOVE '4344 ' TO W-IDLEVNR-ALPHA  END-IF                           
261400     IF W-IDLEVNR-ALPHA = 'CN5BA'                                         
261500        MOVE '4528 ' TO W-IDLEVNR-ALPHA  END-IF                           
261600     IF W-IDLEVNR-ALPHA = 'C8Q0A'                                         
261700        MOVE '4955 ' TO W-IDLEVNR-ALPHA  END-IF                           
261800     IF W-IDLEVNR-ALPHA = 'K817J'                                         
261900        MOVE '4994 ' TO W-IDLEVNR-ALPHA  END-IF                           
262000     IF W-IDLEVNR-ALPHA = 'B40QG'                                         
262100        MOVE '5019 ' TO W-IDLEVNR-ALPHA  END-IF                           
262200     IF W-IDLEVNR-ALPHA = 'C7G4A'                                         
262300        MOVE '5051 ' TO W-IDLEVNR-ALPHA  END-IF                           
262400     IF W-IDLEVNR-ALPHA = 'C9A2A'                                         
262500        MOVE '5135 ' TO W-IDLEVNR-ALPHA  END-IF                           
262600     IF W-IDLEVNR-ALPHA = 'CFN4A'                                         
262700        MOVE '5287 ' TO W-IDLEVNR-ALPHA  END-IF                           
262800     IF W-IDLEVNR-ALPHA = 'P8D3A'                                         
262900        MOVE '5595 ' TO W-IDLEVNR-ALPHA  END-IF                           
263000     IF W-IDLEVNR-ALPHA = 'F962A'                                         
263100        MOVE '6026 ' TO W-IDLEVNR-ALPHA  END-IF                           
263200     IF W-IDLEVNR-ALPHA = 'DL6KA'                                         
263300        MOVE '6163 ' TO W-IDLEVNR-ALPHA  END-IF                           
263400     IF W-IDLEVNR-ALPHA = 'D0SAA'                                         
263500        MOVE '6235 ' TO W-IDLEVNR-ALPHA  END-IF                           
263600     IF W-IDLEVNR-ALPHA = 'DLLEA'                                         
263700        MOVE '6296 ' TO W-IDLEVNR-ALPHA  END-IF                           
263800     IF W-IDLEVNR-ALPHA = 'BX6BC'                                         
263900        MOVE '6335 ' TO W-IDLEVNR-ALPHA  END-IF                           
264000     IF W-IDLEVNR-ALPHA = 'CUQLA'                                         
264100        MOVE '6584 ' TO W-IDLEVNR-ALPHA  END-IF                           
264200     IF W-IDLEVNR-ALPHA = 'T1X5A'                                         
264300        MOVE '6838 ' TO W-IDLEVNR-ALPHA  END-IF                           
264400     IF W-IDLEVNR-ALPHA = 'D3R7A'                                         
264500        MOVE '6908 ' TO W-IDLEVNR-ALPHA  END-IF                           
264600     IF W-IDLEVNR-ALPHA = 'CFT1A'                                         
264700        MOVE '7203 ' TO W-IDLEVNR-ALPHA  END-IF                           
264800     IF W-IDLEVNR-ALPHA = 'K8XJA'                                         
264900        MOVE '7231 ' TO W-IDLEVNR-ALPHA  END-IF                           
265000     IF W-IDLEVNR-ALPHA = 'BP3JA'                                         
265100        MOVE '7367 ' TO W-IDLEVNR-ALPHA  END-IF                           
265200     IF W-IDLEVNR-ALPHA = 'DT9EA'                                         
265300        MOVE '7662 ' TO W-IDLEVNR-ALPHA  END-IF                           
265400     IF W-IDLEVNR-ALPHA = 'BH6NA'                                         
265500        MOVE '7724 ' TO W-IDLEVNR-ALPHA  END-IF                           
265600     IF W-IDLEVNR-ALPHA = 'CKSMA'                                         
265700        MOVE '7767 ' TO W-IDLEVNR-ALPHA  END-IF                           
265800     IF W-IDLEVNR-ALPHA = 'BQ8GA'                                         
265900        MOVE '7786 ' TO W-IDLEVNR-ALPHA  END-IF                           
266000     IF W-IDLEVNR-ALPHA = 'DLLSA'                                         
266100        MOVE '7821 ' TO W-IDLEVNR-ALPHA  END-IF                           
266200     IF W-IDLEVNR-ALPHA = 'DBF3A'                                         
266300        MOVE '7956 ' TO W-IDLEVNR-ALPHA  END-IF                           
266400     IF W-IDLEVNR-ALPHA = 'H222A'                                         
266500        MOVE '7961 ' TO W-IDLEVNR-ALPHA  END-IF                           
266600     IF W-IDLEVNR-ALPHA = 'P1NQA'                                         
266700        MOVE '8204 ' TO W-IDLEVNR-ALPHA  END-IF                           
266800     IF W-IDLEVNR-ALPHA = 'CFNNA'                                         
266900        MOVE '10105' TO W-IDLEVNR-ALPHA  END-IF                           
267000     IF W-IDLEVNR-ALPHA = 'CN5LA'                                         
267100        MOVE '10141' TO W-IDLEVNR-ALPHA  END-IF                           
267200     IF W-IDLEVNR-ALPHA = 'L8K5R'                                         
267300        MOVE '10178' TO W-IDLEVNR-ALPHA  END-IF                           
267400     IF W-IDLEVNR-ALPHA = 'BRV3A'                                         
267500        MOVE '10339' TO W-IDLEVNR-ALPHA  END-IF                           
267600     IF W-IDLEVNR-ALPHA = 'L8K5G'                                         
267700        MOVE '10342' TO W-IDLEVNR-ALPHA  END-IF                           
267800     IF W-IDLEVNR-ALPHA = 'U7PMA'                                         
267900        MOVE '10986' TO W-IDLEVNR-ALPHA  END-IF                           
268000     IF W-IDLEVNR-ALPHA = 'E1P5A'                                         
268100        MOVE '11578' TO W-IDLEVNR-ALPHA  END-IF                           
268200     IF W-IDLEVNR-ALPHA = 'BQ9EA'                                         
268300        MOVE '12569' TO W-IDLEVNR-ALPHA  END-IF                           
268400     IF W-IDLEVNR-ALPHA = 'T655C'                                         
268500        MOVE '13011' TO W-IDLEVNR-ALPHA  END-IF                           
268600     IF W-IDLEVNR-ALPHA = 'D3U6A'                                         
268700        MOVE '13386' TO W-IDLEVNR-ALPHA  END-IF                           
268800     IF W-IDLEVNR-ALPHA = 'S4LBA'                                         
268900        MOVE '13391' TO W-IDLEVNR-ALPHA  END-IF                           
269000     IF W-IDLEVNR-ALPHA = 'DPVGA'                                         
269100        MOVE '13486' TO W-IDLEVNR-ALPHA  END-IF                           
269200     IF W-IDLEVNR-ALPHA = 'BQ9HB'                                         
269300        MOVE '13516' TO W-IDLEVNR-ALPHA  END-IF                           
269400     IF W-IDLEVNR-ALPHA = 'M9TPB'                                         
269500        MOVE '13556' TO W-IDLEVNR-ALPHA  END-IF                           
269600     IF W-IDLEVNR-ALPHA = 'BN7SA'                                         
269700        MOVE '13598' TO W-IDLEVNR-ALPHA  END-IF                           
269800     IF W-IDLEVNR-ALPHA = 'BQ9LA'                                         
269900        MOVE '13600' TO W-IDLEVNR-ALPHA  END-IF                           
270000     IF W-IDLEVNR-ALPHA = 'BQ9HC'                                         
270100        MOVE '13661' TO W-IDLEVNR-ALPHA  END-IF                           
270200     IF W-IDLEVNR-ALPHA = 'V4FWA'                                         
270300        MOVE '14313' TO W-IDLEVNR-ALPHA  END-IF                           
270400     IF W-IDLEVNR-ALPHA = 'D45NA'                                         
270500        MOVE '14602' TO W-IDLEVNR-ALPHA  END-IF                           
270600     IF W-IDLEVNR-ALPHA = 'L8K5A'                                         
270700        MOVE '14605' TO W-IDLEVNR-ALPHA  END-IF                           
270800     IF W-IDLEVNR-ALPHA = 'D26YB'                                         
270900        MOVE '15580' TO W-IDLEVNR-ALPHA  END-IF                           
271000     IF W-IDLEVNR-ALPHA = 'BTKBA'                                         
271100        MOVE '16048' TO W-IDLEVNR-ALPHA  END-IF                           
271200     IF W-IDLEVNR-ALPHA = 'S13SA'                                         
271300        MOVE '16058' TO W-IDLEVNR-ALPHA  END-IF                           
271400     IF W-IDLEVNR-ALPHA = 'E510F'                                         
271500        MOVE '16132' TO W-IDLEVNR-ALPHA  END-IF                           
271600     IF W-IDLEVNR-ALPHA = 'R7B3A'                                         
271700        MOVE '16158' TO W-IDLEVNR-ALPHA  END-IF                           
271800     IF W-IDLEVNR-ALPHA = 'CJ0GA'                                         
271900        MOVE '16251' TO W-IDLEVNR-ALPHA  END-IF                           
272000     IF W-IDLEVNR-ALPHA = 'S1YHA'                                         
272100        MOVE '16267' TO W-IDLEVNR-ALPHA  END-IF                           
272200     IF W-IDLEVNR-ALPHA = 'BQ9UA'                                         
272300        MOVE '16325' TO W-IDLEVNR-ALPHA  END-IF                           
272400     IF W-IDLEVNR-ALPHA = 'T446A'                                         
272500        MOVE '16470' TO W-IDLEVNR-ALPHA  END-IF                           
272600     IF W-IDLEVNR-ALPHA = 'B44XE'                                         
272700        MOVE '16490' TO W-IDLEVNR-ALPHA  END-IF                           
272800     IF W-IDLEVNR-ALPHA = 'BRV3B'                                         
272900        MOVE '17712' TO W-IDLEVNR-ALPHA  END-IF                           
273000     IF W-IDLEVNR-ALPHA = 'CN5QA'                                         
273100        MOVE '17713' TO W-IDLEVNR-ALPHA  END-IF                           
273200     IF W-IDLEVNR-ALPHA = 'CN5SA'                                         
273300        MOVE '17819' TO W-IDLEVNR-ALPHA  END-IF                           
273400     IF W-IDLEVNR-ALPHA = 'CW5YA'                                         
273500        MOVE '17819' TO W-IDLEVNR-ALPHA  END-IF                           
273600     IF W-IDLEVNR-ALPHA = 'DSHRA'                                         
273700        MOVE '18971' TO W-IDLEVNR-ALPHA  END-IF                           
273800     IF W-IDLEVNR-ALPHA = 'C9T2C'                                         
273900        MOVE '18997' TO W-IDLEVNR-ALPHA  END-IF                           
274000     IF W-IDLEVNR-ALPHA = 'AA8SA'                                         
274100        MOVE '19892' TO W-IDLEVNR-ALPHA  END-IF                           
274200     IF W-IDLEVNR-ALPHA = 'F641A'                                         
274300        MOVE '23093' TO W-IDLEVNR-ALPHA  END-IF                           
274400     IF W-IDLEVNR-ALPHA = 'DLNVA'                                         
274500        MOVE '23685' TO W-IDLEVNR-ALPHA  END-IF                           
274600     IF W-IDLEVNR-ALPHA = 'P108C'                                         
274700        MOVE '24263' TO W-IDLEVNR-ALPHA  END-IF                           
274800     IF W-IDLEVNR-ALPHA = 'CT3JA'                                         
274900        MOVE '24521' TO W-IDLEVNR-ALPHA  END-IF                           
275000     IF W-IDLEVNR-ALPHA = 'CW0HA'                                         
275100        MOVE '24653' TO W-IDLEVNR-ALPHA  END-IF                           
275200     IF W-IDLEVNR-ALPHA = 'CVVEA'                                         
275300        MOVE '24789' TO W-IDLEVNR-ALPHA  END-IF                           
275400     IF W-IDLEVNR-ALPHA = 'S5U0B'                                         
275500        MOVE '24896' TO W-IDLEVNR-ALPHA  END-IF                           
275600     IF W-IDLEVNR-ALPHA = 'CYSZB'                                         
275700        MOVE '24921' TO W-IDLEVNR-ALPHA  END-IF                           
275800     IF W-IDLEVNR-ALPHA = 'C72GA'                                         
275900        MOVE '24999' TO W-IDLEVNR-ALPHA  END-IF                           
276000     IF W-IDLEVNR-ALPHA = 'CYFWA'                                         
276100        MOVE '25286' TO W-IDLEVNR-ALPHA  END-IF                           
276200     IF W-IDLEVNR-ALPHA = 'BKL3A'                                         
276300        MOVE '25402' TO W-IDLEVNR-ALPHA  END-IF                           
276400     IF W-IDLEVNR-ALPHA = 'CZSEB'                                         
276500        MOVE '25620' TO W-IDLEVNR-ALPHA  END-IF                           
276600     IF W-IDLEVNR-ALPHA = 'DBBTA'                                         
276700        MOVE '25755' TO W-IDLEVNR-ALPHA  END-IF                           
276800     IF W-IDLEVNR-ALPHA = 'DA9EA'                                         
276900        MOVE '25765' TO W-IDLEVNR-ALPHA  END-IF                           
277000     IF W-IDLEVNR-ALPHA = 'JWMJA'                                         
277100        MOVE '25770' TO W-IDLEVNR-ALPHA  END-IF                           
277200     IF W-IDLEVNR-ALPHA = 'CYDAB'                                         
277300        MOVE '25797' TO W-IDLEVNR-ALPHA  END-IF                           
277400     IF W-IDLEVNR-ALPHA = 'EQ62A'                                         
277500        MOVE '25808' TO W-IDLEVNR-ALPHA  END-IF                           
277600     IF W-IDLEVNR-ALPHA = 'DDA4A'                                         
277700        MOVE '25810' TO W-IDLEVNR-ALPHA  END-IF                           
277800     IF W-IDLEVNR-ALPHA = 'DDETA'                                         
277900        MOVE '25818' TO W-IDLEVNR-ALPHA  END-IF                           
278000     IF W-IDLEVNR-ALPHA = 'V2NYA'                                         
278100        MOVE '25847' TO W-IDLEVNR-ALPHA  END-IF                           
278200     IF W-IDLEVNR-ALPHA = 'T655D'                                         
278300        MOVE '25859' TO W-IDLEVNR-ALPHA  END-IF                           
278400     IF W-IDLEVNR-ALPHA = 'G261S'                                         
278500        MOVE '25869' TO W-IDLEVNR-ALPHA  END-IF                           
278600     IF W-IDLEVNR-ALPHA = 'DH0RA'                                         
278700        MOVE '25870' TO W-IDLEVNR-ALPHA  END-IF                           
278800     IF W-IDLEVNR-ALPHA = 'BNWSE'                                         
278900        MOVE '25972' TO W-IDLEVNR-ALPHA  END-IF                           
279000     IF W-IDLEVNR-ALPHA = 'DRTHA'                                         
279100        MOVE '25996' TO W-IDLEVNR-ALPHA  END-IF                           
279200     IF W-IDLEVNR-ALPHA = 'BP8HA'                                         
279300        MOVE '34621' TO W-IDLEVNR-ALPHA  END-IF                           
279400     IF W-IDLEVNR-ALPHA = 'U9JXA'                                         
279500        MOVE '51065' TO W-IDLEVNR-ALPHA  END-IF                           
279600     IF W-IDLEVNR-ALPHA = 'A405A'                                         
279700        MOVE '51103' TO W-IDLEVNR-ALPHA  END-IF                           
279800     IF W-IDLEVNR-ALPHA = 'T733E'                                         
279900        MOVE '51568' TO W-IDLEVNR-ALPHA  END-IF                           
280000     IF W-IDLEVNR-ALPHA = 'F903H'                                         
280100        MOVE '62513' TO W-IDLEVNR-ALPHA  END-IF                           
280200     IF W-IDLEVNR-ALPHA = 'DCYQA'                                         
280300        MOVE '63300' TO W-IDLEVNR-ALPHA  END-IF                           
280400*************************************************                         
280500     IF W-IDLEVNR-ALPHA = 'C61MA'                                         
280600        MOVE '82   ' TO W-IDLEVNR-ALPHA  END-IF                           
280700     IF W-IDLEVNR-ALPHA = 'W064Z'                                         
280800        MOVE '4001 ' TO W-IDLEVNR-ALPHA  END-IF                           
280900     IF W-IDLEVNR-ALPHA = 'F260B'                                         
281000        MOVE '4038 ' TO W-IDLEVNR-ALPHA  END-IF                           
281100     IF W-IDLEVNR-ALPHA = 'BPQUA'                                         
281200        MOVE '4210 ' TO W-IDLEVNR-ALPHA  END-IF                           
281300     IF W-IDLEVNR-ALPHA = 'L217Q'                                         
281400        MOVE '4230 ' TO W-IDLEVNR-ALPHA  END-IF                           
281500     IF W-IDLEVNR-ALPHA = 'BPQUA'                                         
281600        MOVE '4280 ' TO W-IDLEVNR-ALPHA  END-IF                           
281700     IF W-IDLEVNR-ALPHA = 'BPGRA'                                         
281800        MOVE '4536 ' TO W-IDLEVNR-ALPHA  END-IF                           
281900     IF W-IDLEVNR-ALPHA = 'KFBGA'                                         
282000        MOVE '4667 ' TO W-IDLEVNR-ALPHA  END-IF                           
282100     IF W-IDLEVNR-ALPHA = 'D00MD'                                         
282200        MOVE '4921 ' TO W-IDLEVNR-ALPHA  END-IF                           
282300     IF W-IDLEVNR-ALPHA = 'D00MB'                                         
282400        MOVE '4930 ' TO W-IDLEVNR-ALPHA  END-IF                           
282500     IF W-IDLEVNR-ALPHA = 'D2T1E'                                         
282600        MOVE '6434 ' TO W-IDLEVNR-ALPHA  END-IF                           
282700     IF W-IDLEVNR-ALPHA = 'V1W4A'                                         
282800        MOVE '6969 ' TO W-IDLEVNR-ALPHA  END-IF                           
282900     IF W-IDLEVNR-ALPHA = 'J7TDB'                                         
283000        MOVE '7254 ' TO W-IDLEVNR-ALPHA  END-IF                           
283100     IF W-IDLEVNR-ALPHA = 'P981A'                                         
283200        MOVE '7994 ' TO W-IDLEVNR-ALPHA  END-IF                           
283300     IF W-IDLEVNR-ALPHA = 'TC12A'                                         
283400        MOVE '12101' TO W-IDLEVNR-ALPHA  END-IF                           
283500     IF W-IDLEVNR-ALPHA = 'BNWSB'                                         
283600        MOVE '13673' TO W-IDLEVNR-ALPHA  END-IF                           
283700     IF W-IDLEVNR-ALPHA = 'D00ME'                                         
283800        MOVE '14345' TO W-IDLEVNR-ALPHA  END-IF                           
283900     IF W-IDLEVNR-ALPHA = 'BQ6XB'                                         
284000        MOVE '14642' TO W-IDLEVNR-ALPHA  END-IF                           
284100     IF W-IDLEVNR-ALPHA = 'B468X'                                         
284200        MOVE '17064' TO W-IDLEVNR-ALPHA  END-IF                           
284300     IF W-IDLEVNR-ALPHA = 'B535B'                                         
284400        MOVE '18992' TO W-IDLEVNR-ALPHA  END-IF                           
284500     IF W-IDLEVNR-ALPHA = 'CQPPA'                                         
284600        MOVE '19018' TO W-IDLEVNR-ALPHA  END-IF                           
284700     IF W-IDLEVNR-ALPHA = 'T6A3A'                                         
284800        MOVE '23559' TO W-IDLEVNR-ALPHA  END-IF                           
284900     IF W-IDLEVNR-ALPHA = 'D00MF'                                         
285000        MOVE '23560' TO W-IDLEVNR-ALPHA  END-IF                           
285100     IF W-IDLEVNR-ALPHA = 'B40CC'                                         
285200        MOVE '23758' TO W-IDLEVNR-ALPHA  END-IF                           
285300     IF W-IDLEVNR-ALPHA = 'MLE6B'                                         
285400        MOVE '25832' TO W-IDLEVNR-ALPHA  END-IF                           
285500     IF W-IDLEVNR-ALPHA = 'D00MG'                                         
285600        MOVE '25925' TO W-IDLEVNR-ALPHA  END-IF                           
285700     IF W-IDLEVNR-ALPHA = 'D00MH'                                         
285800        MOVE '34345' TO W-IDLEVNR-ALPHA  END-IF                           
285900     IF W-IDLEVNR-ALPHA = 'R235B'                                         
286000        MOVE '50091' TO W-IDLEVNR-ALPHA  END-IF                           
286100     IF W-IDLEVNR-ALPHA = 'M617T'                                         
286200        MOVE '62385' TO W-IDLEVNR-ALPHA  END-IF                           
286300     IF W-IDLEVNR-ALPHA = 'R235G'                                         
286400        MOVE '63544' TO W-IDLEVNR-ALPHA  END-IF                           
286500*************************************************                         
286600     IF W-IDLEVNR-ALPHA = 'CQF3A'                                         
286700        MOVE '124  ' TO W-IDLEVNR-ALPHA  END-IF                           
286800     IF W-IDLEVNR-ALPHA = 'BJRAA'                                         
286900        MOVE '175  ' TO W-IDLEVNR-ALPHA  END-IF                           
287000     IF W-IDLEVNR-ALPHA = 'DFVDA'                                         
287100        MOVE '281  ' TO W-IDLEVNR-ALPHA  END-IF                           
287200     IF W-IDLEVNR-ALPHA = 'BJVLA'                                         
287300        MOVE '312  ' TO W-IDLEVNR-ALPHA  END-IF                           
287400     IF W-IDLEVNR-ALPHA = 'BQ1PA'                                         
287500        MOVE '966  ' TO W-IDLEVNR-ALPHA  END-IF                           
287600     IF W-IDLEVNR-ALPHA = 'S3DHA'                                         
287700        MOVE '1125 ' TO W-IDLEVNR-ALPHA  END-IF                           
287800     IF W-IDLEVNR-ALPHA = 'CFNGA'                                         
287900        MOVE '1235 ' TO W-IDLEVNR-ALPHA  END-IF                           
288000     IF W-IDLEVNR-ALPHA = 'CFNHA'                                         
288100        MOVE '1271 ' TO W-IDLEVNR-ALPHA  END-IF                           
288200     IF W-IDLEVNR-ALPHA = 'BKTVA'                                         
288300        MOVE '1495 ' TO W-IDLEVNR-ALPHA  END-IF                           
288400     IF W-IDLEVNR-ALPHA = 'CFJEA'                                         
288500        MOVE '1757 ' TO W-IDLEVNR-ALPHA  END-IF                           
288600     IF W-IDLEVNR-ALPHA = 'BYL2A'                                         
288700        MOVE '2001 ' TO W-IDLEVNR-ALPHA  END-IF                           
288800     IF W-IDLEVNR-ALPHA = 'CFTPA'                                         
288900        MOVE '2250 ' TO W-IDLEVNR-ALPHA  END-IF                           
289000     IF W-IDLEVNR-ALPHA = 'S3DHC'                                         
289100        MOVE '2429 ' TO W-IDLEVNR-ALPHA  END-IF                           
289200     IF W-IDLEVNR-ALPHA = 'BPUFA'                                         
289300        MOVE '2503 ' TO W-IDLEVNR-ALPHA  END-IF                           
289400     IF W-IDLEVNR-ALPHA = 'BKXQA'                                         
289500        MOVE '2650 ' TO W-IDLEVNR-ALPHA  END-IF                           
289600     IF W-IDLEVNR-ALPHA = 'CFFXA'                                         
289700        MOVE '3143 ' TO W-IDLEVNR-ALPHA  END-IF                           
289800     IF W-IDLEVNR-ALPHA = 'D1V4A'                                         
289900        MOVE '3312 ' TO W-IDLEVNR-ALPHA  END-IF                           
290000     IF W-IDLEVNR-ALPHA = 'L8K5V'                                         
290100        MOVE '3341 ' TO W-IDLEVNR-ALPHA  END-IF                           
290200     IF W-IDLEVNR-ALPHA = 'S552A'                                         
290300        MOVE '6074 ' TO W-IDLEVNR-ALPHA  END-IF                           
290400     IF W-IDLEVNR-ALPHA = 'T226F'                                         
290500        MOVE '6089 ' TO W-IDLEVNR-ALPHA  END-IF                           
290600     IF W-IDLEVNR-ALPHA = 'C8P5A'                                         
290700        MOVE '6670 ' TO W-IDLEVNR-ALPHA  END-IF                           
290800     IF W-IDLEVNR-ALPHA = 'JBA1A'                                         
290900        MOVE '6745 ' TO W-IDLEVNR-ALPHA  END-IF                           
291000     IF W-IDLEVNR-ALPHA = 'BP7YA'                                         
291100        MOVE '7500 ' TO W-IDLEVNR-ALPHA  END-IF                           
291200     IF W-IDLEVNR-ALPHA = 'CY7ZA'                                         
291300        MOVE '7923 ' TO W-IDLEVNR-ALPHA  END-IF                           
291400     IF W-IDLEVNR-ALPHA = 'BJVHA'                                         
291500        MOVE '10057' TO W-IDLEVNR-ALPHA  END-IF                           
291600     IF W-IDLEVNR-ALPHA = 'D36ZA'                                         
291700        MOVE '10947' TO W-IDLEVNR-ALPHA  END-IF                           
291800     IF W-IDLEVNR-ALPHA = 'J3BDA'                                         
291900        MOVE '14922' TO W-IDLEVNR-ALPHA  END-IF                           
292000     IF W-IDLEVNR-ALPHA = 'J6R3A'                                         
292100        MOVE '14990' TO W-IDLEVNR-ALPHA  END-IF                           
292200     IF W-IDLEVNR-ALPHA = 'C0VAH'                                         
292300        MOVE '15284' TO W-IDLEVNR-ALPHA  END-IF                           
292400     IF W-IDLEVNR-ALPHA = 'C8P5C'                                         
292500        MOVE '16037' TO W-IDLEVNR-ALPHA  END-IF                           
292600     IF W-IDLEVNR-ALPHA = 'C8P5D'                                         
292700        MOVE '16179' TO W-IDLEVNR-ALPHA  END-IF                           
292800     IF W-IDLEVNR-ALPHA = 'KTY7B'                                         
292900        MOVE '16336' TO W-IDLEVNR-ALPHA  END-IF                           
293000     IF W-IDLEVNR-ALPHA = 'C8P5J'                                         
293100        MOVE '16372' TO W-IDLEVNR-ALPHA  END-IF                           
293200     IF W-IDLEVNR-ALPHA = 'DR7TA'                                         
293300        MOVE '19206' TO W-IDLEVNR-ALPHA  END-IF                           
293400     IF W-IDLEVNR-ALPHA = 'C9H7A'                                         
293500        MOVE '19611' TO W-IDLEVNR-ALPHA  END-IF                           
293600     IF W-IDLEVNR-ALPHA = 'BXMZA'                                         
293700        MOVE '19914' TO W-IDLEVNR-ALPHA  END-IF                           
293800     IF W-IDLEVNR-ALPHA = 'C72DC'                                         
293900        MOVE '22292' TO W-IDLEVNR-ALPHA  END-IF                           
294000     IF W-IDLEVNR-ALPHA = 'A0VWC'                                         
294100        MOVE '23934' TO W-IDLEVNR-ALPHA  END-IF                           
294200     IF W-IDLEVNR-ALPHA = 'BQ9VB'                                         
294300        MOVE '24081' TO W-IDLEVNR-ALPHA  END-IF                           
294400     IF W-IDLEVNR-ALPHA = 'AJFWA'                                         
294500        MOVE '25072' TO W-IDLEVNR-ALPHA  END-IF                           
294600     IF W-IDLEVNR-ALPHA = 'AJFXA'                                         
294700        MOVE '25073' TO W-IDLEVNR-ALPHA  END-IF                           
294800     IF W-IDLEVNR-ALPHA = 'D2L4A'                                         
294900        MOVE '25082' TO W-IDLEVNR-ALPHA  END-IF                           
295000     IF W-IDLEVNR-ALPHA = 'K261A'                                         
295100        MOVE '25682' TO W-IDLEVNR-ALPHA  END-IF                           
295200     IF W-IDLEVNR-ALPHA = 'A76VE'                                         
295300        MOVE '25817' TO W-IDLEVNR-ALPHA  END-IF                           
295400     IF W-IDLEVNR-ALPHA = 'D36ZC'                                         
295500        MOVE '25826' TO W-IDLEVNR-ALPHA  END-IF                           
295600     IF W-IDLEVNR-ALPHA = 'DSDLA'                                         
295700        MOVE '25871' TO W-IDLEVNR-ALPHA  END-IF                           
295800     IF W-IDLEVNR-ALPHA = 'Q9KNA'                                         
295900        MOVE '25967' TO W-IDLEVNR-ALPHA  END-IF                           
296000     IF W-IDLEVNR-ALPHA = 'L2HWG'                                         
296100        MOVE '63609' TO W-IDLEVNR-ALPHA  END-IF                           
296200*************************************************                         
296300*************************************************                         
296400     IF W-IDLEVNR-ALPHA = 'BPGTB'                                         
296500        MOVE '211  ' TO W-IDLEVNR-ALPHA  END-IF                           
296600     IF W-IDLEVNR-ALPHA = 'BPGTA'                                         
296700        MOVE '1826 ' TO W-IDLEVNR-ALPHA  END-IF                           
296800     IF W-IDLEVNR-ALPHA = 'AD1YA'                                         
296900        MOVE '3469 ' TO W-IDLEVNR-ALPHA  END-IF                           
297000     IF W-IDLEVNR-ALPHA = 'L8K5U'                                         
297100        MOVE '3471 ' TO W-IDLEVNR-ALPHA  END-IF                           
297200     IF W-IDLEVNR-ALPHA = 'G8UTB'                                         
297300        MOVE '3494 ' TO W-IDLEVNR-ALPHA  END-IF                           
297400     IF W-IDLEVNR-ALPHA = 'MCSQA'                                         
297500        MOVE '3767 ' TO W-IDLEVNR-ALPHA  END-IF                           
297600     IF W-IDLEVNR-ALPHA = 'LNAPA'                                         
297700        MOVE '4540 ' TO W-IDLEVNR-ALPHA  END-IF                           
297800     IF W-IDLEVNR-ALPHA = 'LJVZA'                                         
297900        MOVE '4730 ' TO W-IDLEVNR-ALPHA  END-IF                           
298000     IF W-IDLEVNR-ALPHA = 'DL5LA'                                         
298100        MOVE '4762 ' TO W-IDLEVNR-ALPHA  END-IF                           
298200     IF W-IDLEVNR-ALPHA = 'DDE2A'                                         
298300        MOVE '4790 ' TO W-IDLEVNR-ALPHA  END-IF                           
298400     IF W-IDLEVNR-ALPHA = 'F636B'                                         
298500        MOVE '5154 ' TO W-IDLEVNR-ALPHA  END-IF                           
298600     IF W-IDLEVNR-ALPHA = 'Q33YA'                                         
298700        MOVE '5220 ' TO W-IDLEVNR-ALPHA  END-IF                           
298800     IF W-IDLEVNR-ALPHA = 'DGDKA'                                         
298900        MOVE '5239 ' TO W-IDLEVNR-ALPHA  END-IF                           
299000     IF W-IDLEVNR-ALPHA = 'B517X'                                         
299100        MOVE '5419 ' TO W-IDLEVNR-ALPHA  END-IF                           
299200     IF W-IDLEVNR-ALPHA = 'BRGHA'                                         
299300        MOVE '5614 ' TO W-IDLEVNR-ALPHA  END-IF                           
299400     IF W-IDLEVNR-ALPHA = 'C89GA'                                         
299500        MOVE '5664 ' TO W-IDLEVNR-ALPHA  END-IF                           
299600     IF W-IDLEVNR-ALPHA = 'CKBZA'                                         
299700        MOVE '6133 ' TO W-IDLEVNR-ALPHA  END-IF                           
299800     IF W-IDLEVNR-ALPHA = 'DL6LA'                                         
299900        MOVE '6292 ' TO W-IDLEVNR-ALPHA  END-IF                           
300000     IF W-IDLEVNR-ALPHA = 'D17MA'                                         
300100        MOVE '6363 ' TO W-IDLEVNR-ALPHA  END-IF                           
300200     IF W-IDLEVNR-ALPHA = 'E355B'                                         
300300        MOVE '6369 ' TO W-IDLEVNR-ALPHA  END-IF                           
300400     IF W-IDLEVNR-ALPHA = 'R1N9A'                                         
300500        MOVE '6552 ' TO W-IDLEVNR-ALPHA  END-IF                           
300600     IF W-IDLEVNR-ALPHA = 'M738A'                                         
300700        MOVE '6599 ' TO W-IDLEVNR-ALPHA  END-IF                           
300800     IF W-IDLEVNR-ALPHA = 'DLLMA'                                         
300900        MOVE '6677 ' TO W-IDLEVNR-ALPHA  END-IF                           
301000     IF W-IDLEVNR-ALPHA = 'B45GA'                                         
301100        MOVE '6748 ' TO W-IDLEVNR-ALPHA  END-IF                           
301200     IF W-IDLEVNR-ALPHA = 'D0FVB'                                         
301300        MOVE '7752 ' TO W-IDLEVNR-ALPHA  END-IF                           
301400     IF W-IDLEVNR-ALPHA = 'D0HHD'                                         
301500        MOVE '7800 ' TO W-IDLEVNR-ALPHA  END-IF                           
301600     IF W-IDLEVNR-ALPHA = 'BPTQB'                                         
301700        MOVE '10157' TO W-IDLEVNR-ALPHA  END-IF                           
301800     IF W-IDLEVNR-ALPHA = 'BPTQA'                                         
301900        MOVE '10158' TO W-IDLEVNR-ALPHA  END-IF                           
302000     IF W-IDLEVNR-ALPHA = 'DL7EA'                                         
302100        MOVE '10356' TO W-IDLEVNR-ALPHA  END-IF                           
302200     IF W-IDLEVNR-ALPHA = 'CT3KA'                                         
302300        MOVE '10934' TO W-IDLEVNR-ALPHA  END-IF                           
302400     IF W-IDLEVNR-ALPHA = 'DL7KA'                                         
302500        MOVE '13349' TO W-IDLEVNR-ALPHA  END-IF                           
302600     IF W-IDLEVNR-ALPHA = 'DL7LA'                                         
302700        MOVE '13388' TO W-IDLEVNR-ALPHA  END-IF                           
302800     IF W-IDLEVNR-ALPHA = 'DLMNA'                                         
302900        MOVE '13396' TO W-IDLEVNR-ALPHA  END-IF                           
303000     IF W-IDLEVNR-ALPHA = 'DBQZA'                                         
303100        MOVE '13571' TO W-IDLEVNR-ALPHA  END-IF                           
303200     IF W-IDLEVNR-ALPHA = 'CRKZA'                                         
303300        MOVE '13572' TO W-IDLEVNR-ALPHA  END-IF                           
303400     IF W-IDLEVNR-ALPHA = 'DL7WA'                                         
303500        MOVE '13583' TO W-IDLEVNR-ALPHA  END-IF                           
303600     IF W-IDLEVNR-ALPHA = 'BQAEB'                                         
303700        MOVE '13717' TO W-IDLEVNR-ALPHA  END-IF                           
303800     IF W-IDLEVNR-ALPHA = 'Q5EGA'                                         
303900        MOVE '14947' TO W-IDLEVNR-ALPHA  END-IF                           
304000     IF W-IDLEVNR-ALPHA = 'Q89FA'                                         
304100        MOVE '16030' TO W-IDLEVNR-ALPHA  END-IF                           
304200     IF W-IDLEVNR-ALPHA = 'AQ2MB'                                         
304300        MOVE '16112' TO W-IDLEVNR-ALPHA  END-IF                           
304400     IF W-IDLEVNR-ALPHA = 'CNT5A'                                         
304500        MOVE '20522' TO W-IDLEVNR-ALPHA  END-IF                           
304600     IF W-IDLEVNR-ALPHA = 'BMZJA'                                         
304700        MOVE '24489' TO W-IDLEVNR-ALPHA  END-IF                           
304800     IF W-IDLEVNR-ALPHA = 'ATC2A'                                         
304900        MOVE '25089' TO W-IDLEVNR-ALPHA  END-IF                           
305000     IF W-IDLEVNR-ALPHA = 'MAXWA'                                         
305100        MOVE '25090' TO W-IDLEVNR-ALPHA  END-IF                           
305200     IF W-IDLEVNR-ALPHA = 'BL2CA'                                         
305300        MOVE '25412' TO W-IDLEVNR-ALPHA  END-IF                           
305400     IF W-IDLEVNR-ALPHA = 'M799G'                                         
305500        MOVE '25446' TO W-IDLEVNR-ALPHA  END-IF                           
305600     IF W-IDLEVNR-ALPHA = 'N2KUA'                                         
305700        MOVE '25828' TO W-IDLEVNR-ALPHA  END-IF                           
305800     IF W-IDLEVNR-ALPHA = 'D38ME'                                         
305900        MOVE '25878' TO W-IDLEVNR-ALPHA  END-IF                           
306000     IF W-IDLEVNR-ALPHA = 'D33BA'                                         
306100        MOVE '25910' TO W-IDLEVNR-ALPHA  END-IF                           
306200     IF W-IDLEVNR-ALPHA = 'BPTQD'                                         
306300        MOVE '25924' TO W-IDLEVNR-ALPHA  END-IF                           
306400     IF W-IDLEVNR-ALPHA = 'DPEWA'                                         
306500        MOVE '25962' TO W-IDLEVNR-ALPHA  END-IF                           
306600     IF W-IDLEVNR-ALPHA = 'DP5RA'                                         
306700        MOVE '25973' TO W-IDLEVNR-ALPHA  END-IF                           
306800     IF W-IDLEVNR-ALPHA = 'DSBYA'                                         
306900        MOVE '26009' TO W-IDLEVNR-ALPHA  END-IF                           
307000     IF W-IDLEVNR-ALPHA = 'CRX1D'                                         
307100        MOVE '26012' TO W-IDLEVNR-ALPHA  END-IF                           
307200     IF W-IDLEVNR-ALPHA = 'BKDDB'                                         
307300        MOVE '31014' TO W-IDLEVNR-ALPHA  END-IF                           
307400     IF W-IDLEVNR-ALPHA = 'BKDDC'                                         
307500        MOVE '41014' TO W-IDLEVNR-ALPHA  END-IF                           
307600*************************************************                         
307700**********************************************                            
307800                                                                          
307900     PERFORM  S21-IDLEVNR-ALPHA-TO-NUM                                    
308000                                                                          
308100     MOVE W-IDLEVNR-NUM       TO UTVCOM1-IDLEVNR-NUM                      
308200     MOVE IN1-PRARTVNA        TO UTVCOM1-PRARTVNA                         
308300     MOVE IN1-PRARTSTD        TO UTVCOM1-PRARTSTD                         
308400     MOVE IN1-FLIART          TO UTVCOM1-FLIART                           
308500     MOVE IN1-IDPROJ          TO UTVCOM1-IDPROJ                           
308600     MOVE IN1-IDAO(1)         TO UTVCOM1-IDAO(1)                          
308700     MOVE IN1-IDAO(2)         TO UTVCOM1-IDAO(2)                          
308800     MOVE IN1-TIFINLEV        TO UTVCOM1-TIFINLEV                         
308900     MOVE IN1-IDANSK          TO UTVCOM1-IDANSK                           
309000     MOVE IN1-KVPB            TO UTVCOM1-KVPB                             
309100     MOVE IN1-KDVVKL          TO UTVCOM1-KDVVKL                           
309200     MOVE IN1-BELEVART        TO UTVCOM1-BELEVART                         
309300     MOVE IN1-KDTIPPR         TO UTVCOM1-KDTIPPR                          
309400     IF IN1-IDINK(1:1) NUMERIC                                            
309500       IF IN1-IDINK(1:3) NUMERIC                                          
309600         MOVE IN1-IDINK(1:3)      TO UTVCOM1-IDINK-OLD                    
309700       ELSE                                                               
309800         IF IN1-IDINK(1:2) NUMERIC                                        
309900           MOVE IN1-IDINK(1:2)    TO UTVCOM1-IDINK-OLD                    
310000         ELSE                                                             
310100           MOVE ZERO              TO UTVCOM1-IDINK-OLD                    
310200         END-IF                                                           
310300       END-IF                                                             
310400     ELSE                                                                 
310500       IF IN1-IDINK(2:3) NUMERIC                                          
310600         MOVE IN1-IDINK(2:3)      TO UTVCOM1-IDINK-OLD                    
310700       ELSE                                                               
310800         IF IN1-IDINK(2:2) NUMERIC                                        
310900           MOVE IN1-IDINK(2:2)    TO UTVCOM1-IDINK-OLD                    
311000         ELSE                                                             
311100           MOVE ZERO              TO UTVCOM1-IDINK-OLD                    
311200         END-IF                                                           
311300       END-IF                                                             
311400     END-IF                                                               
311500                                                                          
311600     MOVE IN1-IDINK           TO UTVCOM1-IDINK                            
311700     MOVE IN1-TIREGDAT        TO UTVCOM1-TIREGDAT                         
311800     MOVE IN1-KDUART          TO UTVCOM1-KDUART                           
311900     MOVE IN1-IDARTNR-MOTSV   TO UTVCOM1-IDARTNR-MOTSV                    
312000     MOVE IN1-PRINK           TO UTVCOM1-PRINK                            
312100     MOVE IN1-IDRITN          TO UTVCOM1-IDRITN                           
312200     MOVE IN1-PRHANTK         TO UTVCOM1-PRHANTK                          
312300     MOVE IN1-KDAGE           TO UTVCOM1-KDAGE                            
312400     MOVE IN1-KDPSLLOC        TO UTVCOM1-KDPSLLOC                         
312500     MOVE IN1-SLAG-IDLEVNR    TO UTVCOM1-SLAG-IDLEVNR                     
312600                                 W-IDLEVNR-ALPHA                          
312700     MOVE IN1-IDKAT(1)        TO UTVCOM1-IDKAT(1)                         
312800     MOVE IN1-IDKAT(2)        TO UTVCOM1-IDKAT(2)                         
312900     MOVE IN1-IDKAT(3)        TO UTVCOM1-IDKAT(3)                         
313000     MOVE IN1-KDRAB           TO UTVCOM1-KDRAB                            
313100     MOVE IN1-PRARTBEL        TO UTVCOM1-PRARTBEL                         
313200     MOVE IN1-FLLSRDEL        TO UTVCOM1-FLLSRDEL                         
313300     MOVE IN1-IDPROJUP        TO UTVCOM1-IDPROJUP                         
313400     MOVE IN1-FLGEMFMC        TO UTVCOM1-FLGEMFMC                         
313410     MOVE IN1-TIURPROD        TO UTVCOM1-TIURPROD                         
313500****FIX FÖR ALFA LEVNR**************************                          
313600     IF W-IDLEVNR-ALPHA = 'BQ2QA'                                         
313700        MOVE '01385' TO W-IDLEVNR-ALPHA                                   
313800     END-IF                                                               
313900     IF W-IDLEVNR-ALPHA = 'D12YA'                                         
314000        MOVE '06414' TO W-IDLEVNR-ALPHA                                   
314100     END-IF                                                               
314200     IF W-IDLEVNR-ALPHA = 'D0KLA'                                         
314300        MOVE '06916' TO W-IDLEVNR-ALPHA                                   
314400     END-IF                                                               
314500**********************************************                            
314600*************************************************                         
314700     IF W-IDLEVNR-ALPHA = 'BJ7TB'                                         
314800        MOVE '10132' TO W-IDLEVNR-ALPHA  END-IF                           
314900     IF W-IDLEVNR-ALPHA = 'D6M4A'                                         
315000        MOVE '19064' TO W-IDLEVNR-ALPHA  END-IF                           
315100     IF W-IDLEVNR-ALPHA = 'BQCTA'                                         
315200        MOVE '19535' TO W-IDLEVNR-ALPHA  END-IF                           
315300     IF W-IDLEVNR-ALPHA = 'R14MA'                                         
315400        MOVE '19610' TO W-IDLEVNR-ALPHA  END-IF                           
315500     IF W-IDLEVNR-ALPHA = 'BQYGA'                                         
315600        MOVE '22   ' TO W-IDLEVNR-ALPHA  END-IF                           
315700     IF W-IDLEVNR-ALPHA = 'BLTLA'                                         
315800        MOVE '2591 ' TO W-IDLEVNR-ALPHA  END-IF                           
315900     IF W-IDLEVNR-ALPHA = 'F4P6B'                                         
316000        MOVE '4445 ' TO W-IDLEVNR-ALPHA  END-IF                           
316100     IF W-IDLEVNR-ALPHA = 'CN5CA'                                         
316200        MOVE '4893 ' TO W-IDLEVNR-ALPHA  END-IF                           
316300     IF W-IDLEVNR-ALPHA = 'BWMZA'                                         
316400        MOVE '7630 ' TO W-IDLEVNR-ALPHA  END-IF                           
316500     IF W-IDLEVNR-ALPHA = 'CFT2B'                                         
316600        MOVE '7949 ' TO W-IDLEVNR-ALPHA  END-IF                           
316700**********************************************                            
316800     IF W-IDLEVNR-ALPHA = 'BWS5A'                                         
316900        MOVE '132  ' TO W-IDLEVNR-ALPHA  END-IF                           
317000     IF W-IDLEVNR-ALPHA = 'BMBQA'                                         
317100        MOVE '226  ' TO W-IDLEVNR-ALPHA  END-IF                           
317200     IF W-IDLEVNR-ALPHA = 'BKFVA'                                         
317300        MOVE '346  ' TO W-IDLEVNR-ALPHA  END-IF                           
317400     IF W-IDLEVNR-ALPHA = 'BWS9A'                                         
317500        MOVE '350  ' TO W-IDLEVNR-ALPHA  END-IF                           
317600     IF W-IDLEVNR-ALPHA = 'R9KSA'                                         
317700        MOVE '500  ' TO W-IDLEVNR-ALPHA  END-IF                           
317800     IF W-IDLEVNR-ALPHA = 'BQ2MA'                                         
317900        MOVE '1362 ' TO W-IDLEVNR-ALPHA  END-IF                           
318000     IF W-IDLEVNR-ALPHA = 'BQ2RA'                                         
318100        MOVE '1389 ' TO W-IDLEVNR-ALPHA  END-IF                           
318200     IF W-IDLEVNR-ALPHA = 'BPW9A'                                         
318300        MOVE '1425 ' TO W-IDLEVNR-ALPHA  END-IF                           
318400     IF W-IDLEVNR-ALPHA = 'DDDPA'                                         
318500        MOVE '1594 ' TO W-IDLEVNR-ALPHA  END-IF                           
318600     IF W-IDLEVNR-ALPHA = 'BQ3BA'                                         
318700        MOVE '1603 ' TO W-IDLEVNR-ALPHA  END-IF                           
318800     IF W-IDLEVNR-ALPHA = 'CFNKA'                                         
318900        MOVE '1675 ' TO W-IDLEVNR-ALPHA  END-IF                           
319000     IF W-IDLEVNR-ALPHA = 'MNDLA'                                         
319100        MOVE '2299 ' TO W-IDLEVNR-ALPHA  END-IF                           
319200     IF W-IDLEVNR-ALPHA = 'BQ6AA'                                         
319300        MOVE '2507 ' TO W-IDLEVNR-ALPHA  END-IF                           
319400     IF W-IDLEVNR-ALPHA = 'CFN0A'                                         
319500        MOVE '3664 ' TO W-IDLEVNR-ALPHA  END-IF                           
319600     IF W-IDLEVNR-ALPHA = 'CFN9A'                                         
319700        MOVE '3718 ' TO W-IDLEVNR-ALPHA  END-IF                           
319800     IF W-IDLEVNR-ALPHA = 'C96AA'                                         
319900        MOVE '5183 ' TO W-IDLEVNR-ALPHA  END-IF                           
320000     IF W-IDLEVNR-ALPHA = 'D0RED'                                         
320100        MOVE '6323 ' TO W-IDLEVNR-ALPHA  END-IF                           
320200     IF W-IDLEVNR-ALPHA = 'D0REB'                                         
320300        MOVE '6704 ' TO W-IDLEVNR-ALPHA  END-IF                           
320400     IF W-IDLEVNR-ALPHA = 'D0REA'                                         
320500        MOVE '6770 ' TO W-IDLEVNR-ALPHA  END-IF                           
320600     IF W-IDLEVNR-ALPHA = 'BUA7A'                                         
320700        MOVE '10108' TO W-IDLEVNR-ALPHA  END-IF                           
320800     IF W-IDLEVNR-ALPHA = 'AGPBA'                                         
320900        MOVE '14829' TO W-IDLEVNR-ALPHA  END-IF                           
321000*************************************************                         
321100     IF W-IDLEVNR-ALPHA = 'BYLRA'                                         
321200        MOVE '839  ' TO W-IDLEVNR-ALPHA  END-IF                           
321300     IF W-IDLEVNR-ALPHA = 'BQ2DA'                                         
321400        MOVE '1100 ' TO W-IDLEVNR-ALPHA  END-IF                           
321500     IF W-IDLEVNR-ALPHA = 'BKWRA'                                         
321600        MOVE '1205 ' TO W-IDLEVNR-ALPHA  END-IF                           
321700     IF W-IDLEVNR-ALPHA = 'BQ2HA'                                         
321800        MOVE '1285 ' TO W-IDLEVNR-ALPHA  END-IF                           
321900     IF W-IDLEVNR-ALPHA = 'N81NA'                                         
322000        MOVE '1336 ' TO W-IDLEVNR-ALPHA  END-IF                           
322100     IF W-IDLEVNR-ALPHA = 'S51YA'                                         
322200        MOVE '1605 ' TO W-IDLEVNR-ALPHA  END-IF                           
322300     IF W-IDLEVNR-ALPHA = 'AHTXA'                                         
322400        MOVE '3948 ' TO W-IDLEVNR-ALPHA  END-IF                           
322500     IF W-IDLEVNR-ALPHA = 'R500F'                                         
322600        MOVE '4034 ' TO W-IDLEVNR-ALPHA  END-IF                           
322700     IF W-IDLEVNR-ALPHA = 'K4UKA'                                         
322800        MOVE '4937 ' TO W-IDLEVNR-ALPHA  END-IF                           
322900     IF W-IDLEVNR-ALPHA = 'S3ULA'                                         
323000        MOVE '4979 ' TO W-IDLEVNR-ALPHA  END-IF                           
323100     IF W-IDLEVNR-ALPHA = 'C9F8A'                                         
323200        MOVE '5012 ' TO W-IDLEVNR-ALPHA  END-IF                           
323300     IF W-IDLEVNR-ALPHA = 'CFN1A'                                         
323400        MOVE '5145 ' TO W-IDLEVNR-ALPHA  END-IF                           
323500     IF W-IDLEVNR-ALPHA = 'BVNUA'                                         
323600        MOVE '5197 ' TO W-IDLEVNR-ALPHA  END-IF                           
323700     IF W-IDLEVNR-ALPHA = 'BQ7BA'                                         
323800        MOVE '5356 ' TO W-IDLEVNR-ALPHA  END-IF                           
323900     IF W-IDLEVNR-ALPHA = 'T727Z'                                         
324000        MOVE '5645 ' TO W-IDLEVNR-ALPHA  END-IF                           
324100     IF W-IDLEVNR-ALPHA = 'U494Q'                                         
324200        MOVE '5868 ' TO W-IDLEVNR-ALPHA  END-IF                           
324300     IF W-IDLEVNR-ALPHA = 'D35VA'                                         
324400        MOVE '6543 ' TO W-IDLEVNR-ALPHA  END-IF                           
324500     IF W-IDLEVNR-ALPHA = 'FNT8A'                                         
324600        MOVE '6985 ' TO W-IDLEVNR-ALPHA  END-IF                           
324700     IF W-IDLEVNR-ALPHA = 'CFT0A'                                         
324800        MOVE '7139 ' TO W-IDLEVNR-ALPHA  END-IF                           
324900     IF W-IDLEVNR-ALPHA = 'CFT8A'                                         
325000        MOVE '8187 ' TO W-IDLEVNR-ALPHA  END-IF                           
325100     IF W-IDLEVNR-ALPHA = 'K4UKB'                                         
325200        MOVE '11577' TO W-IDLEVNR-ALPHA  END-IF                           
325300     IF W-IDLEVNR-ALPHA = 'H387D'                                         
325400        MOVE '14985' TO W-IDLEVNR-ALPHA  END-IF                           
325500     IF W-IDLEVNR-ALPHA = 'S12HA'                                         
325600        MOVE '16076' TO W-IDLEVNR-ALPHA  END-IF                           
325700     IF W-IDLEVNR-ALPHA = 'BPXJA'                                         
325800        MOVE '16265' TO W-IDLEVNR-ALPHA  END-IF                           
325900     IF W-IDLEVNR-ALPHA = 'CFT8B'                                         
326000        MOVE '22410' TO W-IDLEVNR-ALPHA  END-IF                           
326100     IF W-IDLEVNR-ALPHA = 'C9G6A'                                         
326200        MOVE '24030' TO W-IDLEVNR-ALPHA  END-IF                           
326300     IF W-IDLEVNR-ALPHA = 'S12HE'                                         
326400        MOVE '24331' TO W-IDLEVNR-ALPHA  END-IF                           
326500*************************************************                         
326600*************************************************                         
326700     IF W-IDLEVNR-ALPHA = 'BQ1FA'                                         
326800        MOVE '640  ' TO W-IDLEVNR-ALPHA  END-IF                           
326900     IF W-IDLEVNR-ALPHA = 'BK3DA'                                         
327000        MOVE '813  ' TO W-IDLEVNR-ALPHA  END-IF                           
327100     IF W-IDLEVNR-ALPHA = 'N81FA'                                         
327200        MOVE '836  ' TO W-IDLEVNR-ALPHA  END-IF                           
327300     IF W-IDLEVNR-ALPHA = 'BQ1JA'                                         
327400        MOVE '845  ' TO W-IDLEVNR-ALPHA  END-IF                           
327500     IF W-IDLEVNR-ALPHA = 'BQ1KA'                                         
327600        MOVE '850  ' TO W-IDLEVNR-ALPHA  END-IF                           
327700     IF W-IDLEVNR-ALPHA = 'BQ1LA'                                         
327800        MOVE '861  ' TO W-IDLEVNR-ALPHA  END-IF                           
327900     IF W-IDLEVNR-ALPHA = 'BHFCA'                                         
328000        MOVE '927  ' TO W-IDLEVNR-ALPHA  END-IF                           
328100     IF W-IDLEVNR-ALPHA = 'BKCKA'                                         
328200        MOVE '930  ' TO W-IDLEVNR-ALPHA  END-IF                           
328300     IF W-IDLEVNR-ALPHA = 'N81JA'                                         
328400        MOVE '933  ' TO W-IDLEVNR-ALPHA  END-IF                           
328500     IF W-IDLEVNR-ALPHA = 'BQ2XA'                                         
328600        MOVE '1326 ' TO W-IDLEVNR-ALPHA  END-IF                           
328700     IF W-IDLEVNR-ALPHA = 'BLU5A'                                         
328800        MOVE '1659 ' TO W-IDLEVNR-ALPHA  END-IF                           
328900     IF W-IDLEVNR-ALPHA = 'BQ2YA'                                         
329000        MOVE '1977 ' TO W-IDLEVNR-ALPHA  END-IF                           
329100     IF W-IDLEVNR-ALPHA = 'CN5GA'                                         
329200        MOVE '7186 ' TO W-IDLEVNR-ALPHA  END-IF                           
329300     IF W-IDLEVNR-ALPHA = 'D0FTA'                                         
329400        MOVE '7208 ' TO W-IDLEVNR-ALPHA  END-IF                           
329500     IF W-IDLEVNR-ALPHA = 'FLZ6B'                                         
329600        MOVE '16154' TO W-IDLEVNR-ALPHA  END-IF                           
329700     IF W-IDLEVNR-ALPHA = 'S9GAA'                                         
329800        MOVE '18086' TO W-IDLEVNR-ALPHA  END-IF                           
329900*************************************************                         
330000     IF W-IDLEVNR-ALPHA = 'BWTDA'                                         
330100        MOVE '548  ' TO W-IDLEVNR-ALPHA  END-IF                           
330200     IF W-IDLEVNR-ALPHA = 'BWTFA'                                         
330300        MOVE '598  ' TO W-IDLEVNR-ALPHA  END-IF                           
330400     IF W-IDLEVNR-ALPHA = 'BQ1CA'                                         
330500        MOVE '605  ' TO W-IDLEVNR-ALPHA  END-IF                           
330600     IF W-IDLEVNR-ALPHA = 'DBHJA'                                         
330700        MOVE '667  ' TO W-IDLEVNR-ALPHA  END-IF                           
330800     IF W-IDLEVNR-ALPHA = 'BQ1MA'                                         
330900        MOVE '890  ' TO W-IDLEVNR-ALPHA  END-IF                           
331000     IF W-IDLEVNR-ALPHA = 'CD2JA'                                         
331100        MOVE '897  ' TO W-IDLEVNR-ALPHA  END-IF                           
331200     IF W-IDLEVNR-ALPHA = 'BLRQA'                                         
331300        MOVE '929  ' TO W-IDLEVNR-ALPHA  END-IF                           
331400     IF W-IDLEVNR-ALPHA = 'BQ2PA'                                         
331500        MOVE '1380 ' TO W-IDLEVNR-ALPHA  END-IF                           
331600     IF W-IDLEVNR-ALPHA = 'BQ6QB'                                         
331700        MOVE '1386 ' TO W-IDLEVNR-ALPHA  END-IF                           
331800     IF W-IDLEVNR-ALPHA = 'BSBZA'                                         
331900        MOVE '1560 ' TO W-IDLEVNR-ALPHA  END-IF                           
332000     IF W-IDLEVNR-ALPHA = 'BQ3EA'                                         
332100        MOVE '1728 ' TO W-IDLEVNR-ALPHA  END-IF                           
332200     IF W-IDLEVNR-ALPHA = 'BQ3NA'                                         
332300        MOVE '1736 ' TO W-IDLEVNR-ALPHA  END-IF                           
332400     IF W-IDLEVNR-ALPHA = 'BQ8AA'                                         
332500        MOVE '1783 ' TO W-IDLEVNR-ALPHA  END-IF                           
332600     IF W-IDLEVNR-ALPHA = 'BK6ZA'                                         
332700        MOVE '1809 ' TO W-IDLEVNR-ALPHA  END-IF                           
332800     IF W-IDLEVNR-ALPHA = 'BQ5MA'                                         
332900        MOVE '1978 ' TO W-IDLEVNR-ALPHA  END-IF                           
333000     IF W-IDLEVNR-ALPHA = 'BQ5NA'                                         
333100        MOVE '1982 ' TO W-IDLEVNR-ALPHA  END-IF                           
333200     IF W-IDLEVNR-ALPHA = 'S52HA'                                         
333300        MOVE '2087 ' TO W-IDLEVNR-ALPHA  END-IF                           
333400     IF W-IDLEVNR-ALPHA = 'CFNYA'                                         
333500        MOVE '3380 ' TO W-IDLEVNR-ALPHA  END-IF                           
333600     IF W-IDLEVNR-ALPHA = 'E622D'                                         
333700        MOVE '3654 ' TO W-IDLEVNR-ALPHA  END-IF                           
333800     IF W-IDLEVNR-ALPHA = 'D3D4A'                                         
333900        MOVE '6881 ' TO W-IDLEVNR-ALPHA  END-IF                           
334000     IF W-IDLEVNR-ALPHA = 'BQ7YC'                                         
334100        MOVE '7314 ' TO W-IDLEVNR-ALPHA  END-IF                           
334200     IF W-IDLEVNR-ALPHA = 'BQ7YB'                                         
334300        MOVE '7369 ' TO W-IDLEVNR-ALPHA  END-IF                           
334400     IF W-IDLEVNR-ALPHA = 'BQ7YA'                                         
334500        MOVE '7420 ' TO W-IDLEVNR-ALPHA  END-IF                           
334600     IF W-IDLEVNR-ALPHA = 'BPVBC'                                         
334700        MOVE '7881 ' TO W-IDLEVNR-ALPHA  END-IF                           
334800     IF W-IDLEVNR-ALPHA = 'BPVBB'                                         
334900        MOVE '10096' TO W-IDLEVNR-ALPHA  END-IF                           
335000     IF W-IDLEVNR-ALPHA = 'CFNZB'                                         
335100        MOVE '13691' TO W-IDLEVNR-ALPHA  END-IF                           
335200     IF W-IDLEVNR-ALPHA = 'U7SAD'                                         
335300        MOVE '14996' TO W-IDLEVNR-ALPHA  END-IF                           
335400     IF W-IDLEVNR-ALPHA = 'AN3AB'                                         
335500        MOVE '16226' TO W-IDLEVNR-ALPHA  END-IF                           
335600     IF W-IDLEVNR-ALPHA = 'U910A'                                         
335700        MOVE '24719' TO W-IDLEVNR-ALPHA  END-IF                           
335800     IF W-IDLEVNR-ALPHA = 'J5C2B'                                         
335900        MOVE '25016' TO W-IDLEVNR-ALPHA  END-IF                           
336000     IF W-IDLEVNR-ALPHA = 'LMJGA'                                         
336100        MOVE '25676' TO W-IDLEVNR-ALPHA  END-IF                           
336200     IF W-IDLEVNR-ALPHA = 'BPVBA'                                         
336300        MOVE '13445' TO W-IDLEVNR-ALPHA  END-IF                           
336400*************************************************                         
336500*************************************************                         
336600     IF W-IDLEVNR-ALPHA = 'BQ8ZD'                                         
336700        MOVE '93   ' TO W-IDLEVNR-ALPHA  END-IF                           
336800     IF W-IDLEVNR-ALPHA = 'BQ9CB'                                         
336900        MOVE '234  ' TO W-IDLEVNR-ALPHA  END-IF                           
337000     IF W-IDLEVNR-ALPHA = 'J2A6B'                                         
337100        MOVE '386  ' TO W-IDLEVNR-ALPHA  END-IF                           
337200     IF W-IDLEVNR-ALPHA = 'BQ8ZB'                                         
337300        MOVE '607  ' TO W-IDLEVNR-ALPHA  END-IF                           
337400     IF W-IDLEVNR-ALPHA = 'CXC6A'                                         
337500        MOVE '780  ' TO W-IDLEVNR-ALPHA  END-IF                           
337600     IF W-IDLEVNR-ALPHA = 'C7Q2D'                                         
337700        MOVE '835  ' TO W-IDLEVNR-ALPHA  END-IF                           
337800     IF W-IDLEVNR-ALPHA = 'BQ9AA'                                         
337900        MOVE '1187 ' TO W-IDLEVNR-ALPHA  END-IF                           
338000     IF W-IDLEVNR-ALPHA = 'C7Q2B'                                         
338100        MOVE '1228 ' TO W-IDLEVNR-ALPHA  END-IF                           
338200     IF W-IDLEVNR-ALPHA = 'BQ9CC'                                         
338300        MOVE '1393 ' TO W-IDLEVNR-ALPHA  END-IF                           
338400     IF W-IDLEVNR-ALPHA = 'BPU0A'                                         
338500        MOVE '2065 ' TO W-IDLEVNR-ALPHA  END-IF                           
338600     IF W-IDLEVNR-ALPHA = 'BQ5PA'                                         
338700        MOVE '2108 ' TO W-IDLEVNR-ALPHA  END-IF                           
338800     IF W-IDLEVNR-ALPHA = 'BPU0B'                                         
338900        MOVE '2157 ' TO W-IDLEVNR-ALPHA  END-IF                           
339000     IF W-IDLEVNR-ALPHA = 'BKVVA'                                         
339100        MOVE '2220 ' TO W-IDLEVNR-ALPHA  END-IF                           
339200     IF W-IDLEVNR-ALPHA = 'J2A6A'                                         
339300        MOVE '2288 ' TO W-IDLEVNR-ALPHA  END-IF                           
339400     IF W-IDLEVNR-ALPHA = 'BX0ZA'                                         
339500        MOVE '2289 ' TO W-IDLEVNR-ALPHA  END-IF                           
339600     IF W-IDLEVNR-ALPHA = 'BQ5RA'                                         
339700        MOVE '2315 ' TO W-IDLEVNR-ALPHA  END-IF                           
339800     IF W-IDLEVNR-ALPHA = 'D3F1A'                                         
339900        MOVE '2379 ' TO W-IDLEVNR-ALPHA  END-IF                           
340000     IF W-IDLEVNR-ALPHA = 'CXC6B'                                         
340100        MOVE '2545 ' TO W-IDLEVNR-ALPHA  END-IF                           
340200     IF W-IDLEVNR-ALPHA = 'D3F1B'                                         
340300        MOVE '2560 ' TO W-IDLEVNR-ALPHA  END-IF                           
340400     IF W-IDLEVNR-ALPHA = 'S053A'                                         
340500        MOVE '5033 ' TO W-IDLEVNR-ALPHA  END-IF                           
340600     IF W-IDLEVNR-ALPHA = 'P90CA'                                         
340700        MOVE '6664 ' TO W-IDLEVNR-ALPHA  END-IF                           
340800     IF W-IDLEVNR-ALPHA = 'BQ9CD'                                         
340900        MOVE '8294 ' TO W-IDLEVNR-ALPHA  END-IF                           
341000     IF W-IDLEVNR-ALPHA = 'BQ1QB'                                         
341100        MOVE '10220' TO W-IDLEVNR-ALPHA  END-IF                           
341200     IF W-IDLEVNR-ALPHA = 'BQ8UA'                                         
341300        MOVE '10374' TO W-IDLEVNR-ALPHA  END-IF                           
341400     IF W-IDLEVNR-ALPHA = 'N5MXB'                                         
341500        MOVE '14032' TO W-IDLEVNR-ALPHA  END-IF                           
341600     IF W-IDLEVNR-ALPHA = 'BQ9CE'                                         
341700        MOVE '19623' TO W-IDLEVNR-ALPHA  END-IF                           
341800     IF W-IDLEVNR-ALPHA = 'D3F1C'                                         
341900        MOVE '25979' TO W-IDLEVNR-ALPHA  END-IF                           
342000     IF W-IDLEVNR-ALPHA = 'D3F1D'                                         
342100        MOVE '25980' TO W-IDLEVNR-ALPHA  END-IF                           
342200     IF W-IDLEVNR-ALPHA = 'DMS1A'                                         
342300        MOVE '25923' TO W-IDLEVNR-ALPHA  END-IF                           
342400*************************************************                         
342500*************************************************                         
342600     IF W-IDLEVNR-ALPHA = 'BHFAA'                                         
342700        MOVE '4    ' TO W-IDLEVNR-ALPHA  END-IF                           
342800     IF W-IDLEVNR-ALPHA = 'CFM6A'                                         
342900        MOVE '32   ' TO W-IDLEVNR-ALPHA  END-IF                           
343000     IF W-IDLEVNR-ALPHA = 'BKR3A'                                         
343100        MOVE '64   ' TO W-IDLEVNR-ALPHA  END-IF                           
343200     IF W-IDLEVNR-ALPHA = 'CL3WA'                                         
343300        MOVE '75   ' TO W-IDLEVNR-ALPHA  END-IF                           
343400     IF W-IDLEVNR-ALPHA = 'BJPKA'                                         
343500        MOVE '81   ' TO W-IDLEVNR-ALPHA  END-IF                           
343600     IF W-IDLEVNR-ALPHA = 'CL3ZA'                                         
343700        MOVE '682  ' TO W-IDLEVNR-ALPHA  END-IF                           
343800     IF W-IDLEVNR-ALPHA = 'CXN9A'                                         
343900        MOVE '777  ' TO W-IDLEVNR-ALPHA  END-IF                           
344000     IF W-IDLEVNR-ALPHA = 'BLRUA'                                         
344100        MOVE '1952 ' TO W-IDLEVNR-ALPHA  END-IF                           
344200     IF W-IDLEVNR-ALPHA = 'BQ5UA'                                         
344300        MOVE '2387 ' TO W-IDLEVNR-ALPHA  END-IF                           
344400     IF W-IDLEVNR-ALPHA = 'BQ5YA'                                         
344500        MOVE '2437 ' TO W-IDLEVNR-ALPHA  END-IF                           
344600     IF W-IDLEVNR-ALPHA = 'BJR1A'                                         
344700        MOVE '2483 ' TO W-IDLEVNR-ALPHA  END-IF                           
344800     IF W-IDLEVNR-ALPHA = 'BQ5ZA'                                         
344900        MOVE '2495 ' TO W-IDLEVNR-ALPHA  END-IF                           
345000     IF W-IDLEVNR-ALPHA = 'BQ6BA'                                         
345100        MOVE '2539 ' TO W-IDLEVNR-ALPHA  END-IF                           
345200     IF W-IDLEVNR-ALPHA = 'CZ1SA'                                         
345300        MOVE '3730 ' TO W-IDLEVNR-ALPHA  END-IF                           
345400     IF W-IDLEVNR-ALPHA = 'F842A'                                         
345500        MOVE '4961 ' TO W-IDLEVNR-ALPHA  END-IF                           
345600     IF W-IDLEVNR-ALPHA = 'P7ZFA'                                         
345700        MOVE '4969 ' TO W-IDLEVNR-ALPHA  END-IF                           
345800     IF W-IDLEVNR-ALPHA = 'C9B7B'                                         
345900        MOVE '6757 ' TO W-IDLEVNR-ALPHA  END-IF                           
346000     IF W-IDLEVNR-ALPHA = 'BQ5VA'                                         
346100        MOVE '10370' TO W-IDLEVNR-ALPHA  END-IF                           
346200     IF W-IDLEVNR-ALPHA = 'R2FLG'                                         
346300        MOVE '11096' TO W-IDLEVNR-ALPHA  END-IF                           
346400     IF W-IDLEVNR-ALPHA = 'BPUKA'                                         
346500        MOVE '13360' TO W-IDLEVNR-ALPHA  END-IF                           
346600     IF W-IDLEVNR-ALPHA = 'BPUMA'                                         
346700        MOVE '13538' TO W-IDLEVNR-ALPHA  END-IF                           
346800     IF W-IDLEVNR-ALPHA = 'E019A'                                         
346900        MOVE '50535' TO W-IDLEVNR-ALPHA  END-IF                           
347000*************************************************                         
347100*************************************************                         
347200     IF W-IDLEVNR-ALPHA = 'LCWLA'                                         
347300        MOVE '139  ' TO W-IDLEVNR-ALPHA  END-IF                           
347400     IF W-IDLEVNR-ALPHA = 'CN4TA'                                         
347500        MOVE '793  ' TO W-IDLEVNR-ALPHA  END-IF                           
347600     IF W-IDLEVNR-ALPHA = 'DL2QA'                                         
347700        MOVE '3911 ' TO W-IDLEVNR-ALPHA  END-IF                           
347800     IF W-IDLEVNR-ALPHA = 'CBVLA'                                         
347900        MOVE '808  ' TO W-IDLEVNR-ALPHA  END-IF                           
348000     IF W-IDLEVNR-ALPHA = 'BMBMA'                                         
348100        MOVE '855  ' TO W-IDLEVNR-ALPHA  END-IF                           
348200     IF W-IDLEVNR-ALPHA = 'BQ3AA'                                         
348300        MOVE '1566 ' TO W-IDLEVNR-ALPHA  END-IF                           
348400     IF W-IDLEVNR-ALPHA = 'CFNMA'                                         
348500        MOVE '1859 ' TO W-IDLEVNR-ALPHA  END-IF                           
348600     IF W-IDLEVNR-ALPHA = 'CBSDA'                                         
348700        MOVE '1863 ' TO W-IDLEVNR-ALPHA  END-IF                           
348800     IF W-IDLEVNR-ALPHA = 'BUF5A'                                         
348900        MOVE '1902 ' TO W-IDLEVNR-ALPHA  END-IF                           
349000     IF W-IDLEVNR-ALPHA = 'BQ6EA'                                         
349100        MOVE '2609 ' TO W-IDLEVNR-ALPHA  END-IF                           
349200     IF W-IDLEVNR-ALPHA = 'BLUDA'                                         
349300        MOVE '3034 ' TO W-IDLEVNR-ALPHA  END-IF                           
349400     IF W-IDLEVNR-ALPHA = 'BKJDA'                                         
349500        MOVE '3152 ' TO W-IDLEVNR-ALPHA  END-IF                           
349600     IF W-IDLEVNR-ALPHA = 'BL1UA'                                         
349700        MOVE '3167 ' TO W-IDLEVNR-ALPHA  END-IF                           
349800     IF W-IDLEVNR-ALPHA = 'BQ6RA'                                         
349900        MOVE '3370 ' TO W-IDLEVNR-ALPHA  END-IF                           
350000     IF W-IDLEVNR-ALPHA = 'D3U3A'                                         
350100        MOVE '3538 ' TO W-IDLEVNR-ALPHA  END-IF                           
350200     IF W-IDLEVNR-ALPHA = 'S0H5D'                                         
350300        MOVE '3669 ' TO W-IDLEVNR-ALPHA  END-IF                           
350400     IF W-IDLEVNR-ALPHA = 'G5FPC'                                         
350500        MOVE '3722 ' TO W-IDLEVNR-ALPHA  END-IF                           
350600     IF W-IDLEVNR-ALPHA = 'V136C'                                         
350700        MOVE '3770 ' TO W-IDLEVNR-ALPHA  END-IF                           
350800     IF W-IDLEVNR-ALPHA = 'P38NA'                                         
350900        MOVE '3988 ' TO W-IDLEVNR-ALPHA  END-IF                           
351000     IF W-IDLEVNR-ALPHA = 'G5FPD'                                         
351100        MOVE '3994 ' TO W-IDLEVNR-ALPHA  END-IF                           
351200     IF W-IDLEVNR-ALPHA = 'E623F'                                         
351300        MOVE '4880 ' TO W-IDLEVNR-ALPHA  END-IF                           
351400     IF W-IDLEVNR-ALPHA = 'D3U2A'                                         
351500        MOVE '4942 ' TO W-IDLEVNR-ALPHA  END-IF                           
351600     IF W-IDLEVNR-ALPHA = 'D3K6A'                                         
351700        MOVE '5314 ' TO W-IDLEVNR-ALPHA  END-IF                           
351800     IF W-IDLEVNR-ALPHA = 'D3L3A'                                         
351900        MOVE '5362 ' TO W-IDLEVNR-ALPHA  END-IF                           
352000     IF W-IDLEVNR-ALPHA = 'BQYEA'                                         
352100        MOVE '25982' TO W-IDLEVNR-ALPHA  END-IF                           
352200     IF W-IDLEVNR-ALPHA = 'V2NYA'                                         
352300        MOVE '25847' TO W-IDLEVNR-ALPHA  END-IF                           
352400     IF W-IDLEVNR-ALPHA = 'DRTHA'                                         
352500        MOVE '25996' TO W-IDLEVNR-ALPHA  END-IF                           
352600     IF W-IDLEVNR-ALPHA = 'D3D0A'                                         
352700        MOVE '6014 ' TO W-IDLEVNR-ALPHA  END-IF                           
352800     IF W-IDLEVNR-ALPHA = 'CRGJA'                                         
352900        MOVE '6101 ' TO W-IDLEVNR-ALPHA  END-IF                           
353000     IF W-IDLEVNR-ALPHA = 'C6S4A'                                         
353100        MOVE '6200 ' TO W-IDLEVNR-ALPHA  END-IF                           
353200     IF W-IDLEVNR-ALPHA = 'Q6QLA'                                         
353300        MOVE '6346 ' TO W-IDLEVNR-ALPHA  END-IF                           
353400     IF W-IDLEVNR-ALPHA = 'D25KA'                                         
353500        MOVE '6810 ' TO W-IDLEVNR-ALPHA  END-IF                           
353600     IF W-IDLEVNR-ALPHA = 'E623B'                                         
353700        MOVE '6842 ' TO W-IDLEVNR-ALPHA  END-IF                           
353800     IF W-IDLEVNR-ALPHA = 'T7WFA'                                         
353900        MOVE '6996 ' TO W-IDLEVNR-ALPHA  END-IF                           
354000     IF W-IDLEVNR-ALPHA = 'BP8JA'                                         
354100        MOVE '10135' TO W-IDLEVNR-ALPHA  END-IF                           
354200     IF W-IDLEVNR-ALPHA = 'BP8JE'                                         
354300        MOVE '10483' TO W-IDLEVNR-ALPHA  END-IF                           
354400     IF W-IDLEVNR-ALPHA = 'R5YYA'                                         
354500        MOVE '10488' TO W-IDLEVNR-ALPHA  END-IF                           
354600     IF W-IDLEVNR-ALPHA = 'BP8JC'                                         
354700        MOVE '10493' TO W-IDLEVNR-ALPHA  END-IF                           
354800     IF W-IDLEVNR-ALPHA = 'Q5EGB'                                         
354900        MOVE '10494' TO W-IDLEVNR-ALPHA  END-IF                           
355000     IF W-IDLEVNR-ALPHA = 'Q5EGB'                                         
355100        MOVE '10494' TO W-IDLEVNR-ALPHA  END-IF                           
355200     IF W-IDLEVNR-ALPHA = 'BQ6RB'                                         
355300        MOVE '13389' TO W-IDLEVNR-ALPHA  END-IF                           
355400     IF W-IDLEVNR-ALPHA = 'M9TMB'                                         
355500        MOVE '13578' TO W-IDLEVNR-ALPHA  END-IF                           
355600     IF W-IDLEVNR-ALPHA = 'F432J'                                         
355700        MOVE '13594' TO W-IDLEVNR-ALPHA  END-IF                           
355800     IF W-IDLEVNR-ALPHA = 'D07LG'                                         
355900        MOVE '13612' TO W-IDLEVNR-ALPHA  END-IF                           
356000     IF W-IDLEVNR-ALPHA = 'D07LB'                                         
356100        MOVE '13614' TO W-IDLEVNR-ALPHA  END-IF                           
356200     IF W-IDLEVNR-ALPHA = 'CR9GA'                                         
356300        MOVE '13741' TO W-IDLEVNR-ALPHA  END-IF                           
356400     IF W-IDLEVNR-ALPHA = 'AN1JE'                                         
356500        MOVE '13793' TO W-IDLEVNR-ALPHA  END-IF                           
356600     IF W-IDLEVNR-ALPHA = 'BQJFA'                                         
356700        MOVE '15451' TO W-IDLEVNR-ALPHA  END-IF                           
356800     IF W-IDLEVNR-ALPHA = 'D25KD'                                         
356900        MOVE '16004' TO W-IDLEVNR-ALPHA  END-IF                           
357000     IF W-IDLEVNR-ALPHA = 'D25KE'                                         
357100        MOVE '16005' TO W-IDLEVNR-ALPHA  END-IF                           
357200     IF W-IDLEVNR-ALPHA = 'E623C'                                         
357300        MOVE '16039' TO W-IDLEVNR-ALPHA  END-IF                           
357400     IF W-IDLEVNR-ALPHA = 'B41YA'                                         
357500        MOVE '16080' TO W-IDLEVNR-ALPHA  END-IF                           
357600     IF W-IDLEVNR-ALPHA = 'E623E'                                         
357700        MOVE '16268' TO W-IDLEVNR-ALPHA  END-IF                           
357800     IF W-IDLEVNR-ALPHA = 'EKM4A'                                         
357900        MOVE '16403' TO W-IDLEVNR-ALPHA  END-IF                           
358000     IF W-IDLEVNR-ALPHA = 'BP8JD'                                         
358100        MOVE '17715' TO W-IDLEVNR-ALPHA  END-IF                           
358200     IF W-IDLEVNR-ALPHA = 'BKPTA'                                         
358300        MOVE '18203' TO W-IDLEVNR-ALPHA  END-IF                           
358400     IF W-IDLEVNR-ALPHA = 'ANEBA'                                         
358500        MOVE '19907' TO W-IDLEVNR-ALPHA  END-IF                           
358600     IF W-IDLEVNR-ALPHA = 'C72DC'                                         
358700        MOVE '22292' TO W-IDLEVNR-ALPHA  END-IF                           
358800     IF W-IDLEVNR-ALPHA = 'DDC3A'                                         
358900        MOVE '25729' TO W-IDLEVNR-ALPHA  END-IF                           
359000     IF W-IDLEVNR-ALPHA = 'AUJ7B'                                         
359100        MOVE '25749' TO W-IDLEVNR-ALPHA  END-IF                           
359200     IF W-IDLEVNR-ALPHA = 'AUJ7C'                                         
359300        MOVE '25846' TO W-IDLEVNR-ALPHA  END-IF                           
359400     IF W-IDLEVNR-ALPHA = 'DNK9A'                                         
359500        MOVE '25931' TO W-IDLEVNR-ALPHA  END-IF                           
359600*************************************************                         
359700**********************************************                            
359800     IF W-IDLEVNR-ALPHA = 'BQEFA'                                         
359900        MOVE '29   ' TO W-IDLEVNR-ALPHA  END-IF                           
360000     IF W-IDLEVNR-ALPHA = 'BQZ7A'                                         
360100        MOVE '280  ' TO W-IDLEVNR-ALPHA  END-IF                           
360200     IF W-IDLEVNR-ALPHA = 'BQZ9A'                                         
360300        MOVE '320  ' TO W-IDLEVNR-ALPHA  END-IF                           
360400     IF W-IDLEVNR-ALPHA = 'BQ0DA'                                         
360500        MOVE '372  ' TO W-IDLEVNR-ALPHA  END-IF                           
360600     IF W-IDLEVNR-ALPHA = 'BQ0EA'                                         
360700        MOVE '495  ' TO W-IDLEVNR-ALPHA  END-IF                           
360800     IF W-IDLEVNR-ALPHA = 'BQ6K1'                                         
360900        MOVE '579  ' TO W-IDLEVNR-ALPHA  END-IF                           
361000     IF W-IDLEVNR-ALPHA = 'CFNEA'                                         
361100        MOVE '710  ' TO W-IDLEVNR-ALPHA  END-IF                           
361200     IF W-IDLEVNR-ALPHA = 'BQ5LA'                                         
361300        MOVE '1912 ' TO W-IDLEVNR-ALPHA  END-IF                           
361400     IF W-IDLEVNR-ALPHA = 'CN4WA'                                         
361500        MOVE '1944 ' TO W-IDLEVNR-ALPHA  END-IF                           
361600     IF W-IDLEVNR-ALPHA = 'CW6BA'                                         
361700        MOVE '2020 ' TO W-IDLEVNR-ALPHA  END-IF                           
361800     IF W-IDLEVNR-ALPHA = 'C6T3A'                                         
361900        MOVE '2351 ' TO W-IDLEVNR-ALPHA  END-IF                           
362000     IF W-IDLEVNR-ALPHA = 'CFNSA'                                         
362100        MOVE '2410 ' TO W-IDLEVNR-ALPHA  END-IF                           
362200     IF W-IDLEVNR-ALPHA = 'P112B'                                         
362300        MOVE '3559 ' TO W-IDLEVNR-ALPHA  END-IF                           
362400     IF W-IDLEVNR-ALPHA = 'C75RA'                                         
362500        MOVE '3606 ' TO W-IDLEVNR-ALPHA  END-IF                           
362600     IF W-IDLEVNR-ALPHA = 'P112L'                                         
362700        MOVE '3705 ' TO W-IDLEVNR-ALPHA  END-IF                           
362800     IF W-IDLEVNR-ALPHA = 'R7NAB'                                         
362900        MOVE '3865 ' TO W-IDLEVNR-ALPHA  END-IF                           
363000     IF W-IDLEVNR-ALPHA = 'B2N4A'                                         
363100        MOVE '4964 ' TO W-IDLEVNR-ALPHA  END-IF                           
363200     IF W-IDLEVNR-ALPHA = 'C8Z7A'                                         
363300        MOVE '6605 ' TO W-IDLEVNR-ALPHA  END-IF                           
363400     IF W-IDLEVNR-ALPHA = 'D0U2A'                                         
363500        MOVE '6765 ' TO W-IDLEVNR-ALPHA  END-IF                           
363600     IF W-IDLEVNR-ALPHA = 'C97GA'                                         
363700        MOVE '6947 ' TO W-IDLEVNR-ALPHA  END-IF                           
363800     IF W-IDLEVNR-ALPHA = 'AZJLA'                                         
363900        MOVE '11148' TO W-IDLEVNR-ALPHA  END-IF                           
364000     IF W-IDLEVNR-ALPHA = 'K0R6F'                                         
364100        MOVE '11326' TO W-IDLEVNR-ALPHA  END-IF                           
364200     IF W-IDLEVNR-ALPHA = 'Q18RA'                                         
364300        MOVE '13382' TO W-IDLEVNR-ALPHA  END-IF                           
364400     IF W-IDLEVNR-ALPHA = 'P112D'                                         
364500        MOVE '13540' TO W-IDLEVNR-ALPHA  END-IF                           
364600     IF W-IDLEVNR-ALPHA = 'P112M'                                         
364700        MOVE '13541' TO W-IDLEVNR-ALPHA  END-IF                           
364800     IF W-IDLEVNR-ALPHA = 'T0CJA'                                         
364900        MOVE '13548' TO W-IDLEVNR-ALPHA  END-IF                           
365000     IF W-IDLEVNR-ALPHA = 'C75RB'                                         
365100        MOVE '13579' TO W-IDLEVNR-ALPHA  END-IF                           
365200     IF W-IDLEVNR-ALPHA = 'K0R6G'                                         
365300        MOVE '13587' TO W-IDLEVNR-ALPHA  END-IF                           
365400     IF W-IDLEVNR-ALPHA = 'BP8HB'                                         
365500        MOVE '14621' TO W-IDLEVNR-ALPHA  END-IF                           
365600     IF W-IDLEVNR-ALPHA = 'D0U2B'                                         
365700        MOVE '16172' TO W-IDLEVNR-ALPHA  END-IF                           
365800     IF W-IDLEVNR-ALPHA = 'D17KA'                                         
365900        MOVE '19255' TO W-IDLEVNR-ALPHA  END-IF                           
366000     IF W-IDLEVNR-ALPHA = 'R76JA'                                         
366100        MOVE '25936' TO W-IDLEVNR-ALPHA  END-IF                           
366200     IF W-IDLEVNR-ALPHA = 'D26QC'                                         
366300        MOVE '25937' TO W-IDLEVNR-ALPHA  END-IF                           
366400     IF W-IDLEVNR-ALPHA = 'BP8HA'                                         
366500        MOVE '34621' TO W-IDLEVNR-ALPHA  END-IF                           
366600*************************************************                         
366700**********************************************                            
366800     IF W-IDLEVNR-ALPHA = 'BJWQA'                                         
366900        MOVE '354  ' TO W-IDLEVNR-ALPHA  END-IF                           
367000     IF W-IDLEVNR-ALPHA = 'BQ3DA'                                         
367100        MOVE '1612 ' TO W-IDLEVNR-ALPHA  END-IF                           
367200     IF W-IDLEVNR-ALPHA = 'CFNLA'                                         
367300        MOVE '1755 ' TO W-IDLEVNR-ALPHA  END-IF                           
367400     IF W-IDLEVNR-ALPHA = 'C96UD'                                         
367500        MOVE '1800 ' TO W-IDLEVNR-ALPHA  END-IF                           
367600     IF W-IDLEVNR-ALPHA = 'BK9KA'                                         
367700        MOVE '2248 ' TO W-IDLEVNR-ALPHA  END-IF                           
367800     IF W-IDLEVNR-ALPHA = 'GPR5A'                                         
367900        MOVE '5425 ' TO W-IDLEVNR-ALPHA  END-IF                           
368000     IF W-IDLEVNR-ALPHA = 'Q0ERA'                                         
368100        MOVE '5489 ' TO W-IDLEVNR-ALPHA  END-IF                           
368200     IF W-IDLEVNR-ALPHA = 'AHHSA'                                         
368300        MOVE '5679 ' TO W-IDLEVNR-ALPHA  END-IF                           
368400     IF W-IDLEVNR-ALPHA = 'S2ZLA'                                         
368500        MOVE '6756 ' TO W-IDLEVNR-ALPHA  END-IF                           
368600     IF W-IDLEVNR-ALPHA = 'C7B1A'                                         
368700        MOVE '6775 ' TO W-IDLEVNR-ALPHA  END-IF                           
368800     IF W-IDLEVNR-ALPHA = 'D2S2A'                                         
368900        MOVE '6795 ' TO W-IDLEVNR-ALPHA  END-IF                           
369000     IF W-IDLEVNR-ALPHA = 'D23YA'                                         
369100        MOVE '6807 ' TO W-IDLEVNR-ALPHA  END-IF                           
369200     IF W-IDLEVNR-ALPHA = 'BQ7PA'                                         
369300        MOVE '7218 ' TO W-IDLEVNR-ALPHA  END-IF                           
369400     IF W-IDLEVNR-ALPHA = 'CDHSA'                                         
369500        MOVE '7757 ' TO W-IDLEVNR-ALPHA  END-IF                           
369600     IF W-IDLEVNR-ALPHA = 'D23YB'                                         
369700        MOVE '7922 ' TO W-IDLEVNR-ALPHA  END-IF                           
369800     IF W-IDLEVNR-ALPHA = 'P8C8A'                                         
369900        MOVE '14615' TO W-IDLEVNR-ALPHA  END-IF                           
370000     IF W-IDLEVNR-ALPHA = 'N2M5A'                                         
370100        MOVE '16088' TO W-IDLEVNR-ALPHA  END-IF                           
370200     IF W-IDLEVNR-ALPHA = 'DAFTA'                                         
370300        MOVE '16171' TO W-IDLEVNR-ALPHA  END-IF                           
370400     IF W-IDLEVNR-ALPHA = 'BQ9VA'                                         
370500        MOVE '16388' TO W-IDLEVNR-ALPHA  END-IF                           
370600     IF W-IDLEVNR-ALPHA = 'BHZ3A'                                         
370700        MOVE '18688' TO W-IDLEVNR-ALPHA  END-IF                           
370800     IF W-IDLEVNR-ALPHA = 'V4FVA'                                         
370900        MOVE '19454' TO W-IDLEVNR-ALPHA  END-IF                           
371000     IF W-IDLEVNR-ALPHA = 'S3JJA'                                         
371100        MOVE '19455' TO W-IDLEVNR-ALPHA  END-IF                           
371200     IF W-IDLEVNR-ALPHA = 'BQ7PB'                                         
371300        MOVE '24065' TO W-IDLEVNR-ALPHA  END-IF                           
371400     IF W-IDLEVNR-ALPHA = 'BQ7RB'                                         
371500        MOVE '24066' TO W-IDLEVNR-ALPHA  END-IF                           
371600     IF W-IDLEVNR-ALPHA = 'G13FC'                                         
371700        MOVE '24078' TO W-IDLEVNR-ALPHA  END-IF                           
371800     IF W-IDLEVNR-ALPHA = 'BQ7RC'                                         
371900        MOVE '25752' TO W-IDLEVNR-ALPHA  END-IF                           
372000*************************************************                         
372100     IF W-IDLEVNR-ALPHA = 'BMLWA'                                         
372200        MOVE '143  ' TO W-IDLEVNR-ALPHA  END-IF                           
372300     IF W-IDLEVNR-ALPHA = 'BQ1NA'                                         
372400        MOVE '934  ' TO W-IDLEVNR-ALPHA  END-IF                           
372500     IF W-IDLEVNR-ALPHA = 'BPUYA'                                         
372600        MOVE '1008 ' TO W-IDLEVNR-ALPHA  END-IF                           
372700     IF W-IDLEVNR-ALPHA = 'BKRZA'                                         
372800        MOVE '1062 ' TO W-IDLEVNR-ALPHA  END-IF                           
372900     IF W-IDLEVNR-ALPHA = 'S34XF'                                         
373000        MOVE '1134 ' TO W-IDLEVNR-ALPHA  END-IF                           
373100     IF W-IDLEVNR-ALPHA = 'BKDQA'                                         
373200        MOVE '1196 ' TO W-IDLEVNR-ALPHA  END-IF                           
373300     IF W-IDLEVNR-ALPHA = 'U7ABB'                                         
373400        MOVE '1244 ' TO W-IDLEVNR-ALPHA  END-IF                           
373500     IF W-IDLEVNR-ALPHA = 'U7ABC'                                         
373600        MOVE '1269 ' TO W-IDLEVNR-ALPHA  END-IF                           
373700     IF W-IDLEVNR-ALPHA = 'BSKYA'                                         
373800        MOVE '1449 ' TO W-IDLEVNR-ALPHA  END-IF                           
373900     IF W-IDLEVNR-ALPHA = 'BKMMA'                                         
374000        MOVE '1662 ' TO W-IDLEVNR-ALPHA  END-IF                           
374100     IF W-IDLEVNR-ALPHA = 'U7ABA'                                         
374200        MOVE '1847 ' TO W-IDLEVNR-ALPHA  END-IF                           
374300     IF W-IDLEVNR-ALPHA = 'S34XD'                                         
374400        MOVE '2500 ' TO W-IDLEVNR-ALPHA  END-IF                           
374500     IF W-IDLEVNR-ALPHA = 'BQAJA'                                         
374600        MOVE '2552 ' TO W-IDLEVNR-ALPHA  END-IF                           
374700     IF W-IDLEVNR-ALPHA = 'BWKGA'                                         
374800        MOVE '2553 ' TO W-IDLEVNR-ALPHA  END-IF                           
374900     IF W-IDLEVNR-ALPHA = 'BWTCA'                                         
375000        MOVE '2558 ' TO W-IDLEVNR-ALPHA  END-IF                           
375100     IF W-IDLEVNR-ALPHA = 'CFNVA'                                         
375200        MOVE '2633 ' TO W-IDLEVNR-ALPHA  END-IF                           
375300     IF W-IDLEVNR-ALPHA = 'BQAJB'                                         
375400        MOVE '2684 ' TO W-IDLEVNR-ALPHA  END-IF                           
375500     IF W-IDLEVNR-ALPHA = 'BWKSA'                                         
375600        MOVE '3054 ' TO W-IDLEVNR-ALPHA  END-IF                           
375700     IF W-IDLEVNR-ALPHA = 'CN4YA'                                         
375800        MOVE '3165 ' TO W-IDLEVNR-ALPHA  END-IF                           
375900     IF W-IDLEVNR-ALPHA = 'CFNWA'                                         
376000        MOVE '3197 ' TO W-IDLEVNR-ALPHA  END-IF                           
376100     IF W-IDLEVNR-ALPHA = 'D3C5A'                                         
376200        MOVE '3468 ' TO W-IDLEVNR-ALPHA  END-IF                           
376300     IF W-IDLEVNR-ALPHA = 'BP9ZA'                                         
376400        MOVE '3558 ' TO W-IDLEVNR-ALPHA  END-IF                           
376500     IF W-IDLEVNR-ALPHA = 'Q6TFA'                                         
376600        MOVE '3662 ' TO W-IDLEVNR-ALPHA  END-IF                           
376700     IF W-IDLEVNR-ALPHA = 'D5H4A'                                         
376800        MOVE '3807 ' TO W-IDLEVNR-ALPHA  END-IF                           
376900     IF W-IDLEVNR-ALPHA = 'BQ6VA'                                         
377000        MOVE '3982 ' TO W-IDLEVNR-ALPHA  END-IF                           
377100     IF W-IDLEVNR-ALPHA = 'AHMPA'                                         
377200        MOVE '4172 ' TO W-IDLEVNR-ALPHA  END-IF                           
377300     IF W-IDLEVNR-ALPHA = 'BPFNA'                                         
377400        MOVE '4721 ' TO W-IDLEVNR-ALPHA  END-IF                           
377500     IF W-IDLEVNR-ALPHA = 'ATNNA'                                         
377600        MOVE '4724 ' TO W-IDLEVNR-ALPHA  END-IF                           
377700     IF W-IDLEVNR-ALPHA = 'BCJSA'                                         
377800        MOVE '4988 ' TO W-IDLEVNR-ALPHA  END-IF                           
377900     IF W-IDLEVNR-ALPHA = 'C7L2A'                                         
378000        MOVE '5281 ' TO W-IDLEVNR-ALPHA  END-IF                           
378100     IF W-IDLEVNR-ALPHA = 'E3B2B'                                         
378200        MOVE '6305 ' TO W-IDLEVNR-ALPHA  END-IF                           
378300     IF W-IDLEVNR-ALPHA = 'C68JA'                                         
378400        MOVE '6350 ' TO W-IDLEVNR-ALPHA  END-IF                           
378500     IF W-IDLEVNR-ALPHA = 'D0MGA'                                         
378600        MOVE '6554 ' TO W-IDLEVNR-ALPHA  END-IF                           
378700     IF W-IDLEVNR-ALPHA = 'BPX3B'                                         
378800        MOVE '6555 ' TO W-IDLEVNR-ALPHA  END-IF                           
378900     IF W-IDLEVNR-ALPHA = 'C8T1A'                                         
379000        MOVE '6556 ' TO W-IDLEVNR-ALPHA  END-IF                           
379100     IF W-IDLEVNR-ALPHA = 'C68JC'                                         
379200        MOVE '6597 ' TO W-IDLEVNR-ALPHA  END-IF                           
379300     IF W-IDLEVNR-ALPHA = 'G944E'                                         
379400        MOVE '6911 ' TO W-IDLEVNR-ALPHA  END-IF                           
379500     IF W-IDLEVNR-ALPHA = 'CDNXA'                                         
379600        MOVE '6958 ' TO W-IDLEVNR-ALPHA  END-IF                           
379700     IF W-IDLEVNR-ALPHA = 'D38KA'                                         
379800        MOVE '6961 ' TO W-IDLEVNR-ALPHA  END-IF                           
379900     IF W-IDLEVNR-ALPHA = 'D38KD'                                         
380000        MOVE '6962 ' TO W-IDLEVNR-ALPHA  END-IF                           
380100     IF W-IDLEVNR-ALPHA = 'MTSFA'                                         
380200        MOVE '7462 ' TO W-IDLEVNR-ALPHA  END-IF                           
380300     IF W-IDLEVNR-ALPHA = 'BP9ZC'                                         
380400        MOVE '13672' TO W-IDLEVNR-ALPHA  END-IF                           
380500     IF W-IDLEVNR-ALPHA = 'BP9ZB'                                         
380600        MOVE '13709' TO W-IDLEVNR-ALPHA  END-IF                           
380700     IF W-IDLEVNR-ALPHA = 'D38KF'                                         
380800        MOVE '16044' TO W-IDLEVNR-ALPHA  END-IF                           
380900     IF W-IDLEVNR-ALPHA = 'R7GNA'                                         
381000        MOVE '16092' TO W-IDLEVNR-ALPHA  END-IF                           
381100     IF W-IDLEVNR-ALPHA = 'T2GCC'                                         
381200        MOVE '16141' TO W-IDLEVNR-ALPHA  END-IF                           
381300     IF W-IDLEVNR-ALPHA = 'D0W5D'                                         
381400        MOVE '16142' TO W-IDLEVNR-ALPHA  END-IF                           
381500     IF W-IDLEVNR-ALPHA = 'BPX3C'                                         
381600        MOVE '16365' TO W-IDLEVNR-ALPHA  END-IF                           
381700     IF W-IDLEVNR-ALPHA = 'T2GCA'                                         
381800        MOVE '16406' TO W-IDLEVNR-ALPHA  END-IF                           
381900     IF W-IDLEVNR-ALPHA = 'AD3XA'                                         
382000        MOVE '21256' TO W-IDLEVNR-ALPHA  END-IF                           
382100     IF W-IDLEVNR-ALPHA = 'D38KG'                                         
382200        MOVE '25618' TO W-IDLEVNR-ALPHA  END-IF                           
382300     IF W-IDLEVNR-ALPHA = 'BVPDA'                                         
382400        MOVE '25920' TO W-IDLEVNR-ALPHA  END-IF                           
382500     IF W-IDLEVNR-ALPHA = 'C68JF'                                         
382600        MOVE '26350' TO W-IDLEVNR-ALPHA  END-IF                           
382700*************************************************                         
382800     IF W-IDLEVNR-ALPHA = 'BP8JB'                                         
382900        MOVE '912  ' TO W-IDLEVNR-ALPHA  END-IF                           
383000     IF W-IDLEVNR-ALPHA = 'BMJGA'                                         
383100        MOVE '1005 ' TO W-IDLEVNR-ALPHA  END-IF                           
383200     IF W-IDLEVNR-ALPHA = 'AY0CA'                                         
383300        MOVE '1720 ' TO W-IDLEVNR-ALPHA  END-IF                           
383400     IF W-IDLEVNR-ALPHA = 'BPUNA'                                         
383500        MOVE '2442 ' TO W-IDLEVNR-ALPHA  END-IF                           
383600     IF W-IDLEVNR-ALPHA = 'BKTXA'                                         
383700        MOVE '3074 ' TO W-IDLEVNR-ALPHA  END-IF                           
383800     IF W-IDLEVNR-ALPHA = 'BPA1A'                                         
383900        MOVE '3163 ' TO W-IDLEVNR-ALPHA  END-IF                           
384000     IF W-IDLEVNR-ALPHA = 'C62FA'                                         
384100        MOVE '3575 ' TO W-IDLEVNR-ALPHA  END-IF                           
384200     IF W-IDLEVNR-ALPHA = 'J3CQA'                                         
384300        MOVE '4319 ' TO W-IDLEVNR-ALPHA  END-IF                           
384400     IF W-IDLEVNR-ALPHA = 'D026P'                                         
384500        MOVE '4382 ' TO W-IDLEVNR-ALPHA  END-IF                           
384600     IF W-IDLEVNR-ALPHA = 'MLMZA'                                         
384700        MOVE '4448 ' TO W-IDLEVNR-ALPHA  END-IF                           
384800     IF W-IDLEVNR-ALPHA = 'BQ6WA'                                         
384900        MOVE '4585 ' TO W-IDLEVNR-ALPHA  END-IF                           
385000     IF W-IDLEVNR-ALPHA = 'BQ6WB'                                         
385100        MOVE '4628 ' TO W-IDLEVNR-ALPHA  END-IF                           
385200     IF W-IDLEVNR-ALPHA = 'A426K'                                         
385300        MOVE '4894 ' TO W-IDLEVNR-ALPHA  END-IF                           
385400     IF W-IDLEVNR-ALPHA = 'C0VAG'                                         
385500        MOVE '4965 ' TO W-IDLEVNR-ALPHA  END-IF                           
385600     IF W-IDLEVNR-ALPHA = 'AVG9A'                                         
385700        MOVE '5065 ' TO W-IDLEVNR-ALPHA  END-IF                           
385800     IF W-IDLEVNR-ALPHA = 'D24DA'                                         
385900        MOVE '5647 ' TO W-IDLEVNR-ALPHA  END-IF                           
386000     IF W-IDLEVNR-ALPHA = 'D0RYA'                                         
386100        MOVE '6030 ' TO W-IDLEVNR-ALPHA  END-IF                           
386200     IF W-IDLEVNR-ALPHA = 'BT7WA'                                         
386300        MOVE '6279 ' TO W-IDLEVNR-ALPHA  END-IF                           
386400     IF W-IDLEVNR-ALPHA = 'A426G'                                         
386500        MOVE '6840 ' TO W-IDLEVNR-ALPHA  END-IF                           
386600     IF W-IDLEVNR-ALPHA = 'F488A'                                         
386700        MOVE '6992 ' TO W-IDLEVNR-ALPHA  END-IF                           
386800     IF W-IDLEVNR-ALPHA = 'BP3HA'                                         
386900        MOVE '7349 ' TO W-IDLEVNR-ALPHA  END-IF                           
387000     IF W-IDLEVNR-ALPHA = 'CL3VA'                                         
387100        MOVE '10453' TO W-IDLEVNR-ALPHA  END-IF                           
387200     IF W-IDLEVNR-ALPHA = 'A426S'                                         
387300        MOVE '10814' TO W-IDLEVNR-ALPHA  END-IF                           
387400     IF W-IDLEVNR-ALPHA = 'CFT4B'                                         
387500        MOVE '12543' TO W-IDLEVNR-ALPHA  END-IF                           
387600     IF W-IDLEVNR-ALPHA = 'M09EA'                                         
387700        MOVE '14616' TO W-IDLEVNR-ALPHA  END-IF                           
387800     IF W-IDLEVNR-ALPHA = 'BQ6WC'                                         
387900        MOVE '14643' TO W-IDLEVNR-ALPHA  END-IF                           
388000     IF W-IDLEVNR-ALPHA = 'A426R'                                         
388100        MOVE '14647' TO W-IDLEVNR-ALPHA  END-IF                           
388200     IF W-IDLEVNR-ALPHA = 'N718D'                                         
388300        MOVE '14963' TO W-IDLEVNR-ALPHA  END-IF                           
388400     IF W-IDLEVNR-ALPHA = 'N718B'                                         
388500        MOVE '14988' TO W-IDLEVNR-ALPHA  END-IF                           
388600     IF W-IDLEVNR-ALPHA = 'B492C'                                         
388700        MOVE '16134' TO W-IDLEVNR-ALPHA  END-IF                           
388800     IF W-IDLEVNR-ALPHA = 'F488X'                                         
388900        MOVE '16137' TO W-IDLEVNR-ALPHA  END-IF                           
389000     IF W-IDLEVNR-ALPHA = 'AYZ4A'                                         
389100        MOVE '16210' TO W-IDLEVNR-ALPHA  END-IF                           
389200     IF W-IDLEVNR-ALPHA = 'C685Y'                                         
389300        MOVE '16211' TO W-IDLEVNR-ALPHA  END-IF                           
389400     IF W-IDLEVNR-ALPHA = 'C685C'                                         
389500        MOVE '16222' TO W-IDLEVNR-ALPHA  END-IF                           
389600     IF W-IDLEVNR-ALPHA = 'A426C'                                         
389700        MOVE '16237' TO W-IDLEVNR-ALPHA  END-IF                           
389800     IF W-IDLEVNR-ALPHA = 'A426M'                                         
389900        MOVE '16279' TO W-IDLEVNR-ALPHA  END-IF                           
390000     IF W-IDLEVNR-ALPHA = 'F488Z'                                         
390100        MOVE '16283' TO W-IDLEVNR-ALPHA  END-IF                           
390200     IF W-IDLEVNR-ALPHA = 'BA8YA'                                         
390300        MOVE '20094' TO W-IDLEVNR-ALPHA  END-IF                           
390400     IF W-IDLEVNR-ALPHA = 'AUE4A'                                         
390500        MOVE '21873' TO W-IDLEVNR-ALPHA  END-IF                           
390600     IF W-IDLEVNR-ALPHA = 'M09EB'                                         
390700        MOVE '25851' TO W-IDLEVNR-ALPHA  END-IF                           
390800     IF W-IDLEVNR-ALPHA = 'BARJA'                                         
390900        MOVE '25907' TO W-IDLEVNR-ALPHA  END-IF                           
391000     IF W-IDLEVNR-ALPHA = 'BARJB'                                         
391100        MOVE '25908' TO W-IDLEVNR-ALPHA  END-IF                           
391200     IF W-IDLEVNR-ALPHA = 'CUTBA'                                         
391300        MOVE '25909' TO W-IDLEVNR-ALPHA  END-IF                           
391400     IF W-IDLEVNR-ALPHA = 'A426U'                                         
391500        MOVE '26840' TO W-IDLEVNR-ALPHA  END-IF                           
391600     IF W-IDLEVNR-ALPHA = 'BQ7RB'                                         
391700        MOVE '24066' TO W-IDLEVNR-ALPHA  END-IF                           
391800     IF W-IDLEVNR-ALPHA = 'BQ7RC'                                         
391900        MOVE '25752' TO W-IDLEVNR-ALPHA  END-IF                           
392000*************************************************                         
392100     IF W-IDLEVNR-ALPHA = 'BMP1A'                                         
392200        MOVE '856  ' TO W-IDLEVNR-ALPHA  END-IF                           
392300     IF W-IDLEVNR-ALPHA = 'D07FA'                                         
392400        MOVE '3330 ' TO W-IDLEVNR-ALPHA  END-IF                           
392500     IF W-IDLEVNR-ALPHA = 'T2VGA'                                         
392600        MOVE '5453 ' TO W-IDLEVNR-ALPHA  END-IF                           
392700     IF W-IDLEVNR-ALPHA = 'D07FD'                                         
392800        MOVE '6013 ' TO W-IDLEVNR-ALPHA  END-IF                           
392900     IF W-IDLEVNR-ALPHA = 'E510D'                                         
393000        MOVE '6160 ' TO W-IDLEVNR-ALPHA  END-IF                           
393100     IF W-IDLEVNR-ALPHA = 'D07FF'                                         
393200        MOVE '6193 ' TO W-IDLEVNR-ALPHA  END-IF                           
393300     IF W-IDLEVNR-ALPHA = 'BQ7MA'                                         
393400        MOVE '6285 ' TO W-IDLEVNR-ALPHA  END-IF                           
393500     IF W-IDLEVNR-ALPHA = 'CGSWA'                                         
393600        MOVE '7622 ' TO W-IDLEVNR-ALPHA  END-IF                           
393700     IF W-IDLEVNR-ALPHA = 'BZMDA'                                         
393800        MOVE '10965' TO W-IDLEVNR-ALPHA  END-IF                           
393900     IF W-IDLEVNR-ALPHA = 'BEF1A'                                         
394000        MOVE '20303' TO W-IDLEVNR-ALPHA  END-IF                           
394100*************************************************                         
394200     IF W-IDLEVNR-ALPHA = 'BWTAA'                                         
394300        MOVE '520  ' TO W-IDLEVNR-ALPHA  END-IF                           
394400     IF W-IDLEVNR-ALPHA = 'BQ00A'                                         
394500        MOVE '546  ' TO W-IDLEVNR-ALPHA  END-IF                           
394600     IF W-IDLEVNR-ALPHA = 'AHFGA'                                         
394700        MOVE '547  ' TO W-IDLEVNR-ALPHA  END-IF                           
394800     IF W-IDLEVNR-ALPHA = 'BQ1BA'                                         
394900        MOVE '550  ' TO W-IDLEVNR-ALPHA  END-IF                           
395000     IF W-IDLEVNR-ALPHA = 'BQ3YA'                                         
395100        MOVE '1883 ' TO W-IDLEVNR-ALPHA  END-IF                           
395200     IF W-IDLEVNR-ALPHA = 'S3HXA'                                         
395300        MOVE '4610 ' TO W-IDLEVNR-ALPHA  END-IF                           
395400     IF W-IDLEVNR-ALPHA = 'AKTAD'                                         
395500        MOVE '6110 ' TO W-IDLEVNR-ALPHA  END-IF                           
395600     IF W-IDLEVNR-ALPHA = 'BQAWA'                                         
395700        MOVE '10087' TO W-IDLEVNR-ALPHA  END-IF                           
395800     IF W-IDLEVNR-ALPHA = 'BAM8D'                                         
395900        MOVE '11328' TO W-IDLEVNR-ALPHA  END-IF                           
396000     IF W-IDLEVNR-ALPHA = 'BAM8A'                                         
396100        MOVE '19956' TO W-IDLEVNR-ALPHA  END-IF                           
396200     IF W-IDLEVNR-ALPHA = 'CEPHA'                                         
396300        MOVE '19986' TO W-IDLEVNR-ALPHA  END-IF                           
396400*************************************************                         
396500     IF W-IDLEVNR-ALPHA = 'BQAHA'                                         
396600        MOVE '114  ' TO W-IDLEVNR-ALPHA  END-IF                           
396700     IF W-IDLEVNR-ALPHA = 'S3SQA'                                         
396800        MOVE '642  ' TO W-IDLEVNR-ALPHA  END-IF                           
396900     IF W-IDLEVNR-ALPHA = 'S3SQB'                                         
397000        MOVE '1297 ' TO W-IDLEVNR-ALPHA  END-IF                           
397100     IF W-IDLEVNR-ALPHA = 'BQAHD'                                         
397200        MOVE '2639 ' TO W-IDLEVNR-ALPHA  END-IF                           
397300     IF W-IDLEVNR-ALPHA = 'BT7RA'                                         
397400        MOVE '2647 ' TO W-IDLEVNR-ALPHA  END-IF                           
397500     IF W-IDLEVNR-ALPHA = 'BEFYA'                                         
397600        MOVE '3671 ' TO W-IDLEVNR-ALPHA  END-IF                           
397700     IF W-IDLEVNR-ALPHA = 'K0R6A'                                         
397800        MOVE '3787 ' TO W-IDLEVNR-ALPHA  END-IF                           
397900     IF W-IDLEVNR-ALPHA = 'C84QA'                                         
398000        MOVE '3883 ' TO W-IDLEVNR-ALPHA  END-IF                           
398100     IF W-IDLEVNR-ALPHA = 'P38NA'                                         
398200        MOVE '3988 ' TO W-IDLEVNR-ALPHA  END-IF                           
398300     IF W-IDLEVNR-ALPHA = 'MMV4A'                                         
398400        MOVE '4737 ' TO W-IDLEVNR-ALPHA  END-IF                           
398500     IF W-IDLEVNR-ALPHA = 'F842A'                                         
398600        MOVE '4961 ' TO W-IDLEVNR-ALPHA  END-IF                           
398700     IF W-IDLEVNR-ALPHA = 'E1SLB'                                         
398800        MOVE '5160 ' TO W-IDLEVNR-ALPHA  END-IF                           
398900     IF W-IDLEVNR-ALPHA = 'D2R5A'                                         
399000        MOVE '5162 ' TO W-IDLEVNR-ALPHA  END-IF                           
399100     IF W-IDLEVNR-ALPHA = 'C93SA'                                         
399200        MOVE '5171 ' TO W-IDLEVNR-ALPHA  END-IF                           
399300     IF W-IDLEVNR-ALPHA = 'Q749A'                                         
399400        MOVE '5240 ' TO W-IDLEVNR-ALPHA  END-IF                           
399500     IF W-IDLEVNR-ALPHA = 'H268X'                                         
399600        MOVE '5383 ' TO W-IDLEVNR-ALPHA  END-IF                           
399700     IF W-IDLEVNR-ALPHA = 'AXVNA'                                         
399800        MOVE '5662 ' TO W-IDLEVNR-ALPHA  END-IF                           
399900     IF W-IDLEVNR-ALPHA = 'D16AA'                                         
400000        MOVE '6083 ' TO W-IDLEVNR-ALPHA  END-IF                           
400100     IF W-IDLEVNR-ALPHA = 'E521A'                                         
400200        MOVE '6090 ' TO W-IDLEVNR-ALPHA  END-IF                           
400300     IF W-IDLEVNR-ALPHA = 'D04HA'                                         
400400        MOVE '6096 ' TO W-IDLEVNR-ALPHA  END-IF                           
400500     IF W-IDLEVNR-ALPHA = 'EGX2A'                                         
400600        MOVE '6146 ' TO W-IDLEVNR-ALPHA  END-IF                           
400700     IF W-IDLEVNR-ALPHA = 'C6S4A'                                         
400800        MOVE '6200 ' TO W-IDLEVNR-ALPHA  END-IF                           
400900     IF W-IDLEVNR-ALPHA = 'C9U3A'                                         
401000        MOVE '6226 ' TO W-IDLEVNR-ALPHA  END-IF                           
401100     IF W-IDLEVNR-ALPHA = 'D1C6A'                                         
401200        MOVE '6310 ' TO W-IDLEVNR-ALPHA  END-IF                           
401300     IF W-IDLEVNR-ALPHA = 'F6W7A'                                         
401400        MOVE '6321 ' TO W-IDLEVNR-ALPHA  END-IF                           
401500     IF W-IDLEVNR-ALPHA = 'CFTXA'                                         
401600        MOVE '6328 ' TO W-IDLEVNR-ALPHA  END-IF                           
401700     IF W-IDLEVNR-ALPHA = 'AECTA'                                         
401800        MOVE '6360 ' TO W-IDLEVNR-ALPHA  END-IF                           
401900     IF W-IDLEVNR-ALPHA = 'BPU8A'                                         
402000        MOVE '6421 ' TO W-IDLEVNR-ALPHA  END-IF                           
402100     IF W-IDLEVNR-ALPHA = 'X345A'                                         
402200        MOVE '6443 ' TO W-IDLEVNR-ALPHA  END-IF                           
402300     IF W-IDLEVNR-ALPHA = 'D16MA'                                         
402400        MOVE '6468 ' TO W-IDLEVNR-ALPHA  END-IF                           
402500     IF W-IDLEVNR-ALPHA = 'BB4SA'                                         
402600        MOVE '6488 ' TO W-IDLEVNR-ALPHA  END-IF                           
402700     IF W-IDLEVNR-ALPHA = 'BZFFA'                                         
402800        MOVE '6492 ' TO W-IDLEVNR-ALPHA  END-IF                           
402900     IF W-IDLEVNR-ALPHA = 'C9T9A'                                         
403000        MOVE '6764 ' TO W-IDLEVNR-ALPHA  END-IF                           
403100     IF W-IDLEVNR-ALPHA = 'D3L4A'                                         
403200        MOVE '6903 ' TO W-IDLEVNR-ALPHA  END-IF                           
403300     IF W-IDLEVNR-ALPHA = 'DNR5A'                                         
403400        MOVE '6968 ' TO W-IDLEVNR-ALPHA  END-IF                           
403500     IF W-IDLEVNR-ALPHA = 'D0DMA'                                         
403600        MOVE '8040 ' TO W-IDLEVNR-ALPHA  END-IF                           
403700     IF W-IDLEVNR-ALPHA = 'BPXEB'                                         
403800        MOVE '10103' TO W-IDLEVNR-ALPHA  END-IF                           
403900     IF W-IDLEVNR-ALPHA = 'R2FLG'                                         
404000        MOVE '11096' TO W-IDLEVNR-ALPHA  END-IF                           
404100     IF W-IDLEVNR-ALPHA = 'F432J'                                         
404200        MOVE '13594' TO W-IDLEVNR-ALPHA  END-IF                           
404300     IF W-IDLEVNR-ALPHA = 'F432B'                                         
404400        MOVE '13690' TO W-IDLEVNR-ALPHA  END-IF                           
404500     IF W-IDLEVNR-ALPHA = 'K0R6E'                                         
404600        MOVE '13701' TO W-IDLEVNR-ALPHA  END-IF                           
404700     IF W-IDLEVNR-ALPHA = 'E2L9A'                                         
404800        MOVE '16051' TO W-IDLEVNR-ALPHA  END-IF                           
404900     IF W-IDLEVNR-ALPHA = 'C6S4B'                                         
405000        MOVE '16115' TO W-IDLEVNR-ALPHA  END-IF                           
405100     IF W-IDLEVNR-ALPHA = 'H681K'                                         
405200        MOVE '16383' TO W-IDLEVNR-ALPHA  END-IF                           
405300     IF W-IDLEVNR-ALPHA = 'D0V6C'                                         
405400        MOVE '25047' TO W-IDLEVNR-ALPHA  END-IF                           
405500     IF W-IDLEVNR-ALPHA = 'E521J'                                         
405600        MOVE '25403' TO W-IDLEVNR-ALPHA  END-IF                           
405700     IF W-IDLEVNR-ALPHA = 'E019A'                                         
405800        MOVE '50535' TO W-IDLEVNR-ALPHA  END-IF                           
405900     IF W-IDLEVNR-ALPHA = 'CL3XA'                                         
406000        MOVE '63567' TO W-IDLEVNR-ALPHA  END-IF                           
406100*************************************************                         
406200     IF W-IDLEVNR-ALPHA = 'BSK5A'                                         
406300        MOVE '84   ' TO W-IDLEVNR-ALPHA  END-IF                           
406400     IF W-IDLEVNR-ALPHA = 'DJAYA'                                         
406500        MOVE '157  ' TO W-IDLEVNR-ALPHA  END-IF                           
406600     IF W-IDLEVNR-ALPHA = 'BV9NA'                                         
406700        MOVE '173  ' TO W-IDLEVNR-ALPHA  END-IF                           
406800     IF W-IDLEVNR-ALPHA = 'BU4GA'                                         
406900        MOVE '233  ' TO W-IDLEVNR-ALPHA  END-IF                           
407000     IF W-IDLEVNR-ALPHA = 'DJA8A'                                         
407100        MOVE '235  ' TO W-IDLEVNR-ALPHA  END-IF                           
407200     IF W-IDLEVNR-ALPHA = 'DL0KA'                                         
407300        MOVE '282  ' TO W-IDLEVNR-ALPHA  END-IF                           
407400     IF W-IDLEVNR-ALPHA = 'BXPRA'                                         
407500        MOVE '294  ' TO W-IDLEVNR-ALPHA  END-IF                           
407600     IF W-IDLEVNR-ALPHA = 'DJBBA'                                         
407700        MOVE '357  ' TO W-IDLEVNR-ALPHA  END-IF                           
407800     IF W-IDLEVNR-ALPHA = 'DL0LA'                                         
407900        MOVE '370  ' TO W-IDLEVNR-ALPHA  END-IF                           
408000     IF W-IDLEVNR-ALPHA = 'DJBCA'                                         
408100        MOVE '376  ' TO W-IDLEVNR-ALPHA  END-IF                           
408200     IF W-IDLEVNR-ALPHA = 'DL0MA'                                         
408300        MOVE '428  ' TO W-IDLEVNR-ALPHA  END-IF                           
408400     IF W-IDLEVNR-ALPHA = 'BRQFA'                                         
408500        MOVE '449  ' TO W-IDLEVNR-ALPHA  END-IF                           
408600     IF W-IDLEVNR-ALPHA = 'BSK2A'                                         
408700        MOVE '466  ' TO W-IDLEVNR-ALPHA  END-IF                           
408800     IF W-IDLEVNR-ALPHA = 'S6RPA'                                         
408900        MOVE '498  ' TO W-IDLEVNR-ALPHA  END-IF                           
409000     IF W-IDLEVNR-ALPHA = 'BLMNA'                                         
409100        MOVE '505  ' TO W-IDLEVNR-ALPHA  END-IF                           
409200     IF W-IDLEVNR-ALPHA = 'BJ9QA'                                         
409300        MOVE '523  ' TO W-IDLEVNR-ALPHA  END-IF                           
409400     IF W-IDLEVNR-ALPHA = 'DJBDA'                                         
409500        MOVE '524  ' TO W-IDLEVNR-ALPHA  END-IF                           
409600     IF W-IDLEVNR-ALPHA = 'CVESB'                                         
409700        MOVE '555  ' TO W-IDLEVNR-ALPHA  END-IF                           
409800     IF W-IDLEVNR-ALPHA = 'CFH3A'                                         
409900        MOVE '652  ' TO W-IDLEVNR-ALPHA  END-IF                           
410000     IF W-IDLEVNR-ALPHA = 'DJBEA'                                         
410100        MOVE '670  ' TO W-IDLEVNR-ALPHA  END-IF                           
410200     IF W-IDLEVNR-ALPHA = 'BMKTA'                                         
410300        MOVE '671  ' TO W-IDLEVNR-ALPHA  END-IF                           
410400     IF W-IDLEVNR-ALPHA = 'DJBFA'                                         
410500        MOVE '719  ' TO W-IDLEVNR-ALPHA  END-IF                           
410600     IF W-IDLEVNR-ALPHA = 'CDT1A'                                         
410700        MOVE '753  ' TO W-IDLEVNR-ALPHA  END-IF                           
410800     IF W-IDLEVNR-ALPHA = 'DJBGA'                                         
410900        MOVE '794  ' TO W-IDLEVNR-ALPHA  END-IF                           
411000     IF W-IDLEVNR-ALPHA = 'DJKLA'                                         
411100        MOVE '804  ' TO W-IDLEVNR-ALPHA  END-IF                           
411200     IF W-IDLEVNR-ALPHA = 'BN6ZA'                                         
411300        MOVE '944  ' TO W-IDLEVNR-ALPHA  END-IF                           
411400     IF W-IDLEVNR-ALPHA = 'DL0PA'                                         
411500        MOVE '948  ' TO W-IDLEVNR-ALPHA  END-IF                           
411600     IF W-IDLEVNR-ALPHA = 'BSKZA'                                         
411700        MOVE '971  ' TO W-IDLEVNR-ALPHA  END-IF                           
411800     IF W-IDLEVNR-ALPHA = 'DL0TA'                                         
411900        MOVE '1025 ' TO W-IDLEVNR-ALPHA  END-IF                           
412000     IF W-IDLEVNR-ALPHA = 'BZCEA'                                         
412100        MOVE '1068 ' TO W-IDLEVNR-ALPHA  END-IF                           
412200     IF W-IDLEVNR-ALPHA = 'DJKMA'                                         
412300        MOVE '1102 ' TO W-IDLEVNR-ALPHA  END-IF                           
412400     IF W-IDLEVNR-ALPHA = 'DL0UA'                                         
412500        MOVE '1160 ' TO W-IDLEVNR-ALPHA  END-IF                           
412600     IF W-IDLEVNR-ALPHA = 'BKMEA'                                         
412700        MOVE '1232 ' TO W-IDLEVNR-ALPHA  END-IF                           
412800     IF W-IDLEVNR-ALPHA = 'BJTTA'                                         
412900        MOVE '1293 ' TO W-IDLEVNR-ALPHA  END-IF                           
413000     IF W-IDLEVNR-ALPHA = 'BKMJA'                                         
413100        MOVE '1304 ' TO W-IDLEVNR-ALPHA  END-IF                           
413200     IF W-IDLEVNR-ALPHA = 'BMN7B'                                         
413300        MOVE '1331 ' TO W-IDLEVNR-ALPHA  END-IF                           
413400     IF W-IDLEVNR-ALPHA = 'BX4TA'                                         
413500        MOVE '1512 ' TO W-IDLEVNR-ALPHA  END-IF                           
413600     IF W-IDLEVNR-ALPHA = 'DJKNA'                                         
413700        MOVE '1523 ' TO W-IDLEVNR-ALPHA  END-IF                           
413800     IF W-IDLEVNR-ALPHA = 'DJKPA'                                         
413900        MOVE '1542 ' TO W-IDLEVNR-ALPHA  END-IF                           
414000     IF W-IDLEVNR-ALPHA = 'DJKQA'                                         
414100        MOVE '1606 ' TO W-IDLEVNR-ALPHA  END-IF                           
414200     IF W-IDLEVNR-ALPHA = 'BKEBA'                                         
414300        MOVE '1633 ' TO W-IDLEVNR-ALPHA  END-IF                           
414400     IF W-IDLEVNR-ALPHA = 'BJZQA'                                         
414500        MOVE '1699 ' TO W-IDLEVNR-ALPHA  END-IF                           
414600     IF W-IDLEVNR-ALPHA = 'DJKRA'                                         
414700        MOVE '1756 ' TO W-IDLEVNR-ALPHA  END-IF                           
414800     IF W-IDLEVNR-ALPHA = 'DJKSA'                                         
414900        MOVE '1764 ' TO W-IDLEVNR-ALPHA  END-IF                           
415000     IF W-IDLEVNR-ALPHA = 'BJ9ZA'                                         
415100        MOVE '1887 ' TO W-IDLEVNR-ALPHA  END-IF                           
415200     IF W-IDLEVNR-ALPHA = 'BKMUA'                                         
415300        MOVE '1916 ' TO W-IDLEVNR-ALPHA  END-IF                           
415400     IF W-IDLEVNR-ALPHA = 'DL0XA'                                         
415500        MOVE '2028 ' TO W-IDLEVNR-ALPHA  END-IF                           
415600     IF W-IDLEVNR-ALPHA = 'BLWMA'                                         
415700        MOVE '2055 ' TO W-IDLEVNR-ALPHA  END-IF                           
415800     IF W-IDLEVNR-ALPHA = 'BJQTA'                                         
415900        MOVE '2177 ' TO W-IDLEVNR-ALPHA  END-IF                           
416000     IF W-IDLEVNR-ALPHA = 'BJPDA'                                         
416100        MOVE '2229 ' TO W-IDLEVNR-ALPHA  END-IF                           
416200     IF W-IDLEVNR-ALPHA = 'CZ4BA'                                         
416300        MOVE '2243 ' TO W-IDLEVNR-ALPHA  END-IF                           
416400     IF W-IDLEVNR-ALPHA = 'DJKTA'                                         
416500        MOVE '2284 ' TO W-IDLEVNR-ALPHA  END-IF                           
416600     IF W-IDLEVNR-ALPHA = 'BK4LA'                                         
416700        MOVE '2322 ' TO W-IDLEVNR-ALPHA  END-IF                           
416800     IF W-IDLEVNR-ALPHA = 'DL0ZA'                                         
416900        MOVE '2344 ' TO W-IDLEVNR-ALPHA  END-IF                           
417000     IF W-IDLEVNR-ALPHA = 'DJKVA'                                         
417100        MOVE '2446 ' TO W-IDLEVNR-ALPHA  END-IF                           
417200     IF W-IDLEVNR-ALPHA = 'BJKNA'                                         
417300        MOVE '2451 ' TO W-IDLEVNR-ALPHA  END-IF                           
417400     IF W-IDLEVNR-ALPHA = 'CD2HA'                                         
417500        MOVE '2480 ' TO W-IDLEVNR-ALPHA  END-IF                           
417600     IF W-IDLEVNR-ALPHA = 'BJYVA'                                         
417700        MOVE '2619 ' TO W-IDLEVNR-ALPHA  END-IF                           
417800     IF W-IDLEVNR-ALPHA = 'BL3AA'                                         
417900        MOVE '2669 ' TO W-IDLEVNR-ALPHA  END-IF                           
418000     IF W-IDLEVNR-ALPHA = 'BLNLA'                                         
418100        MOVE '3105 ' TO W-IDLEVNR-ALPHA  END-IF                           
418200     IF W-IDLEVNR-ALPHA = 'DJKWA'                                         
418300        MOVE '3304 ' TO W-IDLEVNR-ALPHA  END-IF                           
418400     IF W-IDLEVNR-ALPHA = 'DL1RA'                                         
418500        MOVE '3310 ' TO W-IDLEVNR-ALPHA  END-IF                           
418600     IF W-IDLEVNR-ALPHA = 'DL1SA'                                         
418700        MOVE '3332 ' TO W-IDLEVNR-ALPHA  END-IF                           
418800     IF W-IDLEVNR-ALPHA = 'DEV4A'                                         
418900        MOVE '3342 ' TO W-IDLEVNR-ALPHA  END-IF                           
419000     IF W-IDLEVNR-ALPHA = 'DJKXA'                                         
419100        MOVE '3343 ' TO W-IDLEVNR-ALPHA  END-IF                           
419200     IF W-IDLEVNR-ALPHA = 'DJKYA'                                         
419300        MOVE '3345 ' TO W-IDLEVNR-ALPHA  END-IF                           
419400     IF W-IDLEVNR-ALPHA = 'DL1TA'                                         
419500        MOVE '3349 ' TO W-IDLEVNR-ALPHA  END-IF                           
419600     IF W-IDLEVNR-ALPHA = 'DL1UA'                                         
419700        MOVE '3354 ' TO W-IDLEVNR-ALPHA  END-IF                           
419800     IF W-IDLEVNR-ALPHA = 'DJKZA'                                         
419900        MOVE '3369 ' TO W-IDLEVNR-ALPHA  END-IF                           
420000     IF W-IDLEVNR-ALPHA = 'DJK0A'                                         
420100        MOVE '3375 ' TO W-IDLEVNR-ALPHA  END-IF                           
420200     IF W-IDLEVNR-ALPHA = 'DL1VA'                                         
420300        MOVE '3376 ' TO W-IDLEVNR-ALPHA  END-IF                           
420400     IF W-IDLEVNR-ALPHA = 'BQ6SA'                                         
420500        MOVE '3389 ' TO W-IDLEVNR-ALPHA  END-IF                           
420600     IF W-IDLEVNR-ALPHA = 'DJK9A'                                         
420700        MOVE '3401 ' TO W-IDLEVNR-ALPHA  END-IF                           
420800     IF W-IDLEVNR-ALPHA = 'CFJCA'                                         
420900        MOVE '3405 ' TO W-IDLEVNR-ALPHA  END-IF                           
421000     IF W-IDLEVNR-ALPHA = 'BK1EA'                                         
421100        MOVE '3421 ' TO W-IDLEVNR-ALPHA  END-IF                           
421200     IF W-IDLEVNR-ALPHA = 'DJLAA'                                         
421300        MOVE '3423 ' TO W-IDLEVNR-ALPHA  END-IF                           
421400     IF W-IDLEVNR-ALPHA = 'DL1XA'                                         
421500        MOVE '3424 ' TO W-IDLEVNR-ALPHA  END-IF                           
421600*************************************************                         
421700     IF W-IDLEVNR-ALPHA = 'BP8CA'                                         
421800        MOVE '1335 ' TO W-IDLEVNR-ALPHA  END-IF                           
421900     IF W-IDLEVNR-ALPHA = 'BWKFA'                                         
422000        MOVE '1649 ' TO W-IDLEVNR-ALPHA  END-IF                           
422100     IF W-IDLEVNR-ALPHA = 'G13FA'                                         
422200        MOVE '1797 ' TO W-IDLEVNR-ALPHA  END-IF                           
422300     IF W-IDLEVNR-ALPHA = 'BLMPA'                                         
422400        MOVE '2113 ' TO W-IDLEVNR-ALPHA  END-IF                           
422500     IF W-IDLEVNR-ALPHA = 'BQ7HA'                                         
422600        MOVE '2302 ' TO W-IDLEVNR-ALPHA  END-IF                           
422700     IF W-IDLEVNR-ALPHA = 'M279D'                                         
422800        MOVE '3556 ' TO W-IDLEVNR-ALPHA  END-IF                           
422900     IF W-IDLEVNR-ALPHA = 'AB7ZA'                                         
423000        MOVE '4760 ' TO W-IDLEVNR-ALPHA  END-IF                           
423100     IF W-IDLEVNR-ALPHA = 'D0UQA'                                         
423200        MOVE '6191 ' TO W-IDLEVNR-ALPHA  END-IF                           
423300     IF W-IDLEVNR-ALPHA = 'P511A'                                         
423400        MOVE '6354 ' TO W-IDLEVNR-ALPHA  END-IF                           
423500     IF W-IDLEVNR-ALPHA = 'ADBTA'                                         
423600        MOVE '6505 ' TO W-IDLEVNR-ALPHA  END-IF                           
423700     IF W-IDLEVNR-ALPHA = 'B42KA'                                         
423800        MOVE '6512 ' TO W-IDLEVNR-ALPHA  END-IF                           
423900     IF W-IDLEVNR-ALPHA = 'K1FWA'                                         
424000        MOVE '6589 ' TO W-IDLEVNR-ALPHA  END-IF                           
424100     IF W-IDLEVNR-ALPHA = 'M1F7A'                                         
424200        MOVE '6608 ' TO W-IDLEVNR-ALPHA  END-IF                           
424300     IF W-IDLEVNR-ALPHA = 'CX9XA'                                         
424400        MOVE '6650 ' TO W-IDLEVNR-ALPHA  END-IF                           
424500     IF W-IDLEVNR-ALPHA = 'P790A'                                         
424600        MOVE '6665 ' TO W-IDLEVNR-ALPHA  END-IF                           
424700     IF W-IDLEVNR-ALPHA = 'S106A'                                         
424800        MOVE '6669 ' TO W-IDLEVNR-ALPHA  END-IF                           
424900     IF W-IDLEVNR-ALPHA = 'J613A'                                         
425000        MOVE '6684 ' TO W-IDLEVNR-ALPHA  END-IF                           
425100     IF W-IDLEVNR-ALPHA = 'BZ02A'                                         
425200        MOVE '6692 ' TO W-IDLEVNR-ALPHA  END-IF                           
425300     IF W-IDLEVNR-ALPHA = 'B4W7A'                                         
425400        MOVE '6823 ' TO W-IDLEVNR-ALPHA  END-IF                           
425500     IF W-IDLEVNR-ALPHA = 'D0QWA'                                         
425600        MOVE '7277 ' TO W-IDLEVNR-ALPHA  END-IF                           
425700     IF W-IDLEVNR-ALPHA = 'BCSFA'                                         
425800        MOVE '7317 ' TO W-IDLEVNR-ALPHA  END-IF                           
425900     IF W-IDLEVNR-ALPHA = 'BPTMB'                                         
426000        MOVE '7900 ' TO W-IDLEVNR-ALPHA  END-IF                           
426100     IF W-IDLEVNR-ALPHA = 'BPTMD'                                         
426200        MOVE '10159' TO W-IDLEVNR-ALPHA  END-IF                           
426300     IF W-IDLEVNR-ALPHA = 'BPTMA'                                         
426400        MOVE '10160' TO W-IDLEVNR-ALPHA  END-IF                           
426500     IF W-IDLEVNR-ALPHA = 'BPTMC'                                         
426600        MOVE '11377' TO W-IDLEVNR-ALPHA  END-IF                           
426700     IF W-IDLEVNR-ALPHA = 'BP8DB'                                         
426800        MOVE '13585' TO W-IDLEVNR-ALPHA  END-IF                           
426900     IF W-IDLEVNR-ALPHA = 'BP8DD'                                         
427000        MOVE '13622' TO W-IDLEVNR-ALPHA  END-IF                           
427100     IF W-IDLEVNR-ALPHA = 'T8LLA'                                         
427200        MOVE '13801' TO W-IDLEVNR-ALPHA  END-IF                           
427300     IF W-IDLEVNR-ALPHA = 'D06MA'                                         
427400        MOVE '13849' TO W-IDLEVNR-ALPHA  END-IF                           
427500     IF W-IDLEVNR-ALPHA = 'B47PG'                                         
427600        MOVE '14280' TO W-IDLEVNR-ALPHA  END-IF                           
427700     IF W-IDLEVNR-ALPHA = 'CNXXA'                                         
427800        MOVE '14500' TO W-IDLEVNR-ALPHA  END-IF                           
427900     IF W-IDLEVNR-ALPHA = 'BPW0A'                                         
428000        MOVE '14519' TO W-IDLEVNR-ALPHA  END-IF                           
428100     IF W-IDLEVNR-ALPHA = 'C9D2A'                                         
428200        MOVE '14944' TO W-IDLEVNR-ALPHA  END-IF                           
428300     IF W-IDLEVNR-ALPHA = 'BPTME'                                         
428400        MOVE '16036' TO W-IDLEVNR-ALPHA  END-IF                           
428500     IF W-IDLEVNR-ALPHA = 'M279C'                                         
428600        MOVE '16144' TO W-IDLEVNR-ALPHA  END-IF                           
428700     IF W-IDLEVNR-ALPHA = 'M279E'                                         
428800        MOVE '16145' TO W-IDLEVNR-ALPHA  END-IF                           
428900     IF W-IDLEVNR-ALPHA = 'U2W5B'                                         
429000        MOVE '16274' TO W-IDLEVNR-ALPHA  END-IF                           
429100     IF W-IDLEVNR-ALPHA = 'K1FWB'                                         
429200        MOVE '16332' TO W-IDLEVNR-ALPHA  END-IF                           
429300     IF W-IDLEVNR-ALPHA = 'D059D'                                         
429400        MOVE '19564' TO W-IDLEVNR-ALPHA  END-IF                           
429500     IF W-IDLEVNR-ALPHA = 'BP8DC'                                         
429600        MOVE '19609' TO W-IDLEVNR-ALPHA  END-IF                           
429700     IF W-IDLEVNR-ALPHA = 'G1UHN'                                         
429800        MOVE '21590' TO W-IDLEVNR-ALPHA  END-IF                           
429900     IF W-IDLEVNR-ALPHA = 'D30FA'                                         
430000        MOVE '22419' TO W-IDLEVNR-ALPHA  END-IF                           
430100     IF W-IDLEVNR-ALPHA = 'D01QA'                                         
430200        MOVE '22420' TO W-IDLEVNR-ALPHA  END-IF                           
430300     IF W-IDLEVNR-ALPHA = 'D059E'                                         
430400        MOVE '23375' TO W-IDLEVNR-ALPHA  END-IF                           
430500     IF W-IDLEVNR-ALPHA = 'BQ1ZC'                                         
430600        MOVE '23919' TO W-IDLEVNR-ALPHA  END-IF                           
430700     IF W-IDLEVNR-ALPHA = 'D059F'                                         
430800        MOVE '23926' TO W-IDLEVNR-ALPHA  END-IF                           
430900     IF W-IDLEVNR-ALPHA = 'ABD3A'                                         
431000        MOVE '25944' TO W-IDLEVNR-ALPHA  END-IF                           
431100     IF W-IDLEVNR-ALPHA = 'P790M'                                         
431200        MOVE '16149' TO W-IDLEVNR-ALPHA  END-IF                           
431300*************************************************                         
431400     IF W-IDLEVNR-ALPHA = 'DL1YA'                                         
431500        MOVE '3445 ' TO W-IDLEVNR-ALPHA  END-IF                           
431600     IF W-IDLEVNR-ALPHA = 'DLH6A'                                         
431700        MOVE '3449 ' TO W-IDLEVNR-ALPHA  END-IF                           
431800     IF W-IDLEVNR-ALPHA = 'DLH7A'                                         
431900        MOVE '3463 ' TO W-IDLEVNR-ALPHA  END-IF                           
432000     IF W-IDLEVNR-ALPHA = 'DL1ZA'                                         
432100        MOVE '3470 ' TO W-IDLEVNR-ALPHA  END-IF                           
432200     IF W-IDLEVNR-ALPHA = 'DLJAA'                                         
432300        MOVE '3474 ' TO W-IDLEVNR-ALPHA  END-IF                           
432400     IF W-IDLEVNR-ALPHA = 'DLJBA'                                         
432500        MOVE '3482 ' TO W-IDLEVNR-ALPHA  END-IF                           
432600     IF W-IDLEVNR-ALPHA = 'DLJCA'                                         
432700        MOVE '3485 ' TO W-IDLEVNR-ALPHA  END-IF                           
432800     IF W-IDLEVNR-ALPHA = 'DLJDA'                                         
432900        MOVE '3487 ' TO W-IDLEVNR-ALPHA  END-IF                           
433000     IF W-IDLEVNR-ALPHA = 'DLJEA'                                         
433100        MOVE '3495 ' TO W-IDLEVNR-ALPHA  END-IF                           
433200     IF W-IDLEVNR-ALPHA = 'DL2BA'                                         
433300        MOVE '3505 ' TO W-IDLEVNR-ALPHA  END-IF                           
433400     IF W-IDLEVNR-ALPHA = 'BEFXA'                                         
433500        MOVE '3511 ' TO W-IDLEVNR-ALPHA  END-IF                           
433600     IF W-IDLEVNR-ALPHA = 'LYSDA'                                         
433700        MOVE '3537 ' TO W-IDLEVNR-ALPHA  END-IF                           
433800     IF W-IDLEVNR-ALPHA = 'DL2DA'                                         
433900        MOVE '3581 ' TO W-IDLEVNR-ALPHA  END-IF                           
434000     IF W-IDLEVNR-ALPHA = 'KYBHA'                                         
434100        MOVE '3634 ' TO W-IDLEVNR-ALPHA  END-IF                           
434200     IF W-IDLEVNR-ALPHA = 'DLJFA'                                         
434300        MOVE '3641 ' TO W-IDLEVNR-ALPHA  END-IF                           
434400     IF W-IDLEVNR-ALPHA = 'F745A'                                         
434500        MOVE '3660 ' TO W-IDLEVNR-ALPHA  END-IF                           
434600     IF W-IDLEVNR-ALPHA = 'E23VB'                                         
434700        MOVE '3663 ' TO W-IDLEVNR-ALPHA  END-IF                           
434800     IF W-IDLEVNR-ALPHA = 'DLJHA'                                         
434900        MOVE '3681 ' TO W-IDLEVNR-ALPHA  END-IF                           
435000     IF W-IDLEVNR-ALPHA = 'DL2EA'                                         
435100        MOVE '3723 ' TO W-IDLEVNR-ALPHA  END-IF                           
435200     IF W-IDLEVNR-ALPHA = 'DL2FA'                                         
435300        MOVE '3728 ' TO W-IDLEVNR-ALPHA  END-IF                           
435400     IF W-IDLEVNR-ALPHA = 'DL2GA'                                         
435500        MOVE '3732 ' TO W-IDLEVNR-ALPHA  END-IF                           
435600     IF W-IDLEVNR-ALPHA = 'CYMBD'                                         
435700        MOVE '3751 ' TO W-IDLEVNR-ALPHA  END-IF                           
435800     IF W-IDLEVNR-ALPHA = 'DL2HA'                                         
435900        MOVE '3771 ' TO W-IDLEVNR-ALPHA  END-IF                           
436000     IF W-IDLEVNR-ALPHA = 'DLJJA'                                         
436100        MOVE '3790 ' TO W-IDLEVNR-ALPHA  END-IF                           
436200     IF W-IDLEVNR-ALPHA = 'DL2JA'                                         
436300        MOVE '3793 ' TO W-IDLEVNR-ALPHA  END-IF                           
436400     IF W-IDLEVNR-ALPHA = 'DL2KA'                                         
436500        MOVE '3799 ' TO W-IDLEVNR-ALPHA  END-IF                           
436600     IF W-IDLEVNR-ALPHA = 'DL2LA'                                         
436700        MOVE '3814 ' TO W-IDLEVNR-ALPHA  END-IF                           
436800     IF W-IDLEVNR-ALPHA = 'DL2MA'                                         
436900        MOVE '3830 ' TO W-IDLEVNR-ALPHA  END-IF                           
437000     IF W-IDLEVNR-ALPHA = 'DL2NA'                                         
437100        MOVE '3833 ' TO W-IDLEVNR-ALPHA  END-IF                           
437200     IF W-IDLEVNR-ALPHA = 'DLJKA'                                         
437300        MOVE '3855 ' TO W-IDLEVNR-ALPHA  END-IF                           
437400     IF W-IDLEVNR-ALPHA = 'CP6JB'                                         
437500        MOVE '3861 ' TO W-IDLEVNR-ALPHA  END-IF                           
437600     IF W-IDLEVNR-ALPHA = 'DL2PA'                                         
437700        MOVE '3866 ' TO W-IDLEVNR-ALPHA  END-IF                           
437800     IF W-IDLEVNR-ALPHA = 'R57KA'                                         
437900        MOVE '3925 ' TO W-IDLEVNR-ALPHA  END-IF                           
438000     IF W-IDLEVNR-ALPHA = 'D0UCC'                                         
438100        MOVE '3933 ' TO W-IDLEVNR-ALPHA  END-IF                           
438200     IF W-IDLEVNR-ALPHA = 'DL4SA'                                         
438300        MOVE '3938 ' TO W-IDLEVNR-ALPHA  END-IF                           
438400     IF W-IDLEVNR-ALPHA = 'DLJMA'                                         
438500        MOVE '3941 ' TO W-IDLEVNR-ALPHA  END-IF                           
438600     IF W-IDLEVNR-ALPHA = 'DL4TA'                                         
438700        MOVE '3952 ' TO W-IDLEVNR-ALPHA  END-IF                           
438800     IF W-IDLEVNR-ALPHA = 'DLJPA'                                         
438900        MOVE '3957 ' TO W-IDLEVNR-ALPHA  END-IF                           
439000     IF W-IDLEVNR-ALPHA = 'DL5GB'                                         
439100        MOVE '3970 ' TO W-IDLEVNR-ALPHA  END-IF                           
439200     IF W-IDLEVNR-ALPHA = 'LEMWA'                                         
439300        MOVE '3977 ' TO W-IDLEVNR-ALPHA  END-IF                           
439400     IF W-IDLEVNR-ALPHA = 'S601E'                                         
439500        MOVE '4148 ' TO W-IDLEVNR-ALPHA  END-IF                           
439600     IF W-IDLEVNR-ALPHA = 'G8KDA'                                         
439700        MOVE '4256 ' TO W-IDLEVNR-ALPHA  END-IF                           
439800     IF W-IDLEVNR-ALPHA = 'LRT1A'                                         
439900        MOVE '4488 ' TO W-IDLEVNR-ALPHA  END-IF                           
440000     IF W-IDLEVNR-ALPHA = 'DL5HA'                                         
440100        MOVE '4542 ' TO W-IDLEVNR-ALPHA  END-IF                           
440200     IF W-IDLEVNR-ALPHA = 'CFJBA'                                         
440300        MOVE '4637 ' TO W-IDLEVNR-ALPHA  END-IF                           
440400     IF W-IDLEVNR-ALPHA = 'DLJRA'                                         
440500        MOVE '4700 ' TO W-IDLEVNR-ALPHA  END-IF                           
440600     IF W-IDLEVNR-ALPHA = 'JHZZA'                                         
440700        MOVE '4749 ' TO W-IDLEVNR-ALPHA  END-IF                           
440800     IF W-IDLEVNR-ALPHA = 'R6PFB'                                         
440900        MOVE '4845 ' TO W-IDLEVNR-ALPHA  END-IF                           
441000     IF W-IDLEVNR-ALPHA = 'D5Q3F'                                         
441100        MOVE '4934 ' TO W-IDLEVNR-ALPHA  END-IF                           
441200     IF W-IDLEVNR-ALPHA = 'DL6FA'                                         
441300        MOVE '4968 ' TO W-IDLEVNR-ALPHA  END-IF                           
441400     IF W-IDLEVNR-ALPHA = 'DL6JA'                                         
441500        MOVE '5122 ' TO W-IDLEVNR-ALPHA  END-IF                           
441600     IF W-IDLEVNR-ALPHA = 'LRVLA'                                         
441700        MOVE '5161 ' TO W-IDLEVNR-ALPHA  END-IF                           
441800     IF W-IDLEVNR-ALPHA = 'L3PGE'                                         
441900        MOVE '5182 ' TO W-IDLEVNR-ALPHA  END-IF                           
442000     IF W-IDLEVNR-ALPHA = 'D21XA'                                         
442100        MOVE '5233 ' TO W-IDLEVNR-ALPHA  END-IF                           
442200     IF W-IDLEVNR-ALPHA = 'C8W0A'                                         
442300        MOVE '5295 ' TO W-IDLEVNR-ALPHA  END-IF                           
442400     IF W-IDLEVNR-ALPHA = 'LHSYA'                                         
442500        MOVE '5354 ' TO W-IDLEVNR-ALPHA  END-IF                           
442600     IF W-IDLEVNR-ALPHA = 'AADLA'                                         
442700        MOVE '5436 ' TO W-IDLEVNR-ALPHA  END-IF                           
442800     IF W-IDLEVNR-ALPHA = 'DLJSA'                                         
442900        MOVE '5443 ' TO W-IDLEVNR-ALPHA  END-IF                           
443000     IF W-IDLEVNR-ALPHA = 'DLJTA'                                         
443100        MOVE '5637 ' TO W-IDLEVNR-ALPHA  END-IF                           
443200     IF W-IDLEVNR-ALPHA = 'D26YA'                                         
443300        MOVE '5671 ' TO W-IDLEVNR-ALPHA  END-IF                           
443400     IF W-IDLEVNR-ALPHA = 'MRZ4A'                                         
443500        MOVE '5678 ' TO W-IDLEVNR-ALPHA  END-IF                           
443600     IF W-IDLEVNR-ALPHA = 'DLK7A'                                         
443700        MOVE '6016 ' TO W-IDLEVNR-ALPHA  END-IF                           
443800     IF W-IDLEVNR-ALPHA = 'D1H7A'                                         
443900        MOVE '6024 ' TO W-IDLEVNR-ALPHA  END-IF                           
444000     IF W-IDLEVNR-ALPHA = 'DLLAA'                                         
444100        MOVE '6045 ' TO W-IDLEVNR-ALPHA  END-IF                           
444200     IF W-IDLEVNR-ALPHA = 'B42DA'                                         
444300        MOVE '6053 ' TO W-IDLEVNR-ALPHA  END-IF                           
444400     IF W-IDLEVNR-ALPHA = 'C8F4A'                                         
444500        MOVE '6061 ' TO W-IDLEVNR-ALPHA  END-IF                           
444600     IF W-IDLEVNR-ALPHA = 'EGX6K'                                         
444700        MOVE '6066 ' TO W-IDLEVNR-ALPHA  END-IF                           
444800     IF W-IDLEVNR-ALPHA = 'DLLBA'                                         
444900        MOVE '6069 ' TO W-IDLEVNR-ALPHA  END-IF                           
445000     IF W-IDLEVNR-ALPHA = 'B40WB'                                         
445100        MOVE '6080 ' TO W-IDLEVNR-ALPHA  END-IF                           
445200     IF W-IDLEVNR-ALPHA = 'D04DA'                                         
445300        MOVE '6098 ' TO W-IDLEVNR-ALPHA  END-IF                           
445400     IF W-IDLEVNR-ALPHA = 'D33QB'                                         
445500        MOVE '6120 ' TO W-IDLEVNR-ALPHA  END-IF                           
445600     IF W-IDLEVNR-ALPHA = 'D23JB'                                         
445700        MOVE '6151 ' TO W-IDLEVNR-ALPHA  END-IF                           
445800     IF W-IDLEVNR-ALPHA = 'DZK7A'                                         
445900        MOVE '6158 ' TO W-IDLEVNR-ALPHA  END-IF                           
446000     IF W-IDLEVNR-ALPHA = 'C8S2B'                                         
446100        MOVE '6202 ' TO W-IDLEVNR-ALPHA  END-IF                           
446200     IF W-IDLEVNR-ALPHA = 'D8NLJ'                                         
446300        MOVE '6228 ' TO W-IDLEVNR-ALPHA  END-IF                           
446400     IF W-IDLEVNR-ALPHA = 'DLLCA'                                         
446500        MOVE '6245 ' TO W-IDLEVNR-ALPHA  END-IF                           
446600     IF W-IDLEVNR-ALPHA = 'D0NNA'                                         
446700        MOVE '6269 ' TO W-IDLEVNR-ALPHA  END-IF                           
446800     IF W-IDLEVNR-ALPHA = 'D0SYA'                                         
446900        MOVE '6283 ' TO W-IDLEVNR-ALPHA  END-IF                           
447000*************************************************                         
447100     IF W-IDLEVNR-ALPHA = 'BQ8YA'                                         
447200        MOVE '80   ' TO W-IDLEVNR-ALPHA  END-IF                           
447300     IF W-IDLEVNR-ALPHA = 'BQ0FA'                                         
447400        MOVE '511  ' TO W-IDLEVNR-ALPHA  END-IF                           
447500     IF W-IDLEVNR-ALPHA = 'BQAGA'                                         
447600        MOVE '894  ' TO W-IDLEVNR-ALPHA  END-IF                           
447700     IF W-IDLEVNR-ALPHA = 'S5PQB'                                         
447800        MOVE '1345 ' TO W-IDLEVNR-ALPHA  END-IF                           
447900     IF W-IDLEVNR-ALPHA = 'LESLA'                                         
448000        MOVE '2333 ' TO W-IDLEVNR-ALPHA  END-IF                           
448100     IF W-IDLEVNR-ALPHA = 'BQ6MA'                                         
448200        MOVE '3050 ' TO W-IDLEVNR-ALPHA  END-IF                           
448300     IF W-IDLEVNR-ALPHA = 'L9GXB'                                         
448400        MOVE '3135 ' TO W-IDLEVNR-ALPHA  END-IF                           
448500     IF W-IDLEVNR-ALPHA = 'L9GXA'                                         
448600        MOVE '3594 ' TO W-IDLEVNR-ALPHA  END-IF                           
448700     IF W-IDLEVNR-ALPHA = 'C9A3A'                                         
448800        MOVE '3616 ' TO W-IDLEVNR-ALPHA  END-IF                           
448900     IF W-IDLEVNR-ALPHA = 'S356A'                                         
449000        MOVE '3752 ' TO W-IDLEVNR-ALPHA  END-IF                           
449100     IF W-IDLEVNR-ALPHA = 'AYSCB'                                         
449200        MOVE '3755 ' TO W-IDLEVNR-ALPHA  END-IF                           
449300     IF W-IDLEVNR-ALPHA = 'E23LA'                                         
449400        MOVE '3912 ' TO W-IDLEVNR-ALPHA  END-IF                           
449500     IF W-IDLEVNR-ALPHA = 'D04JA'                                         
449600        MOVE '4515 ' TO W-IDLEVNR-ALPHA  END-IF                           
449700     IF W-IDLEVNR-ALPHA = 'A628A'                                         
449800        MOVE '5049 ' TO W-IDLEVNR-ALPHA  END-IF                           
449900     IF W-IDLEVNR-ALPHA = 'C66SJ'                                         
450000        MOVE '5085 ' TO W-IDLEVNR-ALPHA  END-IF                           
450100     IF W-IDLEVNR-ALPHA = 'D3C3A'                                         
450200        MOVE '5093 ' TO W-IDLEVNR-ALPHA  END-IF                           
450300     IF W-IDLEVNR-ALPHA = 'C8V8A'                                         
450400        MOVE '5744 ' TO W-IDLEVNR-ALPHA  END-IF                           
450500     IF W-IDLEVNR-ALPHA = 'F4SWG'                                         
450600        MOVE '6022 ' TO W-IDLEVNR-ALPHA  END-IF                           
450700     IF W-IDLEVNR-ALPHA = 'AN3AA'                                         
450800        MOVE '6118 ' TO W-IDLEVNR-ALPHA  END-IF                           
450900     IF W-IDLEVNR-ALPHA = 'H518X'                                         
451000        MOVE '6215 ' TO W-IDLEVNR-ALPHA  END-IF                           
451100     IF W-IDLEVNR-ALPHA = 'CJ6EA'                                         
451200        MOVE '6345 ' TO W-IDLEVNR-ALPHA  END-IF                           
451300     IF W-IDLEVNR-ALPHA = 'B492E'                                         
451400        MOVE '6538 ' TO W-IDLEVNR-ALPHA  END-IF                           
451500     IF W-IDLEVNR-ALPHA = 'B492A'                                         
451600        MOVE '6587 ' TO W-IDLEVNR-ALPHA  END-IF                           
451700     IF W-IDLEVNR-ALPHA = 'C8W2A'                                         
451800        MOVE '7213 ' TO W-IDLEVNR-ALPHA  END-IF                           
451900     IF W-IDLEVNR-ALPHA = 'CFT5A'                                         
452000        MOVE '7609 ' TO W-IDLEVNR-ALPHA  END-IF                           
452100     IF W-IDLEVNR-ALPHA = 'BPLDA'                                         
452200        MOVE '10131' TO W-IDLEVNR-ALPHA  END-IF                           
452300     IF W-IDLEVNR-ALPHA = 'BPLDD'                                         
452400        MOVE '10138' TO W-IDLEVNR-ALPHA  END-IF                           
452500     IF W-IDLEVNR-ALPHA = 'CRK8A'                                         
452600        MOVE '11099' TO W-IDLEVNR-ALPHA  END-IF                           
452700     IF W-IDLEVNR-ALPHA = 'BPLDC'                                         
452800        MOVE '13633' TO W-IDLEVNR-ALPHA  END-IF                           
452900     IF W-IDLEVNR-ALPHA = 'C7U7A'                                         
453000        MOVE '14493' TO W-IDLEVNR-ALPHA  END-IF                           
453100     IF W-IDLEVNR-ALPHA = 'C685B'                                         
453200        MOVE '16213' TO W-IDLEVNR-ALPHA  END-IF                           
453300     IF W-IDLEVNR-ALPHA = 'R19YA'                                         
453400        MOVE '17779' TO W-IDLEVNR-ALPHA  END-IF                           
453500     IF W-IDLEVNR-ALPHA = 'BTTTA'                                         
453600        MOVE '23071' TO W-IDLEVNR-ALPHA  END-IF                           
453700     IF W-IDLEVNR-ALPHA = 'CYVBA'                                         
453800        MOVE '23937' TO W-IDLEVNR-ALPHA  END-IF                           
453900     IF W-IDLEVNR-ALPHA = 'DSKFA'                                         
454000        MOVE '26013' TO W-IDLEVNR-ALPHA  END-IF                           
454100*************************************************                         
454200     IF W-IDLEVNR-ALPHA = 'E520A'                                         
454300        MOVE '6293 ' TO W-IDLEVNR-ALPHA  END-IF                           
454400     IF W-IDLEVNR-ALPHA = 'DL6MA'                                         
454500        MOVE '6307 ' TO W-IDLEVNR-ALPHA  END-IF                           
454600     IF W-IDLEVNR-ALPHA = 'H8Z2A'                                         
454700        MOVE '6326 ' TO W-IDLEVNR-ALPHA  END-IF                           
454800     IF W-IDLEVNR-ALPHA = 'DL6PA'                                         
454900        MOVE '6357 ' TO W-IDLEVNR-ALPHA  END-IF                           
455000     IF W-IDLEVNR-ALPHA = 'DMZCA'                                         
455100        MOVE '6410 ' TO W-IDLEVNR-ALPHA  END-IF                           
455200     IF W-IDLEVNR-ALPHA = 'U0VSA'                                         
455300        MOVE '6425 ' TO W-IDLEVNR-ALPHA  END-IF                           
455400     IF W-IDLEVNR-ALPHA = 'C91WA'                                         
455500        MOVE '6429 ' TO W-IDLEVNR-ALPHA  END-IF                           
455600     IF W-IDLEVNR-ALPHA = 'DLLFA'                                         
455700        MOVE '6430 ' TO W-IDLEVNR-ALPHA  END-IF                           
455800     IF W-IDLEVNR-ALPHA = 'V04BA'                                         
455900        MOVE '6510 ' TO W-IDLEVNR-ALPHA  END-IF                           
456000     IF W-IDLEVNR-ALPHA = 'C7F4A'                                         
456100        MOVE '6524 ' TO W-IDLEVNR-ALPHA  END-IF                           
456200     IF W-IDLEVNR-ALPHA = 'DLLGA'                                         
456300        MOVE '6562 ' TO W-IDLEVNR-ALPHA  END-IF                           
456400     IF W-IDLEVNR-ALPHA = 'L9SYA'                                         
456500        MOVE '6595 ' TO W-IDLEVNR-ALPHA  END-IF                           
456600     IF W-IDLEVNR-ALPHA = 'DLLJA'                                         
456700        MOVE '6603 ' TO W-IDLEVNR-ALPHA  END-IF                           
456800     IF W-IDLEVNR-ALPHA = 'DLLKA'                                         
456900        MOVE '6641 ' TO W-IDLEVNR-ALPHA  END-IF                           
457000     IF W-IDLEVNR-ALPHA = 'DLLLA'                                         
457100        MOVE '6643 ' TO W-IDLEVNR-ALPHA  END-IF                           
457200     IF W-IDLEVNR-ALPHA = 'CKBRA'                                         
457300        MOVE '6656 ' TO W-IDLEVNR-ALPHA  END-IF                           
457400     IF W-IDLEVNR-ALPHA = 'JFYXA'                                         
457500        MOVE '6691 ' TO W-IDLEVNR-ALPHA  END-IF                           
457600     IF W-IDLEVNR-ALPHA = 'C8Q3A'                                         
457700        MOVE '6698 ' TO W-IDLEVNR-ALPHA  END-IF                           
457800     IF W-IDLEVNR-ALPHA = 'DL6QA'                                         
457900        MOVE '6714 ' TO W-IDLEVNR-ALPHA  END-IF                           
458000     IF W-IDLEVNR-ALPHA = 'DLLNA'                                         
458100        MOVE '6761 ' TO W-IDLEVNR-ALPHA  END-IF                           
458200     IF W-IDLEVNR-ALPHA = 'V0TAA'                                         
458300        MOVE '6783 ' TO W-IDLEVNR-ALPHA  END-IF                           
458400     IF W-IDLEVNR-ALPHA = 'C92KA'                                         
458500        MOVE '6812 ' TO W-IDLEVNR-ALPHA  END-IF                           
458600     IF W-IDLEVNR-ALPHA = 'D25WB'                                         
458700        MOVE '6825 ' TO W-IDLEVNR-ALPHA  END-IF                           
458800     IF W-IDLEVNR-ALPHA = 'P4WSA'                                         
458900        MOVE '6847 ' TO W-IDLEVNR-ALPHA  END-IF                           
459000     IF W-IDLEVNR-ALPHA = 'A708A'                                         
459100        MOVE '6869 ' TO W-IDLEVNR-ALPHA  END-IF                           
459200     IF W-IDLEVNR-ALPHA = 'S3C3A'                                         
459300        MOVE '6870 ' TO W-IDLEVNR-ALPHA  END-IF                           
459400     IF W-IDLEVNR-ALPHA = 'DL6TA'                                         
459500        MOVE '6885 ' TO W-IDLEVNR-ALPHA  END-IF                           
459600     IF W-IDLEVNR-ALPHA = 'DL6VA'                                         
459700        MOVE '6892 ' TO W-IDLEVNR-ALPHA  END-IF                           
459800     IF W-IDLEVNR-ALPHA = 'CN5FA'                                         
459900        MOVE '6899 ' TO W-IDLEVNR-ALPHA  END-IF                           
460000     IF W-IDLEVNR-ALPHA = 'N82PA'                                         
460100        MOVE '7038 ' TO W-IDLEVNR-ALPHA  END-IF                           
460200     IF W-IDLEVNR-ALPHA = 'DLLPA'                                         
460300        MOVE '7106 ' TO W-IDLEVNR-ALPHA  END-IF                           
460400     IF W-IDLEVNR-ALPHA = 'HCR1A'                                         
460500        MOVE '7115 ' TO W-IDLEVNR-ALPHA  END-IF                           
460600     IF W-IDLEVNR-ALPHA = 'DLLQA'                                         
460700        MOVE '7132 ' TO W-IDLEVNR-ALPHA  END-IF                           
460800     IF W-IDLEVNR-ALPHA = 'MTJFA'                                         
460900        MOVE '7134 ' TO W-IDLEVNR-ALPHA  END-IF                           
461000     IF W-IDLEVNR-ALPHA = 'MWAJB'                                         
461100        MOVE '7229 ' TO W-IDLEVNR-ALPHA  END-IF                           
461200     IF W-IDLEVNR-ALPHA = 'DL6WA'                                         
461300        MOVE '7235 ' TO W-IDLEVNR-ALPHA  END-IF                           
461400     IF W-IDLEVNR-ALPHA = 'DLLRA'                                         
461500        MOVE '7357 ' TO W-IDLEVNR-ALPHA  END-IF                           
461600     IF W-IDLEVNR-ALPHA = 'DL6YA'                                         
461700        MOVE '7831 ' TO W-IDLEVNR-ALPHA  END-IF                           
461800     IF W-IDLEVNR-ALPHA = 'MHQSA'                                         
461900        MOVE '7955 ' TO W-IDLEVNR-ALPHA  END-IF                           
462000     IF W-IDLEVNR-ALPHA = 'BLLMA'                                         
462100        MOVE '8004 ' TO W-IDLEVNR-ALPHA  END-IF                           
462200     IF W-IDLEVNR-ALPHA = 'BJW6A'                                         
462300        MOVE '8010 ' TO W-IDLEVNR-ALPHA  END-IF                           
462400     IF W-IDLEVNR-ALPHA = 'BKXUA'                                         
462500        MOVE '8023 ' TO W-IDLEVNR-ALPHA  END-IF                           
462600     IF W-IDLEVNR-ALPHA = 'BLMJA'                                         
462700        MOVE '8061 ' TO W-IDLEVNR-ALPHA  END-IF                           
462800     IF W-IDLEVNR-ALPHA = 'DLLTA'                                         
462900        MOVE '8086 ' TO W-IDLEVNR-ALPHA  END-IF                           
463000     IF W-IDLEVNR-ALPHA = 'BKYNA'                                         
463100        MOVE '8094 ' TO W-IDLEVNR-ALPHA  END-IF                           
463200     IF W-IDLEVNR-ALPHA = 'BK5LA'                                         
463300        MOVE '8103 ' TO W-IDLEVNR-ALPHA  END-IF                           
463400     IF W-IDLEVNR-ALPHA = 'DLLUA'                                         
463500        MOVE '8107 ' TO W-IDLEVNR-ALPHA  END-IF                           
463600     IF W-IDLEVNR-ALPHA = 'DL6ZA'                                         
463700        MOVE '8123 ' TO W-IDLEVNR-ALPHA  END-IF                           
463800     IF W-IDLEVNR-ALPHA = 'D13DA'                                         
463900        MOVE '8138 ' TO W-IDLEVNR-ALPHA  END-IF                           
464000     IF W-IDLEVNR-ALPHA = 'BSN7A'                                         
464100        MOVE '8150 ' TO W-IDLEVNR-ALPHA  END-IF                           
464200     IF W-IDLEVNR-ALPHA = 'DLLVA'                                         
464300        MOVE '8151 ' TO W-IDLEVNR-ALPHA  END-IF                           
464400     IF W-IDLEVNR-ALPHA = 'S6LPA'                                         
464500        MOVE '8168 ' TO W-IDLEVNR-ALPHA  END-IF                           
464600     IF W-IDLEVNR-ALPHA = 'CZTVA'                                         
464700        MOVE '8181 ' TO W-IDLEVNR-ALPHA  END-IF                           
464800     IF W-IDLEVNR-ALPHA = 'BSFPA'                                         
464900        MOVE '8190 ' TO W-IDLEVNR-ALPHA  END-IF                           
465000     IF W-IDLEVNR-ALPHA = 'DLLWA'                                         
465100        MOVE '8192 ' TO W-IDLEVNR-ALPHA  END-IF                           
465200     IF W-IDLEVNR-ALPHA = 'BSKXA'                                         
465300        MOVE '8199 ' TO W-IDLEVNR-ALPHA  END-IF                           
465400     IF W-IDLEVNR-ALPHA = 'DL7AA'                                         
465500        MOVE '8203 ' TO W-IDLEVNR-ALPHA  END-IF                           
465600     IF W-IDLEVNR-ALPHA = 'DLLXA'                                         
465700        MOVE '8212 ' TO W-IDLEVNR-ALPHA  END-IF                           
465800     IF W-IDLEVNR-ALPHA = 'S98JA'                                         
465900        MOVE '8216 ' TO W-IDLEVNR-ALPHA  END-IF                           
466000     IF W-IDLEVNR-ALPHA = 'DLLYA'                                         
466100        MOVE '8233 ' TO W-IDLEVNR-ALPHA  END-IF                           
466200     IF W-IDLEVNR-ALPHA = 'DLLZA'                                         
466300        MOVE '8234 ' TO W-IDLEVNR-ALPHA  END-IF                           
466400     IF W-IDLEVNR-ALPHA = 'BAG3A'                                         
466500        MOVE '8860 ' TO W-IDLEVNR-ALPHA  END-IF                           
466600     IF W-IDLEVNR-ALPHA = 'S7YJA'                                         
466700        MOVE '10139' TO W-IDLEVNR-ALPHA  END-IF                           
466800     IF W-IDLEVNR-ALPHA = 'DL7CA'                                         
466900        MOVE '10355' TO W-IDLEVNR-ALPHA  END-IF                           
467000     IF W-IDLEVNR-ALPHA = 'DLL0A'                                         
467100        MOVE '10803' TO W-IDLEVNR-ALPHA  END-IF                           
467200     IF W-IDLEVNR-ALPHA = 'CRJ8A'                                         
467300        MOVE '10910' TO W-IDLEVNR-ALPHA  END-IF                           
467400     IF W-IDLEVNR-ALPHA = 'Q5FPA'                                         
467500        MOVE '11117' TO W-IDLEVNR-ALPHA  END-IF                           
467600     IF W-IDLEVNR-ALPHA = 'D1K4E'                                         
467700        MOVE '12089' TO W-IDLEVNR-ALPHA  END-IF                           
467800     IF W-IDLEVNR-ALPHA = 'BA7ZA'                                         
467900        MOVE '12098' TO W-IDLEVNR-ALPHA  END-IF                           
468000     IF W-IDLEVNR-ALPHA = 'DLL5A'                                         
468100        MOVE '13311' TO W-IDLEVNR-ALPHA  END-IF                           
468200     IF W-IDLEVNR-ALPHA = 'DL7GA'                                         
468300        MOVE '13344' TO W-IDLEVNR-ALPHA  END-IF                           
468400     IF W-IDLEVNR-ALPHA = 'DL7HA'                                         
468500        MOVE '13346' TO W-IDLEVNR-ALPHA  END-IF                           
468600     IF W-IDLEVNR-ALPHA = 'DL7JA'                                         
468700        MOVE '13348' TO W-IDLEVNR-ALPHA  END-IF                           
468800     IF W-IDLEVNR-ALPHA = 'C62JC'                                         
468900        MOVE '13362' TO W-IDLEVNR-ALPHA  END-IF                           
469000     IF W-IDLEVNR-ALPHA = 'DLMEA'                                         
469100        MOVE '13374' TO W-IDLEVNR-ALPHA  END-IF                           
469200     IF W-IDLEVNR-ALPHA = 'DLMFA'                                         
469300        MOVE '13375' TO W-IDLEVNR-ALPHA  END-IF                           
469400     IF W-IDLEVNR-ALPHA = 'DLMGA'                                         
469500        MOVE '13376' TO W-IDLEVNR-ALPHA  END-IF                           
469600*************************************************                         
469700     IF W-IDLEVNR-ALPHA = 'DLMHA'                                         
469800        MOVE '13379' TO W-IDLEVNR-ALPHA  END-IF                           
469900     IF W-IDLEVNR-ALPHA = 'D3W5A'                                         
470000        MOVE '13381' TO W-IDLEVNR-ALPHA  END-IF                           
470100     IF W-IDLEVNR-ALPHA = 'CT3NA'                                         
470200        MOVE '13383' TO W-IDLEVNR-ALPHA  END-IF                           
470300     IF W-IDLEVNR-ALPHA = 'DLMJA'                                         
470400        MOVE '13384' TO W-IDLEVNR-ALPHA  END-IF                           
470500     IF W-IDLEVNR-ALPHA = 'CGECA'                                         
470600        MOVE '13385' TO W-IDLEVNR-ALPHA  END-IF                           
470700     IF W-IDLEVNR-ALPHA = 'DLMLA'                                         
470800        MOVE '13390' TO W-IDLEVNR-ALPHA  END-IF                           
470900     IF W-IDLEVNR-ALPHA = 'DLMMA'                                         
471000        MOVE '13392' TO W-IDLEVNR-ALPHA  END-IF                           
471100     IF W-IDLEVNR-ALPHA = 'DLMPA'                                         
471200        MOVE '13401' TO W-IDLEVNR-ALPHA  END-IF                           
471300     IF W-IDLEVNR-ALPHA = 'DL7MA'                                         
471400        MOVE '13402' TO W-IDLEVNR-ALPHA  END-IF                           
471500     IF W-IDLEVNR-ALPHA = 'CFJDA'                                         
471600        MOVE '13403' TO W-IDLEVNR-ALPHA  END-IF                           
471700     IF W-IDLEVNR-ALPHA = 'DLMSA'                                         
471800        MOVE '13408' TO W-IDLEVNR-ALPHA  END-IF                           
471900     IF W-IDLEVNR-ALPHA = 'DLMTA'                                         
472000        MOVE '13409' TO W-IDLEVNR-ALPHA  END-IF                           
472100     IF W-IDLEVNR-ALPHA = 'DL7NA'                                         
472200        MOVE '13411' TO W-IDLEVNR-ALPHA  END-IF                           
472300     IF W-IDLEVNR-ALPHA = 'DLMUA'                                         
472400        MOVE '13416' TO W-IDLEVNR-ALPHA  END-IF                           
472500     IF W-IDLEVNR-ALPHA = 'DLMVA'                                         
472600        MOVE '13463' TO W-IDLEVNR-ALPHA  END-IF                           
472700     IF W-IDLEVNR-ALPHA = 'DLMWA'                                         
472800        MOVE '13465' TO W-IDLEVNR-ALPHA  END-IF                           
472900     IF W-IDLEVNR-ALPHA = 'BQMJA'                                         
473000        MOVE '13467' TO W-IDLEVNR-ALPHA  END-IF                           
473100     IF W-IDLEVNR-ALPHA = 'DL7RA'                                         
473200        MOVE '13475' TO W-IDLEVNR-ALPHA  END-IF                           
473300     IF W-IDLEVNR-ALPHA = 'AMAAC'                                         
473400        MOVE '13529' TO W-IDLEVNR-ALPHA  END-IF                           
473500     IF W-IDLEVNR-ALPHA = 'LRZ7A'                                         
473600        MOVE '13561' TO W-IDLEVNR-ALPHA  END-IF                           
473700     IF W-IDLEVNR-ALPHA = 'DL7VA'                                         
473800        MOVE '13565' TO W-IDLEVNR-ALPHA  END-IF                           
473900     IF W-IDLEVNR-ALPHA = 'BVNUC'                                         
474000        MOVE '13576' TO W-IDLEVNR-ALPHA  END-IF                           
474100     IF W-IDLEVNR-ALPHA = 'DLMYA'                                         
474200        MOVE '13621' TO W-IDLEVNR-ALPHA  END-IF                           
474300     IF W-IDLEVNR-ALPHA = 'DL7XA'                                         
474400        MOVE '13776' TO W-IDLEVNR-ALPHA  END-IF                           
474500     IF W-IDLEVNR-ALPHA = 'Q99AA'                                         
474600        MOVE '14483' TO W-IDLEVNR-ALPHA  END-IF                           
474700     IF W-IDLEVNR-ALPHA = 'CFH6A'                                         
474800        MOVE '14578' TO W-IDLEVNR-ALPHA  END-IF                           
474900     IF W-IDLEVNR-ALPHA = 'DLMZA'                                         
475000        MOVE '14592' TO W-IDLEVNR-ALPHA  END-IF                           
475100     IF W-IDLEVNR-ALPHA = 'DLM1A'                                         
475200        MOVE '14921' TO W-IDLEVNR-ALPHA  END-IF                           
475300     IF W-IDLEVNR-ALPHA = 'DLM3A'                                         
475400        MOVE '14935' TO W-IDLEVNR-ALPHA  END-IF                           
475500     IF W-IDLEVNR-ALPHA = 'DLNBA'                                         
475600        MOVE '14935' TO W-IDLEVNR-ALPHA  END-IF                           
475700     IF W-IDLEVNR-ALPHA = 'MHLTA'                                         
475800        MOVE '15065' TO W-IDLEVNR-ALPHA  END-IF                           
475900     IF W-IDLEVNR-ALPHA = 'DLNCA'                                         
476000        MOVE '15180' TO W-IDLEVNR-ALPHA  END-IF                           
476100     IF W-IDLEVNR-ALPHA = 'DLNDA'                                         
476200        MOVE '15207' TO W-IDLEVNR-ALPHA  END-IF                           
476300     IF W-IDLEVNR-ALPHA = 'N6WEA'                                         
476400        MOVE '15227' TO W-IDLEVNR-ALPHA  END-IF                           
476500     IF W-IDLEVNR-ALPHA = 'N7381'                                         
476600        MOVE '15256' TO W-IDLEVNR-ALPHA  END-IF                           
476700     IF W-IDLEVNR-ALPHA = 'DLNEA'                                         
476800        MOVE '15260' TO W-IDLEVNR-ALPHA  END-IF                           
476900     IF W-IDLEVNR-ALPHA = 'DLNFA'                                         
477000        MOVE '15266' TO W-IDLEVNR-ALPHA  END-IF                           
477100     IF W-IDLEVNR-ALPHA = 'DLNGA'                                         
477200        MOVE '15310' TO W-IDLEVNR-ALPHA  END-IF                           
477300     IF W-IDLEVNR-ALPHA = 'DLNHA'                                         
477400        MOVE '15322' TO W-IDLEVNR-ALPHA  END-IF                           
477500     IF W-IDLEVNR-ALPHA = 'DL8BA'                                         
477600        MOVE '15325' TO W-IDLEVNR-ALPHA  END-IF                           
477700     IF W-IDLEVNR-ALPHA = 'DL8CA'                                         
477800        MOVE '16087' TO W-IDLEVNR-ALPHA  END-IF                           
477900     IF W-IDLEVNR-ALPHA = 'S3MZA'                                         
478000        MOVE '16090' TO W-IDLEVNR-ALPHA  END-IF                           
478100     IF W-IDLEVNR-ALPHA = 'CN5PA'                                         
478200        MOVE '16116' TO W-IDLEVNR-ALPHA  END-IF                           
478300     IF W-IDLEVNR-ALPHA = 'T93AB'                                         
478400        MOVE '16123' TO W-IDLEVNR-ALPHA  END-IF                           
478500     IF W-IDLEVNR-ALPHA = 'K760B'                                         
478600        MOVE '16367' TO W-IDLEVNR-ALPHA  END-IF                           
478700     IF W-IDLEVNR-ALPHA = 'S5S2A'                                         
478800        MOVE '16466' TO W-IDLEVNR-ALPHA  END-IF                           
478900     IF W-IDLEVNR-ALPHA = 'D3R3A'                                         
479000        MOVE '17782' TO W-IDLEVNR-ALPHA  END-IF                           
479100     IF W-IDLEVNR-ALPHA = 'C9G4A'                                         
479200        MOVE '17784' TO W-IDLEVNR-ALPHA  END-IF                           
479300     IF W-IDLEVNR-ALPHA = 'DLNLA'                                         
479400        MOVE '17785' TO W-IDLEVNR-ALPHA  END-IF                           
479500     IF W-IDLEVNR-ALPHA = 'DLNMA'                                         
479600        MOVE '17792' TO W-IDLEVNR-ALPHA  END-IF                           
479700     IF W-IDLEVNR-ALPHA = 'DLNNA'                                         
479800        MOVE '17813' TO W-IDLEVNR-ALPHA  END-IF                           
479900     IF W-IDLEVNR-ALPHA = 'BKPQA'                                         
480000        MOVE '18000' TO W-IDLEVNR-ALPHA  END-IF                           
480100     IF W-IDLEVNR-ALPHA = 'BKHYA'                                         
480200        MOVE '18012' TO W-IDLEVNR-ALPHA  END-IF                           
480300     IF W-IDLEVNR-ALPHA = 'BSK0A'                                         
480400        MOVE '18060' TO W-IDLEVNR-ALPHA  END-IF                           
480500     IF W-IDLEVNR-ALPHA = 'DLNQA'                                         
480600        MOVE '18067' TO W-IDLEVNR-ALPHA  END-IF                           
480700     IF W-IDLEVNR-ALPHA = 'DLNRA'                                         
480800        MOVE '18075' TO W-IDLEVNR-ALPHA  END-IF                           
480900     IF W-IDLEVNR-ALPHA = 'CXC8A'                                         
481000        MOVE '18120' TO W-IDLEVNR-ALPHA  END-IF                           
481100     IF W-IDLEVNR-ALPHA = 'DLNSA'                                         
481200        MOVE '18977' TO W-IDLEVNR-ALPHA  END-IF                           
481300     IF W-IDLEVNR-ALPHA = 'DLNTA'                                         
481400        MOVE '19052' TO W-IDLEVNR-ALPHA  END-IF                           
481500     IF W-IDLEVNR-ALPHA = 'BK4FA'                                         
481600        MOVE '19235' TO W-IDLEVNR-ALPHA  END-IF                           
481900     IF W-IDLEVNR-ALPHA = 'AYGHA'                                         
482000        MOVE '20895' TO W-IDLEVNR-ALPHA  END-IF                           
482100     IF W-IDLEVNR-ALPHA = 'CECDA'                                         
482200        MOVE '21580' TO W-IDLEVNR-ALPHA  END-IF                           
482300     IF W-IDLEVNR-ALPHA = 'CGSCA'                                         
482400        MOVE '23939' TO W-IDLEVNR-ALPHA  END-IF                           
482500     IF W-IDLEVNR-ALPHA = 'DL8EA'                                         
482600        MOVE '23941' TO W-IDLEVNR-ALPHA  END-IF                           
482700     IF W-IDLEVNR-ALPHA = 'ATPUB'                                         
482800        MOVE '24633' TO W-IDLEVNR-ALPHA  END-IF                           
482900     IF W-IDLEVNR-ALPHA = 'DL8FA'                                         
483000        MOVE '24837' TO W-IDLEVNR-ALPHA  END-IF                           
483100     IF W-IDLEVNR-ALPHA = 'DLNWA'                                         
483200        MOVE '25012' TO W-IDLEVNR-ALPHA  END-IF                           
483300     IF W-IDLEVNR-ALPHA = 'BLWQA'                                         
483400        MOVE '25745' TO W-IDLEVNR-ALPHA  END-IF                           
483500     IF W-IDLEVNR-ALPHA = 'DN6EA'                                         
483600        MOVE '25955' TO W-IDLEVNR-ALPHA  END-IF                           
483700     IF W-IDLEVNR-ALPHA = 'A224A'                                         
483800        MOVE '50049' TO W-IDLEVNR-ALPHA  END-IF                           
483900     IF W-IDLEVNR-ALPHA = 'G8VUE'                                         
484000        MOVE '55005' TO W-IDLEVNR-ALPHA  END-IF                           
484100     IF W-IDLEVNR-ALPHA = 'CZ6AB'                                         
484200        MOVE '80096' TO W-IDLEVNR-ALPHA  END-IF                           
484300*************************************************                         
484400     IF W-IDLEVNR-ALPHA = 'BQYJA'                                         
484500        MOVE '87   ' TO W-IDLEVNR-ALPHA  END-IF                           
484600     IF W-IDLEVNR-ALPHA = 'N81ZA'                                         
484700        MOVE '1186 ' TO W-IDLEVNR-ALPHA  END-IF                           
484800     IF W-IDLEVNR-ALPHA = 'S6NKA'                                         
484900        MOVE '1447 ' TO W-IDLEVNR-ALPHA  END-IF                           
485000     IF W-IDLEVNR-ALPHA = 'BVNQA'                                         
485100        MOVE '2318 ' TO W-IDLEVNR-ALPHA  END-IF                           
485200     IF W-IDLEVNR-ALPHA = 'BQEKA'                                         
485300        MOVE '2356 ' TO W-IDLEVNR-ALPHA  END-IF                           
485400     IF W-IDLEVNR-ALPHA = 'BWKTA'                                         
485500        MOVE '3169 ' TO W-IDLEVNR-ALPHA  END-IF                           
485600     IF W-IDLEVNR-ALPHA = 'T2FBB'                                         
485700        MOVE '3803 ' TO W-IDLEVNR-ALPHA  END-IF                           
485800     IF W-IDLEVNR-ALPHA = 'D0DAA'                                         
485900        MOVE '3894 ' TO W-IDLEVNR-ALPHA  END-IF                           
486000     IF W-IDLEVNR-ALPHA = 'S17VA'                                         
486100        MOVE '3989 ' TO W-IDLEVNR-ALPHA  END-IF                           
486200     IF W-IDLEVNR-ALPHA = 'J3ZLA'                                         
486300        MOVE '4618 ' TO W-IDLEVNR-ALPHA  END-IF                           
486400     IF W-IDLEVNR-ALPHA = 'R8LDA'                                         
486500        MOVE '5212 ' TO W-IDLEVNR-ALPHA  END-IF                           
486600     IF W-IDLEVNR-ALPHA = 'V0QEA'                                         
486700        MOVE '5684 ' TO W-IDLEVNR-ALPHA  END-IF                           
486800     IF W-IDLEVNR-ALPHA = 'G769B'                                         
486900        MOVE '6063 ' TO W-IDLEVNR-ALPHA  END-IF                           
487000     IF W-IDLEVNR-ALPHA = 'C99ZA'                                         
487100        MOVE '6119 ' TO W-IDLEVNR-ALPHA  END-IF                           
487200     IF W-IDLEVNR-ALPHA = 'D1E4A'                                         
487300        MOVE '6166 ' TO W-IDLEVNR-ALPHA  END-IF                           
487400     IF W-IDLEVNR-ALPHA = 'D2H8A'                                         
487500        MOVE '6437 ' TO W-IDLEVNR-ALPHA  END-IF                           
487600     IF W-IDLEVNR-ALPHA = 'G952A'                                         
487700        MOVE '6515 ' TO W-IDLEVNR-ALPHA  END-IF                           
487800     IF W-IDLEVNR-ALPHA = 'Q520A'                                         
487900        MOVE '6522 ' TO W-IDLEVNR-ALPHA  END-IF                           
488000     IF W-IDLEVNR-ALPHA = 'EFR5A'                                         
488100        MOVE '6719 ' TO W-IDLEVNR-ALPHA  END-IF                           
488200     IF W-IDLEVNR-ALPHA = 'L905D'                                         
488300        MOVE '6771 ' TO W-IDLEVNR-ALPHA  END-IF                           
488400     IF W-IDLEVNR-ALPHA = 'BQ7RA'                                         
488500        MOVE '6828 ' TO W-IDLEVNR-ALPHA  END-IF                           
488600     IF W-IDLEVNR-ALPHA = 'U5P7A'                                         
488700        MOVE '6836 ' TO W-IDLEVNR-ALPHA  END-IF                           
488800     IF W-IDLEVNR-ALPHA = 'U1LFD'                                         
488900        MOVE '6857 ' TO W-IDLEVNR-ALPHA  END-IF                           
489000     IF W-IDLEVNR-ALPHA = 'ABYHA'                                         
489100        MOVE '6950 ' TO W-IDLEVNR-ALPHA  END-IF                           
489200     IF W-IDLEVNR-ALPHA = 'BPXDA'                                         
489300        MOVE '7040 ' TO W-IDLEVNR-ALPHA  END-IF                           
489400     IF W-IDLEVNR-ALPHA = 'T4UVA'                                         
489500        MOVE '7258 ' TO W-IDLEVNR-ALPHA  END-IF                           
489600     IF W-IDLEVNR-ALPHA = 'J5VHA'                                         
489700        MOVE '7919 ' TO W-IDLEVNR-ALPHA  END-IF                           
489800     IF W-IDLEVNR-ALPHA = 'N5LQA'                                         
489900        MOVE '7927 ' TO W-IDLEVNR-ALPHA  END-IF                           
490000     IF W-IDLEVNR-ALPHA = 'BQJDD'                                         
490100        MOVE '7954 ' TO W-IDLEVNR-ALPHA  END-IF                           
490200     IF W-IDLEVNR-ALPHA = 'BKAMB'                                         
490300        MOVE '10056' TO W-IDLEVNR-ALPHA  END-IF                           
490400     IF W-IDLEVNR-ALPHA = 'BPEVA'                                         
490500        MOVE '11369' TO W-IDLEVNR-ALPHA  END-IF                           
490600     IF W-IDLEVNR-ALPHA = 'T3DVA'                                         
490700        MOVE '13619' TO W-IDLEVNR-ALPHA  END-IF                           
490800     IF W-IDLEVNR-ALPHA = 'D797B'                                         
490900        MOVE '16096' TO W-IDLEVNR-ALPHA  END-IF                           
491000     IF W-IDLEVNR-ALPHA = 'ALTXA'                                         
491100        MOVE '16238' TO W-IDLEVNR-ALPHA  END-IF                           
491200     IF W-IDLEVNR-ALPHA = 'B45JC'                                         
491300        MOVE '16275' TO W-IDLEVNR-ALPHA  END-IF                           
491400     IF W-IDLEVNR-ALPHA = 'DP5UA'                                         
491500        MOVE '16301' TO W-IDLEVNR-ALPHA  END-IF                           
491600     IF W-IDLEVNR-ALPHA = 'BH0ZA'                                         
491700        MOVE '17253' TO W-IDLEVNR-ALPHA  END-IF                           
491800     IF W-IDLEVNR-ALPHA = 'H82NE'                                         
491900        MOVE '17825' TO W-IDLEVNR-ALPHA  END-IF                           
492000     IF W-IDLEVNR-ALPHA = 'H137P'                                         
492100        MOVE '17826' TO W-IDLEVNR-ALPHA  END-IF                           
492200     IF W-IDLEVNR-ALPHA = 'N508A'                                         
492300        MOVE '18127' TO W-IDLEVNR-ALPHA  END-IF                           
492400     IF W-IDLEVNR-ALPHA = 'AMMCB'                                         
492500        MOVE '19959' TO W-IDLEVNR-ALPHA  END-IF                           
492600     IF W-IDLEVNR-ALPHA = 'CF9JA'                                         
492700        MOVE '23271' TO W-IDLEVNR-ALPHA  END-IF                           
492800     IF W-IDLEVNR-ALPHA = 'CZQ7A'                                         
492900        MOVE '23914' TO W-IDLEVNR-ALPHA  END-IF                           
493000     IF W-IDLEVNR-ALPHA = 'H5PLA'                                         
493100        MOVE '23947' TO W-IDLEVNR-ALPHA  END-IF                           
493200     IF W-IDLEVNR-ALPHA = 'CU8WB'                                         
493300        MOVE '24374' TO W-IDLEVNR-ALPHA  END-IF                           
493400     IF W-IDLEVNR-ALPHA = 'AYSPA'                                         
493500        MOVE '24630' TO W-IDLEVNR-ALPHA  END-IF                           
493600     IF W-IDLEVNR-ALPHA = 'BHD8A'                                         
493700        MOVE '25763' TO W-IDLEVNR-ALPHA  END-IF                           
493800     IF W-IDLEVNR-ALPHA = 'DLKCA'                                         
493900        MOVE '1525 ' TO W-IDLEVNR-ALPHA  END-IF                           
494000     IF W-IDLEVNR-ALPHA = 'A76VA'                                         
494100        MOVE '1592 ' TO W-IDLEVNR-ALPHA  END-IF                           
494200     IF W-IDLEVNR-ALPHA = 'D33HE'                                         
494300        MOVE '3963 ' TO W-IDLEVNR-ALPHA  END-IF                           
494400     IF W-IDLEVNR-ALPHA = 'C69HA'                                         
494500        MOVE '6386 ' TO W-IDLEVNR-ALPHA  END-IF                           
494600     IF W-IDLEVNR-ALPHA = 'D0MNA'                                         
494700        MOVE '6808 ' TO W-IDLEVNR-ALPHA  END-IF                           
494800     IF W-IDLEVNR-ALPHA = 'C97RA'                                         
494900        MOVE '6914 ' TO W-IDLEVNR-ALPHA  END-IF                           
495000     IF W-IDLEVNR-ALPHA = 'BK2EA'                                         
495100        MOVE '8186 ' TO W-IDLEVNR-ALPHA  END-IF                           
495200     IF W-IDLEVNR-ALPHA = 'BQ8SA'                                         
495300        MOVE '10221' TO W-IDLEVNR-ALPHA  END-IF                           
495400     IF W-IDLEVNR-ALPHA = 'BUWJA'                                         
495500        MOVE '10511' TO W-IDLEVNR-ALPHA  END-IF                           
495600     IF W-IDLEVNR-ALPHA = 'BEJZA'                                         
495700        MOVE '10659' TO W-IDLEVNR-ALPHA  END-IF                           
495800     IF W-IDLEVNR-ALPHA = 'BNSRA'                                         
495900        MOVE '10813' TO W-IDLEVNR-ALPHA  END-IF                           
496000     IF W-IDLEVNR-ALPHA = 'D0BEB'                                         
496100        MOVE '11332' TO W-IDLEVNR-ALPHA  END-IF                           
496200     IF W-IDLEVNR-ALPHA = 'DL7SA'                                         
496300        MOVE '13484' TO W-IDLEVNR-ALPHA  END-IF                           
496400     IF W-IDLEVNR-ALPHA = 'D0MNE'                                         
496500        MOVE '13508' TO W-IDLEVNR-ALPHA  END-IF                           
496600     IF W-IDLEVNR-ALPHA = 'N0KFA'                                         
496700        MOVE '14465' TO W-IDLEVNR-ALPHA  END-IF                           
496800     IF W-IDLEVNR-ALPHA = 'MWZFA'                                         
496900        MOVE '14658' TO W-IDLEVNR-ALPHA  END-IF                           
497000     IF W-IDLEVNR-ALPHA = 'AYG1A'                                         
497100        MOVE '14756' TO W-IDLEVNR-ALPHA  END-IF                           
497200     IF W-IDLEVNR-ALPHA = 'BQ9RA'                                         
497300        MOVE '15053' TO W-IDLEVNR-ALPHA  END-IF                           
497400     IF W-IDLEVNR-ALPHA = 'D0MND'                                         
497500        MOVE '16049' TO W-IDLEVNR-ALPHA  END-IF                           
497600     IF W-IDLEVNR-ALPHA = 'D0MNF'                                         
497700        MOVE '16075' TO W-IDLEVNR-ALPHA  END-IF                           
497800     IF W-IDLEVNR-ALPHA = 'D33HA'                                         
497900        MOVE '17789' TO W-IDLEVNR-ALPHA  END-IF                           
498000     IF W-IDLEVNR-ALPHA = 'D0MNG'                                         
498100        MOVE '17945' TO W-IDLEVNR-ALPHA  END-IF                           
498200     IF W-IDLEVNR-ALPHA = 'S4HWB'                                         
498300        MOVE '19739' TO W-IDLEVNR-ALPHA  END-IF                           
498400     IF W-IDLEVNR-ALPHA = 'AJ2AA'                                         
498500        MOVE '21004' TO W-IDLEVNR-ALPHA  END-IF                           
498600     IF W-IDLEVNR-ALPHA = 'C8Q8A'                                         
498700        MOVE '22389' TO W-IDLEVNR-ALPHA  END-IF                           
498800     IF W-IDLEVNR-ALPHA = 'C94MA'                                         
498900        MOVE '23335' TO W-IDLEVNR-ALPHA  END-IF                           
499000     IF W-IDLEVNR-ALPHA = 'F477B'                                         
499100        MOVE '23862' TO W-IDLEVNR-ALPHA  END-IF                           
499200     IF W-IDLEVNR-ALPHA = 'P8TWA'                                         
499300        MOVE '25916' TO W-IDLEVNR-ALPHA  END-IF                           
499400     IF W-IDLEVNR-ALPHA = 'DNK9A'                                         
499500        MOVE '25931' TO W-IDLEVNR-ALPHA  END-IF                           
499600*************************************************                         
499700*************************************************                         
499800     IF W-IDLEVNR-ALPHA = 'BUPDC'                                         
499900        MOVE '31138' TO W-IDLEVNR-ALPHA  END-IF                           
500000     IF W-IDLEVNR-ALPHA = 'BUPDD'                                         
500100        MOVE '41138' TO W-IDLEVNR-ALPHA  END-IF                           
500200     IF W-IDLEVNR-ALPHA = 'L8K5H'                                         
500300        MOVE '11138' TO W-IDLEVNR-ALPHA  END-IF                           
500400*************************************************                         
500500     IF W-IDLEVNR-ALPHA = 'Q99AA'                                         
500600        MOVE '14483' TO W-IDLEVNR-ALPHA  END-IF                           
500700     IF W-IDLEVNR-ALPHA = 'D38KF'                                         
500800        MOVE '16044' TO W-IDLEVNR-ALPHA  END-IF                           
500900     IF W-IDLEVNR-ALPHA = 'S3MZA'                                         
501000        MOVE '16090' TO W-IDLEVNR-ALPHA  END-IF                           
501100     IF W-IDLEVNR-ALPHA = 'C95YC'                                         
501200        MOVE '16284' TO W-IDLEVNR-ALPHA  END-IF                           
501300     IF W-IDLEVNR-ALPHA = 'C95YB'                                         
501400        MOVE '16378' TO W-IDLEVNR-ALPHA  END-IF                           
501500     IF W-IDLEVNR-ALPHA = 'C95YJ'                                         
501600        MOVE '20501' TO W-IDLEVNR-ALPHA  END-IF                           
501700     IF W-IDLEVNR-ALPHA = 'D02KA'                                         
501800        MOVE '26942' TO W-IDLEVNR-ALPHA  END-IF                           
501900     IF W-IDLEVNR-ALPHA = 'D0UCA'                                         
502000        MOVE '3742 ' TO W-IDLEVNR-ALPHA  END-IF                           
502100     IF W-IDLEVNR-ALPHA = 'B4V0A'                                         
502200        MOVE '6685 ' TO W-IDLEVNR-ALPHA  END-IF                           
502300     IF W-IDLEVNR-ALPHA = 'C95YA'                                         
502400        MOVE '6942 ' TO W-IDLEVNR-ALPHA  END-IF                           
502500     IF W-IDLEVNR-ALPHA = 'S63MA'                                         
502600        MOVE '7683 ' TO W-IDLEVNR-ALPHA  END-IF                           
502700     IF W-IDLEVNR-ALPHA = 'BPFNB'                                         
502800        MOVE '34666' TO W-IDLEVNR-ALPHA  END-IF                           
502900     IF W-IDLEVNR-ALPHA = 'CDCHA'                                         
503000        MOVE '10030' TO W-IDLEVNR-ALPHA  END-IF                           
503100     IF W-IDLEVNR-ALPHA = 'L8K5W'                                         
503200        MOVE '10181' TO W-IDLEVNR-ALPHA  END-IF                           
503300     IF W-IDLEVNR-ALPHA = 'CDV5A'                                         
503400        MOVE '10347' TO W-IDLEVNR-ALPHA  END-IF                           
503500     IF W-IDLEVNR-ALPHA = 'S4MZD'                                         
503600        MOVE '11518' TO W-IDLEVNR-ALPHA  END-IF                           
503700     IF W-IDLEVNR-ALPHA = 'S1XMC'                                         
503800        MOVE '11753' TO W-IDLEVNR-ALPHA  END-IF                           
503900     IF W-IDLEVNR-ALPHA = 'ALRHA'                                         
504000        MOVE '12539' TO W-IDLEVNR-ALPHA  END-IF                           
504100     IF W-IDLEVNR-ALPHA = 'BQ9FA'                                         
504200        MOVE '13444' TO W-IDLEVNR-ALPHA  END-IF                           
504300     IF W-IDLEVNR-ALPHA = 'S1XMD'                                         
504400        MOVE '13543' TO W-IDLEVNR-ALPHA  END-IF                           
504500     IF W-IDLEVNR-ALPHA = 'BUPDA'                                         
504600        MOVE '13635' TO W-IDLEVNR-ALPHA  END-IF                           
504700     IF W-IDLEVNR-ALPHA = 'BQ9PA'                                         
504800        MOVE '14599' TO W-IDLEVNR-ALPHA  END-IF                           
504900     IF W-IDLEVNR-ALPHA = 'P9J4A'                                         
505000        MOVE '14924' TO W-IDLEVNR-ALPHA  END-IF                           
505100     IF W-IDLEVNR-ALPHA = 'D11JA'                                         
505200        MOVE '15442' TO W-IDLEVNR-ALPHA  END-IF                           
505300     IF W-IDLEVNR-ALPHA = 'Q42PA'                                         
505400        MOVE '16062' TO W-IDLEVNR-ALPHA  END-IF                           
505500     IF W-IDLEVNR-ALPHA = 'B4X4A'                                         
505600        MOVE '16071' TO W-IDLEVNR-ALPHA  END-IF                           
505700     IF W-IDLEVNR-ALPHA = 'BPGQA'                                         
505800        MOVE '171  ' TO W-IDLEVNR-ALPHA  END-IF                           
505900     IF W-IDLEVNR-ALPHA = 'DJPSA'                                         
506000        MOVE '17255' TO W-IDLEVNR-ALPHA  END-IF                           
506100     IF W-IDLEVNR-ALPHA = 'AZYXA'                                         
506200        MOVE '18609' TO W-IDLEVNR-ALPHA  END-IF                           
506300     IF W-IDLEVNR-ALPHA = 'R0PRA'                                         
506400        MOVE '19733' TO W-IDLEVNR-ALPHA  END-IF                           
506500     IF W-IDLEVNR-ALPHA = 'BPGQD'                                         
506600        MOVE '2036 ' TO W-IDLEVNR-ALPHA  END-IF                           
506700     IF W-IDLEVNR-ALPHA = 'N2D2E'                                         
506800        MOVE '21756' TO W-IDLEVNR-ALPHA  END-IF                           
506900     IF W-IDLEVNR-ALPHA = 'CTYHA'                                         
507000        MOVE '22396' TO W-IDLEVNR-ALPHA  END-IF                           
507100     IF W-IDLEVNR-ALPHA = 'N81QA'                                         
507200        MOVE '230  ' TO W-IDLEVNR-ALPHA  END-IF                           
507300     IF W-IDLEVNR-ALPHA = 'BJHYA'                                         
507400        MOVE '23245' TO W-IDLEVNR-ALPHA  END-IF                           
507500     IF W-IDLEVNR-ALPHA = 'BQEKA'                                         
507600        MOVE '2356 ' TO W-IDLEVNR-ALPHA  END-IF                           
507700     IF W-IDLEVNR-ALPHA = 'BRTUA'                                         
507800        MOVE '23705' TO W-IDLEVNR-ALPHA  END-IF                           
507900     IF W-IDLEVNR-ALPHA = 'CLDQA'                                         
508000        MOVE '23799' TO W-IDLEVNR-ALPHA  END-IF                           
508100     IF W-IDLEVNR-ALPHA = 'BUPDA'                                         
508200        MOVE '24010' TO W-IDLEVNR-ALPHA  END-IF                           
508300     IF W-IDLEVNR-ALPHA = 'CLMPA'                                         
508400        MOVE '25669' TO W-IDLEVNR-ALPHA  END-IF                           
508500     IF W-IDLEVNR-ALPHA = 'BQ9PB'                                         
508600        MOVE '25691' TO W-IDLEVNR-ALPHA  END-IF                           
508700     IF W-IDLEVNR-ALPHA = 'DA5NA'                                         
508800        MOVE '25764' TO W-IDLEVNR-ALPHA  END-IF                           
508900     IF W-IDLEVNR-ALPHA = 'AQHKA'                                         
509000        MOVE '25874' TO W-IDLEVNR-ALPHA  END-IF                           
509100     IF W-IDLEVNR-ALPHA = 'L8K50'                                         
509200        MOVE '25890' TO W-IDLEVNR-ALPHA  END-IF                           
509300     IF W-IDLEVNR-ALPHA = 'L8K5Z'                                         
509400        MOVE '25891' TO W-IDLEVNR-ALPHA  END-IF                           
509500     IF W-IDLEVNR-ALPHA = 'L8K5Y'                                         
509600        MOVE '25892' TO W-IDLEVNR-ALPHA  END-IF                           
509700     IF W-IDLEVNR-ALPHA = 'L8K5X'                                         
509800        MOVE '25893' TO W-IDLEVNR-ALPHA  END-IF                           
509900     IF W-IDLEVNR-ALPHA = 'DNH1A'                                         
510000        MOVE '25930' TO W-IDLEVNR-ALPHA  END-IF                           
510100     IF W-IDLEVNR-ALPHA = 'CQPRA'                                         
510200        MOVE '2662 ' TO W-IDLEVNR-ALPHA  END-IF                           
510300     IF W-IDLEVNR-ALPHA = 'D0N8A'                                         
510400        MOVE '3350 ' TO W-IDLEVNR-ALPHA  END-IF                           
510500     IF W-IDLEVNR-ALPHA = 'S1XMB'                                         
510600        MOVE '3656 ' TO W-IDLEVNR-ALPHA  END-IF                           
510700     IF W-IDLEVNR-ALPHA = 'S1XMA'                                         
510800        MOVE '3964 ' TO W-IDLEVNR-ALPHA  END-IF                           
510900     IF W-IDLEVNR-ALPHA = 'BPGQB'                                         
511000        MOVE '402  ' TO W-IDLEVNR-ALPHA  END-IF                           
511100     IF W-IDLEVNR-ALPHA = 'S41GF'                                         
511200        MOVE '4234 ' TO W-IDLEVNR-ALPHA  END-IF                           
511300     IF W-IDLEVNR-ALPHA = 'C62JA'                                         
511400        MOVE '5191 ' TO W-IDLEVNR-ALPHA  END-IF                           
511500     IF W-IDLEVNR-ALPHA = 'C9S2A'                                         
511600        MOVE '5670 ' TO W-IDLEVNR-ALPHA  END-IF                           
511700     IF W-IDLEVNR-ALPHA = 'B050B'                                         
511800        MOVE '5674 ' TO W-IDLEVNR-ALPHA  END-IF                           
511900     IF W-IDLEVNR-ALPHA = 'AZYXB'                                         
512000        MOVE '6229 ' TO W-IDLEVNR-ALPHA  END-IF                           
512100     IF W-IDLEVNR-ALPHA = 'CFUDB'                                         
512200        MOVE '6268 ' TO W-IDLEVNR-ALPHA  END-IF                           
512300     IF W-IDLEVNR-ALPHA = 'D3P7B'                                         
512400        MOVE '6282 ' TO W-IDLEVNR-ALPHA  END-IF                           
512500     IF W-IDLEVNR-ALPHA = 'C5408'                                         
512600        MOVE '63058' TO W-IDLEVNR-ALPHA  END-IF                           
512700     IF W-IDLEVNR-ALPHA = 'D1K4A'                                         
512800        MOVE '6598 ' TO W-IDLEVNR-ALPHA  END-IF                           
512900     IF W-IDLEVNR-ALPHA = 'R151A'                                         
513000        MOVE '6666 ' TO W-IDLEVNR-ALPHA  END-IF                           
513100     IF W-IDLEVNR-ALPHA = 'S4MZA'                                         
513200        MOVE '6678 ' TO W-IDLEVNR-ALPHA  END-IF                           
513300     IF W-IDLEVNR-ALPHA = 'ENB6A'                                         
513400        MOVE '6809 ' TO W-IDLEVNR-ALPHA  END-IF                           
513500     IF W-IDLEVNR-ALPHA = 'ABYHA'                                         
513600        MOVE '6950 ' TO W-IDLEVNR-ALPHA  END-IF                           
513700     IF W-IDLEVNR-ALPHA = 'C86HA'                                         
513800        MOVE '6972 ' TO W-IDLEVNR-ALPHA  END-IF                           
513900     IF W-IDLEVNR-ALPHA = 'N2D2A'                                         
514000        MOVE '7847 ' TO W-IDLEVNR-ALPHA  END-IF                           
514100     IF W-IDLEVNR-ALPHA = 'D0GWA'                                         
514200        MOVE '7915 ' TO W-IDLEVNR-ALPHA  END-IF                           
514300     IF W-IDLEVNR-ALPHA = 'BP3JB'                                         
514400        MOVE '843  ' TO W-IDLEVNR-ALPHA  END-IF                           
514500     IF W-IDLEVNR-ALPHA = 'C9P1B'                                         
514600        MOVE '10490' TO W-IDLEVNR-ALPHA  END-IF                           
514700     IF W-IDLEVNR-ALPHA = 'CTX0A'                                         
514800        MOVE '11062' TO W-IDLEVNR-ALPHA  END-IF                           
514900     IF W-IDLEVNR-ALPHA = 'BP8AA'                                         
515000        MOVE '11086' TO W-IDLEVNR-ALPHA  END-IF                           
515100     IF W-IDLEVNR-ALPHA = 'BUWLA'                                         
515200        MOVE '11403' TO W-IDLEVNR-ALPHA  END-IF                           
515300     IF W-IDLEVNR-ALPHA = 'BQH8A'                                         
515400        MOVE '1148 ' TO W-IDLEVNR-ALPHA  END-IF                           
515500     IF W-IDLEVNR-ALPHA = 'CN5NA'                                         
515600        MOVE '11708' TO W-IDLEVNR-ALPHA  END-IF                           
515700     IF W-IDLEVNR-ALPHA = 'BJQNA'                                         
515800        MOVE '12055' TO W-IDLEVNR-ALPHA  END-IF                           
515900     IF W-IDLEVNR-ALPHA = 'Q98ZA'                                         
516000        MOVE '13241' TO W-IDLEVNR-ALPHA  END-IF                           
516100     IF W-IDLEVNR-ALPHA = 'CFUBA'                                         
516200        MOVE '13517' TO W-IDLEVNR-ALPHA  END-IF                           
516300     IF W-IDLEVNR-ALPHA = 'BJPMA'                                         
516400        MOVE '13527' TO W-IDLEVNR-ALPHA  END-IF                           
516500     IF W-IDLEVNR-ALPHA = 'BJPMB'                                         
516600        MOVE '13603' TO W-IDLEVNR-ALPHA  END-IF                           
516700     IF W-IDLEVNR-ALPHA = 'BC3EA'                                         
516800        MOVE '14668' TO W-IDLEVNR-ALPHA  END-IF                           
516900     IF W-IDLEVNR-ALPHA = 'BQ2ZA'                                         
517000        MOVE '1480 ' TO W-IDLEVNR-ALPHA  END-IF                           
517100     IF W-IDLEVNR-ALPHA = 'BQ8DA'                                         
517200        MOVE '1528 ' TO W-IDLEVNR-ALPHA  END-IF                           
517300     IF W-IDLEVNR-ALPHA = 'R9Q4A'                                         
517400        MOVE '16081' TO W-IDLEVNR-ALPHA  END-IF                           
517500     IF W-IDLEVNR-ALPHA = 'J17KA'                                         
517600        MOVE '16358' TO W-IDLEVNR-ALPHA  END-IF                           
517700     IF W-IDLEVNR-ALPHA = 'R3U8A'                                         
517800        MOVE '16371' TO W-IDLEVNR-ALPHA  END-IF                           
517900     IF W-IDLEVNR-ALPHA = 'D0N0E'                                         
518000        MOVE '16735' TO W-IDLEVNR-ALPHA  END-IF                           
518100     IF W-IDLEVNR-ALPHA = 'BQYKB'                                         
518200        MOVE '1689 ' TO W-IDLEVNR-ALPHA  END-IF                           
518300     IF W-IDLEVNR-ALPHA = 'BJPMD'                                         
518400        MOVE '21327' TO W-IDLEVNR-ALPHA  END-IF                           
518500     IF W-IDLEVNR-ALPHA = 'N7WWA'                                         
518600        MOVE '214  ' TO W-IDLEVNR-ALPHA  END-IF                           
518700     IF W-IDLEVNR-ALPHA = 'BPF3A'                                         
518800        MOVE '2213 ' TO W-IDLEVNR-ALPHA  END-IF                           
518900     IF W-IDLEVNR-ALPHA = 'BQ5QA'                                         
519000        MOVE '2222 ' TO W-IDLEVNR-ALPHA  END-IF                           
519100     IF W-IDLEVNR-ALPHA = 'BLNMA'                                         
519200        MOVE '2227 ' TO W-IDLEVNR-ALPHA  END-IF                           
519300     IF W-IDLEVNR-ALPHA = 'H7G6A'                                         
519400        MOVE '2312 ' TO W-IDLEVNR-ALPHA  END-IF                           
519500     IF W-IDLEVNR-ALPHA = 'AXQ1A'                                         
519600        MOVE '24660' TO W-IDLEVNR-ALPHA  END-IF                           
519700     IF W-IDLEVNR-ALPHA = 'AQYSA'                                         
519800        MOVE '25759' TO W-IDLEVNR-ALPHA  END-IF                           
519900     IF W-IDLEVNR-ALPHA = 'BQ6LA'                                         
520000        MOVE '25775' TO W-IDLEVNR-ALPHA  END-IF                           
520100     IF W-IDLEVNR-ALPHA = 'DDCLA'                                         
520200        MOVE '25815' TO W-IDLEVNR-ALPHA  END-IF                           
520300     IF W-IDLEVNR-ALPHA = 'CW0XA'                                         
520400        MOVE '26031' TO W-IDLEVNR-ALPHA  END-IF                           
520500     IF W-IDLEVNR-ALPHA = 'BQ6FA'                                         
520600        MOVE '2634 ' TO W-IDLEVNR-ALPHA  END-IF                           
520700     IF W-IDLEVNR-ALPHA = 'BQYLA'                                         
520800        MOVE '266  ' TO W-IDLEVNR-ALPHA  END-IF                           
520900     IF W-IDLEVNR-ALPHA = 'S9B0B'                                         
521000        MOVE '2670 ' TO W-IDLEVNR-ALPHA  END-IF                           
521100     IF W-IDLEVNR-ALPHA = 'BQ6LA'                                         
521200        MOVE '3043 ' TO W-IDLEVNR-ALPHA  END-IF                           
521300     IF W-IDLEVNR-ALPHA = 'CL3YA'                                         
521400        MOVE '3054 ' TO W-IDLEVNR-ALPHA  END-IF                           
521500     IF W-IDLEVNR-ALPHA = 'BKNKA'                                         
521600        MOVE '3096 ' TO W-IDLEVNR-ALPHA  END-IF                           
521700     IF W-IDLEVNR-ALPHA = 'B34SG'                                         
521800        MOVE '3653 ' TO W-IDLEVNR-ALPHA  END-IF                           
521900     IF W-IDLEVNR-ALPHA = 'AKUMA'                                         
522000        MOVE '3666 ' TO W-IDLEVNR-ALPHA  END-IF                           
522100     IF W-IDLEVNR-ALPHA = 'BQ6TA'                                         
522200        MOVE '3832 ' TO W-IDLEVNR-ALPHA  END-IF                           
522300     IF W-IDLEVNR-ALPHA = 'BP3CA'                                         
522400        MOVE '393  ' TO W-IDLEVNR-ALPHA  END-IF                           
522500     IF W-IDLEVNR-ALPHA = 'BQ6UA'                                         
522600        MOVE '3934 ' TO W-IDLEVNR-ALPHA  END-IF                           
522700     IF W-IDLEVNR-ALPHA = 'Q3MXA'                                         
522800        MOVE '4235 ' TO W-IDLEVNR-ALPHA  END-IF                           
522900     IF W-IDLEVNR-ALPHA = 'A7G7A'                                         
523000        MOVE '4253 ' TO W-IDLEVNR-ALPHA  END-IF                           
523100     IF W-IDLEVNR-ALPHA = 'S9B0A'                                         
523200        MOVE '4597 ' TO W-IDLEVNR-ALPHA  END-IF                           
523300     IF W-IDLEVNR-ALPHA = 'E7R9D'                                         
523400        MOVE '4985 ' TO W-IDLEVNR-ALPHA  END-IF                           
523500     IF W-IDLEVNR-ALPHA = 'BJ6BA'                                         
523600        MOVE '507  ' TO W-IDLEVNR-ALPHA  END-IF                           
523700     IF W-IDLEVNR-ALPHA = 'CN4SA'                                         
523800        MOVE '510  ' TO W-IDLEVNR-ALPHA  END-IF                           
523900     IF W-IDLEVNR-ALPHA = 'E9VKB'                                         
524000        MOVE '5177 ' TO W-IDLEVNR-ALPHA  END-IF                           
524100     IF W-IDLEVNR-ALPHA = 'BLHTA'                                         
524200        MOVE '521  ' TO W-IDLEVNR-ALPHA  END-IF                           
524300     IF W-IDLEVNR-ALPHA = 'F610B'                                         
524400        MOVE '5269 ' TO W-IDLEVNR-ALPHA  END-IF                           
524500     IF W-IDLEVNR-ALPHA = 'C8D2A'                                         
524600        MOVE '6057 ' TO W-IDLEVNR-ALPHA  END-IF                           
524700     IF W-IDLEVNR-ALPHA = 'LBEKA'                                         
524800        MOVE '6154 ' TO W-IDLEVNR-ALPHA  END-IF                           
524900     IF W-IDLEVNR-ALPHA = 'V022A'                                         
525000        MOVE '6175 ' TO W-IDLEVNR-ALPHA  END-IF                           
525100     IF W-IDLEVNR-ALPHA = 'ADSMA'                                         
525200        MOVE '6199 ' TO W-IDLEVNR-ALPHA  END-IF                           
525300     IF W-IDLEVNR-ALPHA = 'D04BB'                                         
525400        MOVE '6244 ' TO W-IDLEVNR-ALPHA  END-IF                           
525500     IF W-IDLEVNR-ALPHA = 'S2Z6B'                                         
525600        MOVE '6330 ' TO W-IDLEVNR-ALPHA  END-IF                           
525700     IF W-IDLEVNR-ALPHA = 'B40XA'                                         
525800        MOVE '6474 ' TO W-IDLEVNR-ALPHA  END-IF                           
525900     IF W-IDLEVNR-ALPHA = 'BQ7QA'                                         
526000        MOVE '6649 ' TO W-IDLEVNR-ALPHA  END-IF                           
526100     IF W-IDLEVNR-ALPHA = 'D0N0A'                                         
526200        MOVE '6849 ' TO W-IDLEVNR-ALPHA  END-IF                           
526300     IF W-IDLEVNR-ALPHA = 'B34SA'                                         
526400        MOVE '7785 ' TO W-IDLEVNR-ALPHA  END-IF                           
526500     IF W-IDLEVNR-ALPHA = 'BUTZA'                                         
526600        MOVE '7958 ' TO W-IDLEVNR-ALPHA  END-IF                           
526700     IF W-IDLEVNR-ALPHA = 'BQYKA'                                         
526800        MOVE '88   ' TO W-IDLEVNR-ALPHA  END-IF                           
526900     IF W-IDLEVNR-ALPHA = 'BKNQA'                                         
527000        MOVE '8888 ' TO W-IDLEVNR-ALPHA  END-IF                           
527100     IF W-IDLEVNR-ALPHA = 'BQ0GA'                                         
527200        MOVE '996  ' TO W-IDLEVNR-ALPHA  END-IF                           
527300*************************************************                         
527400     IF W-IDLEVNR-ALPHA = 'CFNFA'                                         
527500        MOVE '848  ' TO W-IDLEVNR-ALPHA  END-IF                           
527600     IF W-IDLEVNR-ALPHA = 'BX3PA'                                         
527700        MOVE '883  ' TO W-IDLEVNR-ALPHA  END-IF                           
527800     IF W-IDLEVNR-ALPHA = 'BJQRA'                                         
527900        MOVE '1203 ' TO W-IDLEVNR-ALPHA  END-IF                           
528000     IF W-IDLEVNR-ALPHA = 'BQ5SA'                                         
528100        MOVE '2346 ' TO W-IDLEVNR-ALPHA  END-IF                           
528200     IF W-IDLEVNR-ALPHA = 'BQEBA'                                         
528300        MOVE '2363 ' TO W-IDLEVNR-ALPHA  END-IF                           
528400     IF W-IDLEVNR-ALPHA = 'BQ5TA'                                         
528500        MOVE '2368 ' TO W-IDLEVNR-ALPHA  END-IF                           
528600     IF W-IDLEVNR-ALPHA = 'BP3EB'                                         
528700        MOVE '2419 ' TO W-IDLEVNR-ALPHA  END-IF                           
528800     IF W-IDLEVNR-ALPHA = 'AY2QA'                                         
528900        MOVE '3036 ' TO W-IDLEVNR-ALPHA  END-IF                           
529000     IF W-IDLEVNR-ALPHA = 'K3D7A'                                         
529100        MOVE '3712 ' TO W-IDLEVNR-ALPHA  END-IF                           
529200     IF W-IDLEVNR-ALPHA = 'X448A'                                         
529300        MOVE '5249 ' TO W-IDLEVNR-ALPHA  END-IF                           
529400     IF W-IDLEVNR-ALPHA = 'D22FA'                                         
529500        MOVE '6040 ' TO W-IDLEVNR-ALPHA  END-IF                           
529600     IF W-IDLEVNR-ALPHA = 'KKM2A'                                         
529700        MOVE '6174 ' TO W-IDLEVNR-ALPHA  END-IF                           
529800     IF W-IDLEVNR-ALPHA = 'BCK8A'                                         
529900        MOVE '6385 ' TO W-IDLEVNR-ALPHA  END-IF                           
530000     IF W-IDLEVNR-ALPHA = 'T9UZA'                                         
530100        MOVE '6485 ' TO W-IDLEVNR-ALPHA  END-IF                           
530200     IF W-IDLEVNR-ALPHA = 'BQ7NA'                                         
530300        MOVE '6513 ' TO W-IDLEVNR-ALPHA  END-IF                           
530400     IF W-IDLEVNR-ALPHA = 'M279A'                                         
530500        MOVE '6600 ' TO W-IDLEVNR-ALPHA  END-IF                           
530600     IF W-IDLEVNR-ALPHA = 'KTY7A'                                         
530700        MOVE '6614 ' TO W-IDLEVNR-ALPHA  END-IF                           
530800     IF W-IDLEVNR-ALPHA = 'D21UA'                                         
530900        MOVE '6701 ' TO W-IDLEVNR-ALPHA  END-IF                           
531000     IF W-IDLEVNR-ALPHA = 'CFTZA'                                         
531100        MOVE '6952 ' TO W-IDLEVNR-ALPHA  END-IF                           
531200     IF W-IDLEVNR-ALPHA = 'D3T6A'                                         
531300        MOVE '7042 ' TO W-IDLEVNR-ALPHA  END-IF                           
531400     IF W-IDLEVNR-ALPHA = 'BQ7TA'                                         
531500        MOVE '7052 ' TO W-IDLEVNR-ALPHA  END-IF                           
531600     IF W-IDLEVNR-ALPHA = 'R742A'                                         
531700        MOVE '7179 ' TO W-IDLEVNR-ALPHA  END-IF                           
531800     IF W-IDLEVNR-ALPHA = 'CXA0A'                                         
531900        MOVE '7189 ' TO W-IDLEVNR-ALPHA  END-IF                           
532000     IF W-IDLEVNR-ALPHA = 'CXA1A'                                         
532100        MOVE '7190 ' TO W-IDLEVNR-ALPHA  END-IF                           
532200     IF W-IDLEVNR-ALPHA = 'Q725B'                                         
532300        MOVE '7246 ' TO W-IDLEVNR-ALPHA  END-IF                           
532400     IF W-IDLEVNR-ALPHA = 'S35LA'                                         
532500        MOVE '7253 ' TO W-IDLEVNR-ALPHA  END-IF                           
532600     IF W-IDLEVNR-ALPHA = 'G1UHS'                                         
532700        MOVE '7255 ' TO W-IDLEVNR-ALPHA  END-IF                           
532800     IF W-IDLEVNR-ALPHA = 'BP3KA'                                         
532900        MOVE '7470 ' TO W-IDLEVNR-ALPHA  END-IF                           
533000     IF W-IDLEVNR-ALPHA = 'CEFPA'                                         
533100        MOVE '7787 ' TO W-IDLEVNR-ALPHA  END-IF                           
533200     IF W-IDLEVNR-ALPHA = 'CDHVA'                                         
533300        MOVE '10024' TO W-IDLEVNR-ALPHA  END-IF                           
533400     IF W-IDLEVNR-ALPHA = 'BP3EA'                                         
533500        MOVE '10121' TO W-IDLEVNR-ALPHA  END-IF                           
533600     IF W-IDLEVNR-ALPHA = 'CXMTA'                                         
533700        MOVE '10290' TO W-IDLEVNR-ALPHA  END-IF                           
533800     IF W-IDLEVNR-ALPHA = 'BJQRD'                                         
533900        MOVE '10324' TO W-IDLEVNR-ALPHA  END-IF                           
534000     IF W-IDLEVNR-ALPHA = 'C7C5A'                                         
534100        MOVE '10510' TO W-IDLEVNR-ALPHA  END-IF                           
534200     IF W-IDLEVNR-ALPHA = 'BJQRB'                                         
534300        MOVE '10657' TO W-IDLEVNR-ALPHA  END-IF                           
534400     IF W-IDLEVNR-ALPHA = 'V4B1B'                                         
534500        MOVE '11100' TO W-IDLEVNR-ALPHA  END-IF                           
534600     IF W-IDLEVNR-ALPHA = 'G8VTA'                                         
534700        MOVE '11101' TO W-IDLEVNR-ALPHA  END-IF                           
534800     IF W-IDLEVNR-ALPHA = 'BCK8B'                                         
534900        MOVE '11137' TO W-IDLEVNR-ALPHA  END-IF                           
535000     IF W-IDLEVNR-ALPHA = 'BJQRC'                                         
535100        MOVE '11139' TO W-IDLEVNR-ALPHA  END-IF                           
535200     IF W-IDLEVNR-ALPHA = 'BQEBB'                                         
535300        MOVE '11155' TO W-IDLEVNR-ALPHA  END-IF                           
535400     IF W-IDLEVNR-ALPHA = 'E521H'                                         
535500        MOVE '11388' TO W-IDLEVNR-ALPHA  END-IF                           
535600     IF W-IDLEVNR-ALPHA = 'B491E'                                         
535700        MOVE '12637' TO W-IDLEVNR-ALPHA  END-IF                           
535800     IF W-IDLEVNR-ALPHA = 'T3WQA'                                         
535900        MOVE '13537' TO W-IDLEVNR-ALPHA  END-IF                           
536000     IF W-IDLEVNR-ALPHA = 'BPTRA'                                         
536100        MOVE '13550' TO W-IDLEVNR-ALPHA  END-IF                           
536200     IF W-IDLEVNR-ALPHA = 'BPK4A'                                         
536300        MOVE '13777' TO W-IDLEVNR-ALPHA  END-IF                           
536400     IF W-IDLEVNR-ALPHA = 'C79MA'                                         
536500        MOVE '14926' TO W-IDLEVNR-ALPHA  END-IF                           
536600     IF W-IDLEVNR-ALPHA = 'D5E1C'                                         
536700        MOVE '14927' TO W-IDLEVNR-ALPHA  END-IF                           
536800     IF W-IDLEVNR-ALPHA = 'C79ME'                                         
536900        MOVE '14941' TO W-IDLEVNR-ALPHA  END-IF                           
537000     IF W-IDLEVNR-ALPHA = 'T9UZB'                                         
537100        MOVE '16021' TO W-IDLEVNR-ALPHA  END-IF                           
537200     IF W-IDLEVNR-ALPHA = 'B40YA'                                         
537300        MOVE '16084' TO W-IDLEVNR-ALPHA  END-IF                           
537400     IF W-IDLEVNR-ALPHA = 'D38CA'                                         
537500        MOVE '16385' TO W-IDLEVNR-ALPHA  END-IF                           
537600     IF W-IDLEVNR-ALPHA = 'MKB2A'                                         
537700        MOVE '16386' TO W-IDLEVNR-ALPHA  END-IF                           
537800     IF W-IDLEVNR-ALPHA = 'G0MMA'                                         
537900        MOVE '17777' TO W-IDLEVNR-ALPHA  END-IF                           
538000     IF W-IDLEVNR-ALPHA = 'T43NA'                                         
538100        MOVE '17801' TO W-IDLEVNR-ALPHA  END-IF                           
538200     IF W-IDLEVNR-ALPHA = 'S5VSA'                                         
538300        MOVE '18038' TO W-IDLEVNR-ALPHA  END-IF                           
538400     IF W-IDLEVNR-ALPHA = 'CBBNA'                                         
538500        MOVE '18059' TO W-IDLEVNR-ALPHA  END-IF                           
538600     IF W-IDLEVNR-ALPHA = 'B45VD'                                         
538700        MOVE '19725' TO W-IDLEVNR-ALPHA  END-IF                           
538800     IF W-IDLEVNR-ALPHA = 'MKB2B'                                         
538900        MOVE '19995' TO W-IDLEVNR-ALPHA  END-IF                           
539000     IF W-IDLEVNR-ALPHA = 'Q9K3B'                                         
539100        MOVE '23511' TO W-IDLEVNR-ALPHA  END-IF                           
539200     IF W-IDLEVNR-ALPHA = 'S35LB'                                         
539300        MOVE '24248' TO W-IDLEVNR-ALPHA  END-IF                           
539400     IF W-IDLEVNR-ALPHA = 'R742B'                                         
539500        MOVE '24332' TO W-IDLEVNR-ALPHA  END-IF                           
539600     IF W-IDLEVNR-ALPHA = 'ND04W'                                         
539700        MOVE '24967' TO W-IDLEVNR-ALPHA  END-IF                           
539800     IF W-IDLEVNR-ALPHA = 'N8U4A'                                         
539900        MOVE '25784' TO W-IDLEVNR-ALPHA  END-IF                           
540000     IF W-IDLEVNR-ALPHA = 'BPTQC'                                         
540100        MOVE '25809' TO W-IDLEVNR-ALPHA  END-IF                           
540200     IF W-IDLEVNR-ALPHA = 'D08GJ'                                         
540300        MOVE '25889' TO W-IDLEVNR-ALPHA  END-IF                           
540400     IF W-IDLEVNR-ALPHA = 'S044X'                                         
540500        MOVE '25895' TO W-IDLEVNR-ALPHA  END-IF                           
540600     IF W-IDLEVNR-ALPHA = 'B46ZA'                                         
540700        MOVE '25919' TO W-IDLEVNR-ALPHA  END-IF                           
540800     IF W-IDLEVNR-ALPHA = 'KTY7D'                                         
540900        MOVE '25949' TO W-IDLEVNR-ALPHA  END-IF                           
541000*************************************************                         
541100     IF W-IDLEVNR-ALPHA = 'DRDGA'                                         
541200        MOVE '15   ' TO W-IDLEVNR-ALPHA  END-IF                           
541300     IF W-IDLEVNR-ALPHA = 'S69YA'                                         
541400        MOVE '19   ' TO W-IDLEVNR-ALPHA  END-IF                           
541500     IF W-IDLEVNR-ALPHA = 'BLJ6A'                                         
541600        MOVE '177  ' TO W-IDLEVNR-ALPHA  END-IF                           
541700     IF W-IDLEVNR-ALPHA = 'BQ6HA'                                         
541800        MOVE '527  ' TO W-IDLEVNR-ALPHA  END-IF                           
541900     IF W-IDLEVNR-ALPHA = 'AY0MA'                                         
542000        MOVE '745  ' TO W-IDLEVNR-ALPHA  END-IF                           
542100     IF W-IDLEVNR-ALPHA = 'BLZWA'                                         
542200        MOVE '870  ' TO W-IDLEVNR-ALPHA  END-IF                           
542300     IF W-IDLEVNR-ALPHA = 'BQ2BA'                                         
542400        MOVE '1049 ' TO W-IDLEVNR-ALPHA  END-IF                           
542500     IF W-IDLEVNR-ALPHA = 'CFNJA'                                         
542600        MOVE '1456 ' TO W-IDLEVNR-ALPHA  END-IF                           
542700     IF W-IDLEVNR-ALPHA = 'CN4VA'                                         
542800        MOVE '1787 ' TO W-IDLEVNR-ALPHA  END-IF                           
542900     IF W-IDLEVNR-ALPHA = 'BQ3XA'                                         
543000        MOVE '1865 ' TO W-IDLEVNR-ALPHA  END-IF                           
543100     IF W-IDLEVNR-ALPHA = 'CFTNA'                                         
543200        MOVE '2054 ' TO W-IDLEVNR-ALPHA  END-IF                           
543300     IF W-IDLEVNR-ALPHA = 'BX9LA'                                         
543400        MOVE '2103 ' TO W-IDLEVNR-ALPHA  END-IF                           
543500     IF W-IDLEVNR-ALPHA = 'S6R1A'                                         
543600        MOVE '2261 ' TO W-IDLEVNR-ALPHA  END-IF                           
543700     IF W-IDLEVNR-ALPHA = 'BJ6HA'                                         
543800        MOVE '2406 ' TO W-IDLEVNR-ALPHA  END-IF                           
543900     IF W-IDLEVNR-ALPHA = 'BQ5WA'                                         
544000        MOVE '2417 ' TO W-IDLEVNR-ALPHA  END-IF                           
544100     IF W-IDLEVNR-ALPHA = 'BQ5XA'                                         
544200        MOVE '2423 ' TO W-IDLEVNR-ALPHA  END-IF                           
544300     IF W-IDLEVNR-ALPHA = 'CFNUA'                                         
544400        MOVE '2457 ' TO W-IDLEVNR-ALPHA  END-IF                           
544500     IF W-IDLEVNR-ALPHA = 'BYL8A'                                         
544600        MOVE '2490 ' TO W-IDLEVNR-ALPHA  END-IF                           
544700     IF W-IDLEVNR-ALPHA = 'DL1WA'                                         
544800        MOVE '3404 ' TO W-IDLEVNR-ALPHA  END-IF                           
544900     IF W-IDLEVNR-ALPHA = 'K4STA'                                         
545000        MOVE '3572 ' TO W-IDLEVNR-ALPHA  END-IF                           
545100     IF W-IDLEVNR-ALPHA = 'A0VWA'                                         
545200        MOVE '3747 ' TO W-IDLEVNR-ALPHA  END-IF                           
545300     IF W-IDLEVNR-ALPHA = 'R39QA'                                         
545400        MOVE '3815 ' TO W-IDLEVNR-ALPHA  END-IF                           
545500     IF W-IDLEVNR-ALPHA = 'DLJLA'                                         
545600        MOVE '3898 ' TO W-IDLEVNR-ALPHA  END-IF                           
545700     IF W-IDLEVNR-ALPHA = 'BWMAA'                                         
545800        MOVE '3902 ' TO W-IDLEVNR-ALPHA  END-IF                           
545900     IF W-IDLEVNR-ALPHA = 'CQPSA'                                         
546000        MOVE '3918 ' TO W-IDLEVNR-ALPHA  END-IF                           
546100     IF W-IDLEVNR-ALPHA = 'K3D7B'                                         
546200        MOVE '3965 ' TO W-IDLEVNR-ALPHA  END-IF                           
546300     IF W-IDLEVNR-ALPHA = 'G273T'                                         
546400        MOVE '4164 ' TO W-IDLEVNR-ALPHA  END-IF                           
546500     IF W-IDLEVNR-ALPHA = 'G255C'                                         
546600        MOVE '4239 ' TO W-IDLEVNR-ALPHA  END-IF                           
546700     IF W-IDLEVNR-ALPHA = 'C212A'                                         
546800        MOVE '4274 ' TO W-IDLEVNR-ALPHA  END-IF                           
546900     IF W-IDLEVNR-ALPHA = 'EQ17A'                                         
547000        MOVE '4344 ' TO W-IDLEVNR-ALPHA  END-IF                           
547100     IF W-IDLEVNR-ALPHA = 'CN5BA'                                         
547200        MOVE '4528 ' TO W-IDLEVNR-ALPHA  END-IF                           
547300     IF W-IDLEVNR-ALPHA = 'C8Q0A'                                         
547400        MOVE '4955 ' TO W-IDLEVNR-ALPHA  END-IF                           
547500     IF W-IDLEVNR-ALPHA = 'K817J'                                         
547600        MOVE '4994 ' TO W-IDLEVNR-ALPHA  END-IF                           
547700     IF W-IDLEVNR-ALPHA = 'B40QG'                                         
547800        MOVE '5019 ' TO W-IDLEVNR-ALPHA  END-IF                           
547900     IF W-IDLEVNR-ALPHA = 'C7G4A'                                         
548000        MOVE '5051 ' TO W-IDLEVNR-ALPHA  END-IF                           
548100     IF W-IDLEVNR-ALPHA = 'C9A2A'                                         
548200        MOVE '5135 ' TO W-IDLEVNR-ALPHA  END-IF                           
548300     IF W-IDLEVNR-ALPHA = 'CFN4A'                                         
548400        MOVE '5287 ' TO W-IDLEVNR-ALPHA  END-IF                           
548500     IF W-IDLEVNR-ALPHA = 'P8D3A'                                         
548600        MOVE '5595 ' TO W-IDLEVNR-ALPHA  END-IF                           
548700     IF W-IDLEVNR-ALPHA = 'F962A'                                         
548800        MOVE '6026 ' TO W-IDLEVNR-ALPHA  END-IF                           
548900     IF W-IDLEVNR-ALPHA = 'DL6KA'                                         
549000        MOVE '6163 ' TO W-IDLEVNR-ALPHA  END-IF                           
549100     IF W-IDLEVNR-ALPHA = 'D0SAA'                                         
549200        MOVE '6235 ' TO W-IDLEVNR-ALPHA  END-IF                           
549300     IF W-IDLEVNR-ALPHA = 'DLLEA'                                         
549400        MOVE '6296 ' TO W-IDLEVNR-ALPHA  END-IF                           
549500     IF W-IDLEVNR-ALPHA = 'BX6BC'                                         
549600        MOVE '6335 ' TO W-IDLEVNR-ALPHA  END-IF                           
549700     IF W-IDLEVNR-ALPHA = 'CUQLA'                                         
549800        MOVE '6584 ' TO W-IDLEVNR-ALPHA  END-IF                           
549900     IF W-IDLEVNR-ALPHA = 'T1X5A'                                         
550000        MOVE '6838 ' TO W-IDLEVNR-ALPHA  END-IF                           
550100     IF W-IDLEVNR-ALPHA = 'D3R7A'                                         
550200        MOVE '6908 ' TO W-IDLEVNR-ALPHA  END-IF                           
550300     IF W-IDLEVNR-ALPHA = 'CFT1A'                                         
550400        MOVE '7203 ' TO W-IDLEVNR-ALPHA  END-IF                           
550500     IF W-IDLEVNR-ALPHA = 'K8XJA'                                         
550600        MOVE '7231 ' TO W-IDLEVNR-ALPHA  END-IF                           
550700     IF W-IDLEVNR-ALPHA = 'BP3JA'                                         
550800        MOVE '7367 ' TO W-IDLEVNR-ALPHA  END-IF                           
550900     IF W-IDLEVNR-ALPHA = 'DT9EA'                                         
551000        MOVE '7662 ' TO W-IDLEVNR-ALPHA  END-IF                           
551100     IF W-IDLEVNR-ALPHA = 'BH6NA'                                         
551200        MOVE '7724 ' TO W-IDLEVNR-ALPHA  END-IF                           
551300     IF W-IDLEVNR-ALPHA = 'CKSMA'                                         
551400        MOVE '7767 ' TO W-IDLEVNR-ALPHA  END-IF                           
551500     IF W-IDLEVNR-ALPHA = 'BQ8GA'                                         
551600        MOVE '7786 ' TO W-IDLEVNR-ALPHA  END-IF                           
551700     IF W-IDLEVNR-ALPHA = 'DLLSA'                                         
551800        MOVE '7821 ' TO W-IDLEVNR-ALPHA  END-IF                           
551900     IF W-IDLEVNR-ALPHA = 'DBF3A'                                         
552000        MOVE '7956 ' TO W-IDLEVNR-ALPHA  END-IF                           
552100     IF W-IDLEVNR-ALPHA = 'H222A'                                         
552200        MOVE '7961 ' TO W-IDLEVNR-ALPHA  END-IF                           
552300     IF W-IDLEVNR-ALPHA = 'P1NQA'                                         
552400        MOVE '8204 ' TO W-IDLEVNR-ALPHA  END-IF                           
552500     IF W-IDLEVNR-ALPHA = 'CFNNA'                                         
552600        MOVE '10105' TO W-IDLEVNR-ALPHA  END-IF                           
552700     IF W-IDLEVNR-ALPHA = 'CN5LA'                                         
552800        MOVE '10141' TO W-IDLEVNR-ALPHA  END-IF                           
552900     IF W-IDLEVNR-ALPHA = 'L8K5R'                                         
553000        MOVE '10178' TO W-IDLEVNR-ALPHA  END-IF                           
553100     IF W-IDLEVNR-ALPHA = 'BRV3A'                                         
553200        MOVE '10339' TO W-IDLEVNR-ALPHA  END-IF                           
553300     IF W-IDLEVNR-ALPHA = 'L8K5G'                                         
553400        MOVE '10342' TO W-IDLEVNR-ALPHA  END-IF                           
553500     IF W-IDLEVNR-ALPHA = 'U7PMA'                                         
553600        MOVE '10986' TO W-IDLEVNR-ALPHA  END-IF                           
553700     IF W-IDLEVNR-ALPHA = 'E1P5A'                                         
553800        MOVE '11578' TO W-IDLEVNR-ALPHA  END-IF                           
553900     IF W-IDLEVNR-ALPHA = 'BQ9EA'                                         
554000        MOVE '12569' TO W-IDLEVNR-ALPHA  END-IF                           
554100     IF W-IDLEVNR-ALPHA = 'T655C'                                         
554200        MOVE '13011' TO W-IDLEVNR-ALPHA  END-IF                           
554300     IF W-IDLEVNR-ALPHA = 'D3U6A'                                         
554400        MOVE '13386' TO W-IDLEVNR-ALPHA  END-IF                           
554500     IF W-IDLEVNR-ALPHA = 'S4LBA'                                         
554600        MOVE '13391' TO W-IDLEVNR-ALPHA  END-IF                           
554700     IF W-IDLEVNR-ALPHA = 'DPVGA'                                         
554800        MOVE '13486' TO W-IDLEVNR-ALPHA  END-IF                           
554900     IF W-IDLEVNR-ALPHA = 'BQ9HB'                                         
555000        MOVE '13516' TO W-IDLEVNR-ALPHA  END-IF                           
555100     IF W-IDLEVNR-ALPHA = 'M9TPB'                                         
555200        MOVE '13556' TO W-IDLEVNR-ALPHA  END-IF                           
555300     IF W-IDLEVNR-ALPHA = 'BN7SA'                                         
555400        MOVE '13598' TO W-IDLEVNR-ALPHA  END-IF                           
555500     IF W-IDLEVNR-ALPHA = 'BQ9LA'                                         
555600        MOVE '13600' TO W-IDLEVNR-ALPHA  END-IF                           
555700     IF W-IDLEVNR-ALPHA = 'BQ9HC'                                         
555800        MOVE '13661' TO W-IDLEVNR-ALPHA  END-IF                           
555900     IF W-IDLEVNR-ALPHA = 'V4FWA'                                         
556000        MOVE '14313' TO W-IDLEVNR-ALPHA  END-IF                           
556100     IF W-IDLEVNR-ALPHA = 'D45NA'                                         
556200        MOVE '14602' TO W-IDLEVNR-ALPHA  END-IF                           
556300     IF W-IDLEVNR-ALPHA = 'L8K5A'                                         
556400        MOVE '14605' TO W-IDLEVNR-ALPHA  END-IF                           
556500     IF W-IDLEVNR-ALPHA = 'D26YB'                                         
556600        MOVE '15580' TO W-IDLEVNR-ALPHA  END-IF                           
556700     IF W-IDLEVNR-ALPHA = 'BTKBA'                                         
556800        MOVE '16048' TO W-IDLEVNR-ALPHA  END-IF                           
556900     IF W-IDLEVNR-ALPHA = 'S13SA'                                         
557000        MOVE '16058' TO W-IDLEVNR-ALPHA  END-IF                           
557100     IF W-IDLEVNR-ALPHA = 'E510F'                                         
557200        MOVE '16132' TO W-IDLEVNR-ALPHA  END-IF                           
557300     IF W-IDLEVNR-ALPHA = 'R7B3A'                                         
557400        MOVE '16158' TO W-IDLEVNR-ALPHA  END-IF                           
557500     IF W-IDLEVNR-ALPHA = 'CJ0GA'                                         
557600        MOVE '16251' TO W-IDLEVNR-ALPHA  END-IF                           
557700     IF W-IDLEVNR-ALPHA = 'S1YHA'                                         
557800        MOVE '16267' TO W-IDLEVNR-ALPHA  END-IF                           
557900     IF W-IDLEVNR-ALPHA = 'BQ9UA'                                         
558000        MOVE '16325' TO W-IDLEVNR-ALPHA  END-IF                           
558100     IF W-IDLEVNR-ALPHA = 'T446A'                                         
558200        MOVE '16470' TO W-IDLEVNR-ALPHA  END-IF                           
558300     IF W-IDLEVNR-ALPHA = 'B44XE'                                         
558400        MOVE '16490' TO W-IDLEVNR-ALPHA  END-IF                           
558500     IF W-IDLEVNR-ALPHA = 'BRV3B'                                         
558600        MOVE '17712' TO W-IDLEVNR-ALPHA  END-IF                           
558700     IF W-IDLEVNR-ALPHA = 'CN5QA'                                         
558800        MOVE '17713' TO W-IDLEVNR-ALPHA  END-IF                           
558900     IF W-IDLEVNR-ALPHA = 'CN5SA'                                         
559000        MOVE '17819' TO W-IDLEVNR-ALPHA  END-IF                           
559100     IF W-IDLEVNR-ALPHA = 'CW5YA'                                         
559200        MOVE '17819' TO W-IDLEVNR-ALPHA  END-IF                           
559300     IF W-IDLEVNR-ALPHA = 'DSHRA'                                         
559400        MOVE '18971' TO W-IDLEVNR-ALPHA  END-IF                           
559500     IF W-IDLEVNR-ALPHA = 'C9T2C'                                         
559600        MOVE '18997' TO W-IDLEVNR-ALPHA  END-IF                           
559700     IF W-IDLEVNR-ALPHA = 'AA8SA'                                         
559800        MOVE '19892' TO W-IDLEVNR-ALPHA  END-IF                           
559900     IF W-IDLEVNR-ALPHA = 'F641A'                                         
560000        MOVE '23093' TO W-IDLEVNR-ALPHA  END-IF                           
560100     IF W-IDLEVNR-ALPHA = 'DLNVA'                                         
560200        MOVE '23685' TO W-IDLEVNR-ALPHA  END-IF                           
560300     IF W-IDLEVNR-ALPHA = 'P108C'                                         
560400        MOVE '24263' TO W-IDLEVNR-ALPHA  END-IF                           
560500     IF W-IDLEVNR-ALPHA = 'CT3JA'                                         
560600        MOVE '24521' TO W-IDLEVNR-ALPHA  END-IF                           
560700     IF W-IDLEVNR-ALPHA = 'CW0HA'                                         
560800        MOVE '24653' TO W-IDLEVNR-ALPHA  END-IF                           
560900     IF W-IDLEVNR-ALPHA = 'CVVEA'                                         
561000        MOVE '24789' TO W-IDLEVNR-ALPHA  END-IF                           
561100     IF W-IDLEVNR-ALPHA = 'S5U0B'                                         
561200        MOVE '24896' TO W-IDLEVNR-ALPHA  END-IF                           
561300     IF W-IDLEVNR-ALPHA = 'CYSZB'                                         
561400        MOVE '24921' TO W-IDLEVNR-ALPHA  END-IF                           
561500     IF W-IDLEVNR-ALPHA = 'C72GA'                                         
561600        MOVE '24999' TO W-IDLEVNR-ALPHA  END-IF                           
561700     IF W-IDLEVNR-ALPHA = 'CYFWA'                                         
561800        MOVE '25286' TO W-IDLEVNR-ALPHA  END-IF                           
561900     IF W-IDLEVNR-ALPHA = 'BKL3A'                                         
562000        MOVE '25402' TO W-IDLEVNR-ALPHA  END-IF                           
562100     IF W-IDLEVNR-ALPHA = 'CZSEB'                                         
562200        MOVE '25620' TO W-IDLEVNR-ALPHA  END-IF                           
562300     IF W-IDLEVNR-ALPHA = 'DBBTA'                                         
562400        MOVE '25755' TO W-IDLEVNR-ALPHA  END-IF                           
562500     IF W-IDLEVNR-ALPHA = 'DA9EA'                                         
562600        MOVE '25765' TO W-IDLEVNR-ALPHA  END-IF                           
562700     IF W-IDLEVNR-ALPHA = 'JWMJA'                                         
562800        MOVE '25770' TO W-IDLEVNR-ALPHA  END-IF                           
562900     IF W-IDLEVNR-ALPHA = 'CYDAB'                                         
563000        MOVE '25797' TO W-IDLEVNR-ALPHA  END-IF                           
563100     IF W-IDLEVNR-ALPHA = 'EQ62A'                                         
563200        MOVE '25808' TO W-IDLEVNR-ALPHA  END-IF                           
563300     IF W-IDLEVNR-ALPHA = 'DDA4A'                                         
563400        MOVE '25810' TO W-IDLEVNR-ALPHA  END-IF                           
563500     IF W-IDLEVNR-ALPHA = 'DDETA'                                         
563600        MOVE '25818' TO W-IDLEVNR-ALPHA  END-IF                           
563700     IF W-IDLEVNR-ALPHA = 'V2NYA'                                         
563800        MOVE '25847' TO W-IDLEVNR-ALPHA  END-IF                           
563900     IF W-IDLEVNR-ALPHA = 'T655D'                                         
564000        MOVE '25859' TO W-IDLEVNR-ALPHA  END-IF                           
564100     IF W-IDLEVNR-ALPHA = 'G261S'                                         
564200        MOVE '25869' TO W-IDLEVNR-ALPHA  END-IF                           
564300     IF W-IDLEVNR-ALPHA = 'DH0RA'                                         
564400        MOVE '25870' TO W-IDLEVNR-ALPHA  END-IF                           
564500     IF W-IDLEVNR-ALPHA = 'BNWSE'                                         
564600        MOVE '25972' TO W-IDLEVNR-ALPHA  END-IF                           
564700     IF W-IDLEVNR-ALPHA = 'DRTHA'                                         
564800        MOVE '25996' TO W-IDLEVNR-ALPHA  END-IF                           
564900     IF W-IDLEVNR-ALPHA = 'BP8HA'                                         
565000        MOVE '34621' TO W-IDLEVNR-ALPHA  END-IF                           
565100     IF W-IDLEVNR-ALPHA = 'U9JXA'                                         
565200        MOVE '51065' TO W-IDLEVNR-ALPHA  END-IF                           
565300     IF W-IDLEVNR-ALPHA = 'A405A'                                         
565400        MOVE '51103' TO W-IDLEVNR-ALPHA  END-IF                           
565500     IF W-IDLEVNR-ALPHA = 'T733E'                                         
565600        MOVE '51568' TO W-IDLEVNR-ALPHA  END-IF                           
565700     IF W-IDLEVNR-ALPHA = 'F903H'                                         
565800        MOVE '62513' TO W-IDLEVNR-ALPHA  END-IF                           
565900     IF W-IDLEVNR-ALPHA = 'DCYQA'                                         
566000        MOVE '63300' TO W-IDLEVNR-ALPHA  END-IF                           
566100*************************************************                         
566200     IF W-IDLEVNR-ALPHA = 'C61MA'                                         
566300        MOVE '82   ' TO W-IDLEVNR-ALPHA  END-IF                           
566400     IF W-IDLEVNR-ALPHA = 'W064Z'                                         
566500        MOVE '4001 ' TO W-IDLEVNR-ALPHA  END-IF                           
566600     IF W-IDLEVNR-ALPHA = 'F260B'                                         
566700        MOVE '4038 ' TO W-IDLEVNR-ALPHA  END-IF                           
566800     IF W-IDLEVNR-ALPHA = 'BPQUA'                                         
566900        MOVE '4210 ' TO W-IDLEVNR-ALPHA  END-IF                           
567000     IF W-IDLEVNR-ALPHA = 'L217Q'                                         
567100        MOVE '4230 ' TO W-IDLEVNR-ALPHA  END-IF                           
567200     IF W-IDLEVNR-ALPHA = 'BPQUA'                                         
567300        MOVE '4280 ' TO W-IDLEVNR-ALPHA  END-IF                           
567400     IF W-IDLEVNR-ALPHA = 'BPGRA'                                         
567500        MOVE '4536 ' TO W-IDLEVNR-ALPHA  END-IF                           
567600     IF W-IDLEVNR-ALPHA = 'KFBGA'                                         
567700        MOVE '4667 ' TO W-IDLEVNR-ALPHA  END-IF                           
567800     IF W-IDLEVNR-ALPHA = 'D00MD'                                         
567900        MOVE '4921 ' TO W-IDLEVNR-ALPHA  END-IF                           
568000     IF W-IDLEVNR-ALPHA = 'D00MB'                                         
568100        MOVE '4930 ' TO W-IDLEVNR-ALPHA  END-IF                           
568200     IF W-IDLEVNR-ALPHA = 'D2T1E'                                         
568300        MOVE '6434 ' TO W-IDLEVNR-ALPHA  END-IF                           
568400     IF W-IDLEVNR-ALPHA = 'V1W4A'                                         
568500        MOVE '6969 ' TO W-IDLEVNR-ALPHA  END-IF                           
568600     IF W-IDLEVNR-ALPHA = 'J7TDB'                                         
568700        MOVE '7254 ' TO W-IDLEVNR-ALPHA  END-IF                           
568800     IF W-IDLEVNR-ALPHA = 'P981A'                                         
568900        MOVE '7994 ' TO W-IDLEVNR-ALPHA  END-IF                           
569000     IF W-IDLEVNR-ALPHA = 'TC12A'                                         
569100        MOVE '12101' TO W-IDLEVNR-ALPHA  END-IF                           
569200     IF W-IDLEVNR-ALPHA = 'BNWSB'                                         
569300        MOVE '13673' TO W-IDLEVNR-ALPHA  END-IF                           
569400     IF W-IDLEVNR-ALPHA = 'D00ME'                                         
569500        MOVE '14345' TO W-IDLEVNR-ALPHA  END-IF                           
569600     IF W-IDLEVNR-ALPHA = 'BQ6XB'                                         
569700        MOVE '14642' TO W-IDLEVNR-ALPHA  END-IF                           
569800     IF W-IDLEVNR-ALPHA = 'B468X'                                         
569900        MOVE '17064' TO W-IDLEVNR-ALPHA  END-IF                           
570000     IF W-IDLEVNR-ALPHA = 'B535B'                                         
570100        MOVE '18992' TO W-IDLEVNR-ALPHA  END-IF                           
570200     IF W-IDLEVNR-ALPHA = 'CQPPA'                                         
570300        MOVE '19018' TO W-IDLEVNR-ALPHA  END-IF                           
570400     IF W-IDLEVNR-ALPHA = 'T6A3A'                                         
570500        MOVE '23559' TO W-IDLEVNR-ALPHA  END-IF                           
570600     IF W-IDLEVNR-ALPHA = 'D00MF'                                         
570700        MOVE '23560' TO W-IDLEVNR-ALPHA  END-IF                           
570800     IF W-IDLEVNR-ALPHA = 'B40CC'                                         
570900        MOVE '23758' TO W-IDLEVNR-ALPHA  END-IF                           
571000     IF W-IDLEVNR-ALPHA = 'MLE6B'                                         
571100        MOVE '25832' TO W-IDLEVNR-ALPHA  END-IF                           
571200     IF W-IDLEVNR-ALPHA = 'D00MG'                                         
571300        MOVE '25925' TO W-IDLEVNR-ALPHA  END-IF                           
571400     IF W-IDLEVNR-ALPHA = 'D00MH'                                         
571500        MOVE '34345' TO W-IDLEVNR-ALPHA  END-IF                           
571600     IF W-IDLEVNR-ALPHA = 'R235B'                                         
571700        MOVE '50091' TO W-IDLEVNR-ALPHA  END-IF                           
571800     IF W-IDLEVNR-ALPHA = 'M617T'                                         
571900        MOVE '62385' TO W-IDLEVNR-ALPHA  END-IF                           
572000     IF W-IDLEVNR-ALPHA = 'R235G'                                         
572100        MOVE '63544' TO W-IDLEVNR-ALPHA  END-IF                           
572200*************************************************                         
572300     IF W-IDLEVNR-ALPHA = 'CQF3A'                                         
572400        MOVE '124  ' TO W-IDLEVNR-ALPHA  END-IF                           
572500     IF W-IDLEVNR-ALPHA = 'BJRAA'                                         
572600        MOVE '175  ' TO W-IDLEVNR-ALPHA  END-IF                           
572700     IF W-IDLEVNR-ALPHA = 'DFVDA'                                         
572800        MOVE '281  ' TO W-IDLEVNR-ALPHA  END-IF                           
572900     IF W-IDLEVNR-ALPHA = 'BJVLA'                                         
573000        MOVE '312  ' TO W-IDLEVNR-ALPHA  END-IF                           
573100     IF W-IDLEVNR-ALPHA = 'BQ1PA'                                         
573200        MOVE '966  ' TO W-IDLEVNR-ALPHA  END-IF                           
573300     IF W-IDLEVNR-ALPHA = 'S3DHA'                                         
573400        MOVE '1125 ' TO W-IDLEVNR-ALPHA  END-IF                           
573500     IF W-IDLEVNR-ALPHA = 'CFNGA'                                         
573600        MOVE '1235 ' TO W-IDLEVNR-ALPHA  END-IF                           
573700     IF W-IDLEVNR-ALPHA = 'CFNHA'                                         
573800        MOVE '1271 ' TO W-IDLEVNR-ALPHA  END-IF                           
573900     IF W-IDLEVNR-ALPHA = 'BKTVA'                                         
574000        MOVE '1495 ' TO W-IDLEVNR-ALPHA  END-IF                           
574100     IF W-IDLEVNR-ALPHA = 'CFJEA'                                         
574200        MOVE '1757 ' TO W-IDLEVNR-ALPHA  END-IF                           
574300     IF W-IDLEVNR-ALPHA = 'BYL2A'                                         
574400        MOVE '2001 ' TO W-IDLEVNR-ALPHA  END-IF                           
574500     IF W-IDLEVNR-ALPHA = 'CFTPA'                                         
574600        MOVE '2250 ' TO W-IDLEVNR-ALPHA  END-IF                           
574700     IF W-IDLEVNR-ALPHA = 'S3DHC'                                         
574800        MOVE '2429 ' TO W-IDLEVNR-ALPHA  END-IF                           
574900     IF W-IDLEVNR-ALPHA = 'BPUFA'                                         
575000        MOVE '2503 ' TO W-IDLEVNR-ALPHA  END-IF                           
575100     IF W-IDLEVNR-ALPHA = 'BKXQA'                                         
575200        MOVE '2650 ' TO W-IDLEVNR-ALPHA  END-IF                           
575300     IF W-IDLEVNR-ALPHA = 'CFFXA'                                         
575400        MOVE '3143 ' TO W-IDLEVNR-ALPHA  END-IF                           
575500     IF W-IDLEVNR-ALPHA = 'D1V4A'                                         
575600        MOVE '3312 ' TO W-IDLEVNR-ALPHA  END-IF                           
575700     IF W-IDLEVNR-ALPHA = 'L8K5V'                                         
575800        MOVE '3341 ' TO W-IDLEVNR-ALPHA  END-IF                           
575900     IF W-IDLEVNR-ALPHA = 'S552A'                                         
576000        MOVE '6074 ' TO W-IDLEVNR-ALPHA  END-IF                           
576100     IF W-IDLEVNR-ALPHA = 'T226F'                                         
576200        MOVE '6089 ' TO W-IDLEVNR-ALPHA  END-IF                           
576300     IF W-IDLEVNR-ALPHA = 'C8P5A'                                         
576400        MOVE '6670 ' TO W-IDLEVNR-ALPHA  END-IF                           
576500     IF W-IDLEVNR-ALPHA = 'JBA1A'                                         
576600        MOVE '6745 ' TO W-IDLEVNR-ALPHA  END-IF                           
576700     IF W-IDLEVNR-ALPHA = 'BP7YA'                                         
576800        MOVE '7500 ' TO W-IDLEVNR-ALPHA  END-IF                           
576900     IF W-IDLEVNR-ALPHA = 'CY7ZA'                                         
577000        MOVE '7923 ' TO W-IDLEVNR-ALPHA  END-IF                           
577100     IF W-IDLEVNR-ALPHA = 'BJVHA'                                         
577200        MOVE '10057' TO W-IDLEVNR-ALPHA  END-IF                           
577300     IF W-IDLEVNR-ALPHA = 'D36ZA'                                         
577400        MOVE '10947' TO W-IDLEVNR-ALPHA  END-IF                           
577500     IF W-IDLEVNR-ALPHA = 'J3BDA'                                         
577600        MOVE '14922' TO W-IDLEVNR-ALPHA  END-IF                           
577700     IF W-IDLEVNR-ALPHA = 'J6R3A'                                         
577800        MOVE '14990' TO W-IDLEVNR-ALPHA  END-IF                           
577900     IF W-IDLEVNR-ALPHA = 'C0VAH'                                         
578000        MOVE '15284' TO W-IDLEVNR-ALPHA  END-IF                           
578100     IF W-IDLEVNR-ALPHA = 'C8P5C'                                         
578200        MOVE '16037' TO W-IDLEVNR-ALPHA  END-IF                           
578300     IF W-IDLEVNR-ALPHA = 'C8P5D'                                         
578400        MOVE '16179' TO W-IDLEVNR-ALPHA  END-IF                           
578500     IF W-IDLEVNR-ALPHA = 'KTY7B'                                         
578600        MOVE '16336' TO W-IDLEVNR-ALPHA  END-IF                           
578700     IF W-IDLEVNR-ALPHA = 'C8P5J'                                         
578800        MOVE '16372' TO W-IDLEVNR-ALPHA  END-IF                           
578900     IF W-IDLEVNR-ALPHA = 'DR7TA'                                         
579000        MOVE '19206' TO W-IDLEVNR-ALPHA  END-IF                           
579100     IF W-IDLEVNR-ALPHA = 'C9H7A'                                         
579200        MOVE '19611' TO W-IDLEVNR-ALPHA  END-IF                           
579300     IF W-IDLEVNR-ALPHA = 'BXMZA'                                         
579400        MOVE '19914' TO W-IDLEVNR-ALPHA  END-IF                           
579500     IF W-IDLEVNR-ALPHA = 'C72DC'                                         
579600        MOVE '22292' TO W-IDLEVNR-ALPHA  END-IF                           
579700     IF W-IDLEVNR-ALPHA = 'A0VWC'                                         
579800        MOVE '23934' TO W-IDLEVNR-ALPHA  END-IF                           
579900     IF W-IDLEVNR-ALPHA = 'BQ9VB'                                         
580000        MOVE '24081' TO W-IDLEVNR-ALPHA  END-IF                           
580100     IF W-IDLEVNR-ALPHA = 'AJFWA'                                         
580200        MOVE '25072' TO W-IDLEVNR-ALPHA  END-IF                           
580300     IF W-IDLEVNR-ALPHA = 'AJFXA'                                         
580400        MOVE '25073' TO W-IDLEVNR-ALPHA  END-IF                           
580500     IF W-IDLEVNR-ALPHA = 'D2L4A'                                         
580600        MOVE '25082' TO W-IDLEVNR-ALPHA  END-IF                           
580700     IF W-IDLEVNR-ALPHA = 'K261A'                                         
580800        MOVE '25682' TO W-IDLEVNR-ALPHA  END-IF                           
580900     IF W-IDLEVNR-ALPHA = 'A76VE'                                         
581000        MOVE '25817' TO W-IDLEVNR-ALPHA  END-IF                           
581100     IF W-IDLEVNR-ALPHA = 'D36ZC'                                         
581200        MOVE '25826' TO W-IDLEVNR-ALPHA  END-IF                           
581300     IF W-IDLEVNR-ALPHA = 'DSDLA'                                         
581400        MOVE '25871' TO W-IDLEVNR-ALPHA  END-IF                           
581500     IF W-IDLEVNR-ALPHA = 'Q9KNA'                                         
581600        MOVE '25967' TO W-IDLEVNR-ALPHA  END-IF                           
581700     IF W-IDLEVNR-ALPHA = 'L2HWG'                                         
581800        MOVE '63609' TO W-IDLEVNR-ALPHA  END-IF                           
581900*************************************************                         
582000*************************************************                         
582100     IF W-IDLEVNR-ALPHA = 'BPGTB'                                         
582200        MOVE '211  ' TO W-IDLEVNR-ALPHA  END-IF                           
582300     IF W-IDLEVNR-ALPHA = 'BPGTA'                                         
582400        MOVE '1826 ' TO W-IDLEVNR-ALPHA  END-IF                           
582500     IF W-IDLEVNR-ALPHA = 'AD1YA'                                         
582600        MOVE '3469 ' TO W-IDLEVNR-ALPHA  END-IF                           
582700     IF W-IDLEVNR-ALPHA = 'L8K5U'                                         
582800        MOVE '3471 ' TO W-IDLEVNR-ALPHA  END-IF                           
582900     IF W-IDLEVNR-ALPHA = 'G8UTB'                                         
583000        MOVE '3494 ' TO W-IDLEVNR-ALPHA  END-IF                           
583100     IF W-IDLEVNR-ALPHA = 'MCSQA'                                         
583200        MOVE '3767 ' TO W-IDLEVNR-ALPHA  END-IF                           
583300     IF W-IDLEVNR-ALPHA = 'LNAPA'                                         
583400        MOVE '4540 ' TO W-IDLEVNR-ALPHA  END-IF                           
583500     IF W-IDLEVNR-ALPHA = 'LJVZA'                                         
583600        MOVE '4730 ' TO W-IDLEVNR-ALPHA  END-IF                           
583700     IF W-IDLEVNR-ALPHA = 'DL5LA'                                         
583800        MOVE '4762 ' TO W-IDLEVNR-ALPHA  END-IF                           
583900     IF W-IDLEVNR-ALPHA = 'DDE2A'                                         
584000        MOVE '4790 ' TO W-IDLEVNR-ALPHA  END-IF                           
584100     IF W-IDLEVNR-ALPHA = 'F636B'                                         
584200        MOVE '5154 ' TO W-IDLEVNR-ALPHA  END-IF                           
584300     IF W-IDLEVNR-ALPHA = 'Q33YA'                                         
584400        MOVE '5220 ' TO W-IDLEVNR-ALPHA  END-IF                           
584500     IF W-IDLEVNR-ALPHA = 'DGDKA'                                         
584600        MOVE '5239 ' TO W-IDLEVNR-ALPHA  END-IF                           
584700     IF W-IDLEVNR-ALPHA = 'B517X'                                         
584800        MOVE '5419 ' TO W-IDLEVNR-ALPHA  END-IF                           
584900     IF W-IDLEVNR-ALPHA = 'BRGHA'                                         
585000        MOVE '5614 ' TO W-IDLEVNR-ALPHA  END-IF                           
585100     IF W-IDLEVNR-ALPHA = 'C89GA'                                         
585200        MOVE '5664 ' TO W-IDLEVNR-ALPHA  END-IF                           
585300     IF W-IDLEVNR-ALPHA = 'CKBZA'                                         
585400        MOVE '6133 ' TO W-IDLEVNR-ALPHA  END-IF                           
585500     IF W-IDLEVNR-ALPHA = 'DL6LA'                                         
585600        MOVE '6292 ' TO W-IDLEVNR-ALPHA  END-IF                           
585700     IF W-IDLEVNR-ALPHA = 'D17MA'                                         
585800        MOVE '6363 ' TO W-IDLEVNR-ALPHA  END-IF                           
585900     IF W-IDLEVNR-ALPHA = 'E355B'                                         
586000        MOVE '6369 ' TO W-IDLEVNR-ALPHA  END-IF                           
586100     IF W-IDLEVNR-ALPHA = 'R1N9A'                                         
586200        MOVE '6552 ' TO W-IDLEVNR-ALPHA  END-IF                           
586300     IF W-IDLEVNR-ALPHA = 'M738A'                                         
586400        MOVE '6599 ' TO W-IDLEVNR-ALPHA  END-IF                           
586500     IF W-IDLEVNR-ALPHA = 'DLLMA'                                         
586600        MOVE '6677 ' TO W-IDLEVNR-ALPHA  END-IF                           
586700     IF W-IDLEVNR-ALPHA = 'B45GA'                                         
586800        MOVE '6748 ' TO W-IDLEVNR-ALPHA  END-IF                           
586900     IF W-IDLEVNR-ALPHA = 'D0FVB'                                         
587000        MOVE '7752 ' TO W-IDLEVNR-ALPHA  END-IF                           
587100     IF W-IDLEVNR-ALPHA = 'D0HHD'                                         
587200        MOVE '7800 ' TO W-IDLEVNR-ALPHA  END-IF                           
587300     IF W-IDLEVNR-ALPHA = 'BPTQB'                                         
587400        MOVE '10157' TO W-IDLEVNR-ALPHA  END-IF                           
587500     IF W-IDLEVNR-ALPHA = 'BPTQA'                                         
587600        MOVE '10158' TO W-IDLEVNR-ALPHA  END-IF                           
587700     IF W-IDLEVNR-ALPHA = 'DL7EA'                                         
587800        MOVE '10356' TO W-IDLEVNR-ALPHA  END-IF                           
587900     IF W-IDLEVNR-ALPHA = 'CT3KA'                                         
588000        MOVE '10934' TO W-IDLEVNR-ALPHA  END-IF                           
588100     IF W-IDLEVNR-ALPHA = 'DL7KA'                                         
588200        MOVE '13349' TO W-IDLEVNR-ALPHA  END-IF                           
588300     IF W-IDLEVNR-ALPHA = 'DL7LA'                                         
588400        MOVE '13388' TO W-IDLEVNR-ALPHA  END-IF                           
588500     IF W-IDLEVNR-ALPHA = 'DLMNA'                                         
588600        MOVE '13396' TO W-IDLEVNR-ALPHA  END-IF                           
588700     IF W-IDLEVNR-ALPHA = 'DBQZA'                                         
588800        MOVE '13571' TO W-IDLEVNR-ALPHA  END-IF                           
588900     IF W-IDLEVNR-ALPHA = 'CRKZA'                                         
589000        MOVE '13572' TO W-IDLEVNR-ALPHA  END-IF                           
589100     IF W-IDLEVNR-ALPHA = 'DL7WA'                                         
589200        MOVE '13583' TO W-IDLEVNR-ALPHA  END-IF                           
589300     IF W-IDLEVNR-ALPHA = 'BQAEB'                                         
589400        MOVE '13717' TO W-IDLEVNR-ALPHA  END-IF                           
589500     IF W-IDLEVNR-ALPHA = 'Q5EGA'                                         
589600        MOVE '14947' TO W-IDLEVNR-ALPHA  END-IF                           
589700     IF W-IDLEVNR-ALPHA = 'Q89FA'                                         
589800        MOVE '16030' TO W-IDLEVNR-ALPHA  END-IF                           
589900     IF W-IDLEVNR-ALPHA = 'AQ2MB'                                         
590000        MOVE '16112' TO W-IDLEVNR-ALPHA  END-IF                           
590100     IF W-IDLEVNR-ALPHA = 'CNT5A'                                         
590200        MOVE '20522' TO W-IDLEVNR-ALPHA  END-IF                           
590300     IF W-IDLEVNR-ALPHA = 'BMZJA'                                         
590400        MOVE '24489' TO W-IDLEVNR-ALPHA  END-IF                           
590500     IF W-IDLEVNR-ALPHA = 'ATC2A'                                         
590600        MOVE '25089' TO W-IDLEVNR-ALPHA  END-IF                           
590700     IF W-IDLEVNR-ALPHA = 'MAXWA'                                         
590800        MOVE '25090' TO W-IDLEVNR-ALPHA  END-IF                           
590900     IF W-IDLEVNR-ALPHA = 'BL2CA'                                         
591000        MOVE '25412' TO W-IDLEVNR-ALPHA  END-IF                           
591100     IF W-IDLEVNR-ALPHA = 'M799G'                                         
591200        MOVE '25446' TO W-IDLEVNR-ALPHA  END-IF                           
591300     IF W-IDLEVNR-ALPHA = 'N2KUA'                                         
591400        MOVE '25828' TO W-IDLEVNR-ALPHA  END-IF                           
591500     IF W-IDLEVNR-ALPHA = 'D38ME'                                         
591600        MOVE '25878' TO W-IDLEVNR-ALPHA  END-IF                           
591700     IF W-IDLEVNR-ALPHA = 'D33BA'                                         
591800        MOVE '25910' TO W-IDLEVNR-ALPHA  END-IF                           
591900     IF W-IDLEVNR-ALPHA = 'BPTQD'                                         
592000        MOVE '25924' TO W-IDLEVNR-ALPHA  END-IF                           
592100     IF W-IDLEVNR-ALPHA = 'DPEWA'                                         
592200        MOVE '25962' TO W-IDLEVNR-ALPHA  END-IF                           
592300     IF W-IDLEVNR-ALPHA = 'DP5RA'                                         
592400        MOVE '25973' TO W-IDLEVNR-ALPHA  END-IF                           
592500     IF W-IDLEVNR-ALPHA = 'DSBYA'                                         
592600        MOVE '26009' TO W-IDLEVNR-ALPHA  END-IF                           
592700     IF W-IDLEVNR-ALPHA = 'CRX1D'                                         
592800        MOVE '26012' TO W-IDLEVNR-ALPHA  END-IF                           
592900     IF W-IDLEVNR-ALPHA = 'BKDDB'                                         
593000        MOVE '31014' TO W-IDLEVNR-ALPHA  END-IF                           
593100     IF W-IDLEVNR-ALPHA = 'BKDDC'                                         
593200        MOVE '41014' TO W-IDLEVNR-ALPHA  END-IF                           
593300*************************************************                         
593400**********************************************                            
593500     PERFORM S21-IDLEVNR-ALPHA-TO-NUM                                     
593600     MOVE W-IDLEVNR-NUM       TO UTVCOM1-SLAG-IDLEVNR-NUM                 
593700*       *                                                                 
593800     PERFORM S14-SKRIV-W33544V-401                                        
593900        .                                                                 
594000        EJECT                                                             
594100 BB-FLYTTA-POST-402-TILL-VCOM SECTION.                                    
594200     SKIP2                                                                
594300     MOVE IN2-IDPTYP          TO UTVCOM2-IDPTYP                           
594400     MOVE IN2-IDVTYP          TO UTVCOM2-IDVTYP                           
594500     MOVE IN2-TEORSAK         TO UTVCOM2-TEORSAK                          
594600     MOVE IN2-TEARTNOT-3      TO UTVCOM2-TEARTNOT-3                       
594700     MOVE IN2-TEARTNOT-7      TO UTVCOM2-TEARTNOT-7                       
594800     PERFORM S15-SKRIV-W33544V-402                                        
594900     .                                                                    
595000     EJECT                                                                
595100 BC-FLYTTA-POST-403-TILL-VCOM SECTION.                                    
595200     SKIP2                                                                
595300     MOVE IN3-IDPTYP          TO UTVCOM3-IDPTYP                           
595400     MOVE IN3-IDVTYP          TO UTVCOM3-IDVTYP                           
595500     MOVE IN3-BEART(1)        TO UTVCOM3-BEART(1)                         
595600     MOVE IN3-BEART(2)        TO UTVCOM3-BEART(2)                         
595700     PERFORM S16-SKRIV-W33544V-403                                        
595800     .                                                                    
595900     EJECT                                                                
596000 C-FLYTTA-INPOST-TILL-UTFIL SECTION.                                      
596100                                                                          
596200     EVALUATE IN1-IDPTYP                                                  
596300       WHEN '401'                                                         
596400         PERFORM CA-FLYTTA-VERS-401-TILL-UTFIL                            
596500       WHEN '402'                                                         
596600         PERFORM CB-FLYTTA-VERS-402-TILL-UTFIL                            
596700       WHEN '403'                                                         
596800         PERFORM CC-FLYTTA-VERS-403-TILL-UTFIL                            
596900       WHEN OTHER                                                         
597000         STRING 'FEL COPYTEXTVERSION : '                                  
597100                IN1-IDPTYP                                                
597200           DELIMITED BY SIZE INTO FELTEXT-STR                             
597300         DISPLAY FELTEXT                                                  
597400         PERFORM S99-ABEND                                                
597500     END-EVALUATE                                                         
597600     .                                                                    
597700     EJECT                                                                
597800                                                                          
597900 CA-FLYTTA-VERS-401-TILL-UTFIL SECTION.                                   
598000     SKIP2                                                                
598100     MOVE IN1-IDPTYP          TO UT1-IDPTYP                               
598200     MOVE IN1-IDVTYP          TO UT1-IDVTYP                               
598300     MOVE IN1-IDARTNR         TO UT1-IDARTNR                              
598400     MOVE IN1-IDFKNGRP        TO UT1-IDFKNGRP                             
598500     MOVE IN1-KDSRA           TO UT1-KDSRA                                
598600     MOVE IN1-KVQPACK-0       TO UT1-KVQPACK-0                            
598700     MOVE IN1-KDARTURS-NUM    TO UT1-KDARTURS-NUM                         
598800     MOVE IN1-KDPRODSL        TO UT1-KDPRODSL                             
598900     MOVE IN1-VLARTNTO        TO UT1-VLARTNTO                             
599000     MOVE IN1-VKART           TO UT1-VKART                                
599100     MOVE IN1-KDVSOP          TO UT1-KDVSOP                               
599200     MOVE IN1-IDSTATNR        TO UT1-IDSTATNR                             
599300     MOVE IN1-KDSORT          TO UT1-KDSORT                               
599400     MOVE IN1-KDERS           TO UT1-KDERS                                
599500     MOVE IN1-KDBPSR          TO UT1-KDBPSR                               
599600     MOVE IN1-KDBBCL          TO UT1-KDBBCL                               
599700     MOVE IN1-IDLEVNR         TO UT1-IDLEVNR                              
599800* PRARTVNA ÄR SJK ELLER LEV-BESTPRIS                                      
599900     MOVE IN1-PRARTVNA        TO UT1-PRARTVNA                             
600000     MOVE IN1-PRARTSTD        TO UT1-PRARTSTD                             
600100     MOVE IN1-FLIART          TO UT1-FLIART                               
600200     MOVE IN1-IDPROJ          TO UT1-IDPROJ                               
600300     MOVE IN1-IDAO(1)         TO UT1-IDAO(1)                              
600400     MOVE IN1-IDAO(2)         TO UT1-IDAO(2)                              
600500     MOVE IN1-TIFINLEV        TO UT1-TIFINLEV                             
600600     MOVE IN1-IDANSK          TO UT1-IDANSK                               
600700     MOVE IN1-KVPB            TO UT1-KVPB                                 
600800     MOVE IN1-KDVVKL          TO UT1-KDVVKL                               
600900     MOVE IN1-BELEVART        TO UT1-BELEVART                             
601000     MOVE IN1-KDTIPPR         TO UT1-KDTIPPR                              
601100     MOVE IN1-IDINK           TO UT1-IDINK                                
601200     MOVE IN1-TIREGDAT        TO UT1-TIREGDAT                             
601300     MOVE IN1-KDUART          TO UT1-KDUART                               
601400     MOVE IN1-IDARTNR-MOTSV   TO UT1-IDARTNR-MOTSV                        
601500     MOVE IN1-PRINK           TO UT1-PRINK                                
601600     MOVE IN1-IDRITN          TO UT1-IDRITN                               
601700     MOVE IN1-PRHANTK         TO UT1-PRHANTK                              
601800     MOVE IN1-KDAGE           TO UT1-KDAGE                                
601900     MOVE IN1-KDPSLLOC        TO UT1-KDPSLLOC                             
602000     MOVE IN1-SLAG-IDLEVNR    TO UT1-SLAG-IDLEVNR                         
602100     MOVE IN1-IDKAT(1)        TO UT1-IDKAT(1)                             
602200     MOVE IN1-IDKAT(2)        TO UT1-IDKAT(2)                             
602300     MOVE IN1-IDKAT(3)        TO UT1-IDKAT(3)                             
602400     MOVE IN1-KDRAB           TO UT1-KDRAB                                
602500     MOVE IN1-PRARTBEL        TO UT1-PRARTBEL                             
602600     MOVE IN1-FLLSRDEL        TO UT1-FLLSRDEL                             
602700     MOVE IN1-IDPROJUP        TO UT1-IDPROJUP                             
602710     MOVE IN1-FLGEMFMC        TO UT1-FLGEMFMC                             
602800     MOVE IN1-TIURPROD        TO UT1-TIURPROD                             
602900     PERFORM S11-SKRIV-W33544U-401                                        
603000     .                                                                    
603100     EJECT                                                                
603200 CB-FLYTTA-VERS-402-TILL-UTFIL SECTION.                                   
603300     SKIP2                                                                
603400     MOVE IN2-IDPTYP          TO UT2-IDPTYP                               
603500     MOVE IN2-IDVTYP          TO UT2-IDVTYP                               
603600     MOVE IN2-TEORSAK         TO UT2-TEORSAK                              
603700     MOVE IN2-TEARTNOT-3      TO UT2-TEARTNOT-3                           
603710     MOVE IN2-TEARTNOT-7      TO UT2-TEARTNOT-7                           
603800     PERFORM S12-SKRIV-W33544U-402                                        
603900     .                                                                    
604000     EJECT                                                                
604100                                                                          
604200 CC-FLYTTA-VERS-403-TILL-UTFIL SECTION.                                   
604300     SKIP2                                                                
604400     MOVE IN3-IDPTYP          TO UT3-IDPTYP                               
604500     MOVE IN3-IDVTYP          TO UT3-IDVTYP                               
604600     MOVE IN3-BEART(1)        TO UT3-BEART(1)                             
604700     MOVE IN3-BEART(2)        TO UT3-BEART(2)                             
604800     PERFORM S13-SKRIV-W33544U-403                                        
604900     .                                                                    
605000     EJECT                                                                
605100 Z-FINIT SECTION.                                                         
605200                                                                          
605300     CLOSE                                                                
605400           W33544I                                                        
605500           W33544U                                                        
605600           W33544V                                                        
605700           W33543                                                         
605800                                                                          
605900     MOVE 'S' TO POSTSUM-OPKOD                                            
606000     CALL POSTSUM USING POSTSUM-PARM                                      
606100     .                                                                    
606200     EJECT                                                                
606300 S02-LAES-W33544I SECTION.                                                
606400                                                                          
606500     READ W33544I INTO IN-AREA                                            
606600     AT END                                                               
606700        SET END-OF-W33544I TO TRUE                                        
606800                                                                          
606900     NOT AT END                                                           
607000        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
607100        MOVE 'W33544'   TO POSTSUM-FDNAMN                                 
607200        MOVE 'W33542D2' TO POSTSUM-DDNAMN2                                
607300        CALL POSTSUM USING POSTSUM-PARM                                   
607400     END-READ                                                             
607500     .                                                                    
607600     EJECT                                                                
607700 S03-LAES-W33543  SECTION.                                                
607800                                                                          
607900     READ W33543  INTO ANTAL-AREA                                         
608000     AT END                                                               
608100        SET END-OF-W33543  TO TRUE                                        
608200                                                                          
608300     NOT AT END                                                           
608400        MOVE 'ANT'      TO POSTSUM-TRANSTYP                               
608500        MOVE 'W33543'   TO POSTSUM-FDNAMN                                 
608600        MOVE 'W33542D5' TO POSTSUM-DDNAMN2                                
608700        CALL POSTSUM USING POSTSUM-PARM                                   
608800     END-READ                                                             
608900     .                                                                    
609000     EJECT                                                                
609100 S10-SKRIV-W33544V-000 SECTION.                                           
609200                                                                          
609300     MOVE ANTAL-IDPTYP TO UTVCOM0-IDPTYP                                  
609400     MOVE ANTAL-IDVTYP TO UTVCOM0-IDVTYP                                  
609500     MOVE ANTAL-KVPOST TO UTVCOM0-KVPOST                                  
609600     WRITE UTVCOM0-POST FROM UTVCOM-AREA                                  
609700                                                                          
609800     MOVE 'PT0 '     TO POSTSUM-TRANSTYP                                  
609900     MOVE 'W33544'   TO POSTSUM-FDNAMN                                    
610000     MOVE 'W33542D4' TO POSTSUM-DDNAMN2                                   
610100     CALL POSTSUM USING POSTSUM-PARM                                      
610200     .                                                                    
610300     EJECT                                                                
610400 S11-SKRIV-W33544U-401 SECTION.                                           
610500                                                                          
610600     WRITE UT1-POST FROM UT-AREA                                          
610700                                                                          
610800     MOVE 'UT1'      TO POSTSUM-TRANSTYP                                  
610900     MOVE 'W33544'   TO POSTSUM-FDNAMN                                    
611000     MOVE 'W33542D3' TO POSTSUM-DDNAMN2                                   
611100     CALL POSTSUM USING POSTSUM-PARM                                      
611200     .                                                                    
611300     EJECT                                                                
611400 S12-SKRIV-W33544U-402 SECTION.                                           
611500                                                                          
611600     WRITE UT2-POST FROM UT-AREA                                          
611700                                                                          
611800     MOVE 'UT2'      TO POSTSUM-TRANSTYP                                  
611900     MOVE 'W33544'   TO POSTSUM-FDNAMN                                    
612000     MOVE 'W33542D3' TO POSTSUM-DDNAMN2                                   
612100     CALL POSTSUM USING POSTSUM-PARM                                      
612200     .                                                                    
612300     EJECT                                                                
612400 S13-SKRIV-W33544U-403 SECTION.                                           
612500                                                                          
612600     WRITE UT3-POST FROM UT-AREA                                          
612700                                                                          
612800     MOVE 'UT3'      TO POSTSUM-TRANSTYP                                  
612900     MOVE 'W33544'   TO POSTSUM-FDNAMN                                    
613000     MOVE 'W33542D3' TO POSTSUM-DDNAMN2                                   
613100     CALL POSTSUM USING POSTSUM-PARM                                      
613200     .                                                                    
613300     EJECT                                                                
613400 S14-SKRIV-W33544V-401 SECTION.                                           
613500                                                                          
613600     WRITE UTVCOM1-POST FROM UTVCOM-AREA                                  
613700                                                                          
613800     MOVE 'PT1'      TO POSTSUM-TRANSTYP                                  
613900     MOVE 'W33544'   TO POSTSUM-FDNAMN                                    
614000     MOVE 'W33542D4' TO POSTSUM-DDNAMN2                                   
614100     CALL POSTSUM USING POSTSUM-PARM                                      
614200     .                                                                    
614300     EJECT                                                                
614400 S15-SKRIV-W33544V-402 SECTION.                                           
614500                                                                          
614600     WRITE UTVCOM2-POST FROM UTVCOM-AREA                                  
614700                                                                          
614800     MOVE 'PT2'      TO POSTSUM-TRANSTYP                                  
614900     MOVE 'W33544'   TO POSTSUM-FDNAMN                                    
615000     MOVE 'W33542D4' TO POSTSUM-DDNAMN2                                   
615100     CALL POSTSUM USING POSTSUM-PARM                                      
615200     .                                                                    
615300     EJECT                                                                
615400 S16-SKRIV-W33544V-403 SECTION.                                           
615500                                                                          
615600     WRITE UTVCOM3-POST FROM UTVCOM-AREA                                  
615700                                                                          
615800     MOVE 'PT3'      TO POSTSUM-TRANSTYP                                  
615900     MOVE 'W33544'   TO POSTSUM-FDNAMN                                    
616000     MOVE 'W33542D4' TO POSTSUM-DDNAMN2                                   
616100     CALL POSTSUM USING POSTSUM-PARM                                      
616200     .                                                                    
616300     EJECT                                                                
616400 S21-IDLEVNR-ALPHA-TO-NUM SECTION.                                        
616500* I/P : W-IDLEVNR-ALPHA     O/P : W-IDLEVNR-NUM                           
616600                                                                          
616700     MOVE ZERO                TO W-TALLY                                  
616800     INSPECT W-IDLEVNR-ALPHA TALLYING W-TALLY                             
616900             FOR CHARACTERS BEFORE INITIAL SPACE                          
617000     IF W-TALLY = ZERO                                                    
617100        MOVE ZERO             TO W-IDLEVNR-NUM                            
617200     ELSE                                                                 
617300        MOVE W-IDLEVNR-ALPHA(1:W-TALLY)                                   
617400                              TO W-IDLEVNR-NUM                            
617500     END-IF                                                               
617600     .                                                                    
617700     EJECT                                                                
617800                                                                          
617900 S99-ABEND SECTION.                                                       
618000                                                                          
618100     MOVE 'S' TO POSTSUM-OPKOD                                            
618200     CALL POSTSUM USING POSTSUM-PARM                                      
618300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
618400     .                                                                    
