000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3354100.                                                
000400*AUTHOR.         RONNY STENHOLM                                           
000500*DATE-WRITTEN.   APRIL 1994.                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*          PROGRAMMET LÄSER FIL INEHÅLLANDE ARTIKELINFO TILL              
001100*        MARKNADSBOLAGEN. AV DENNA FIL SKAPAS "LAGOM STOR" UTFIL          
001200*        SOM SKICKAS VIA VCOM. EV. RESTERANDE POSTER SKRIVS SOM           
001300*        NY GENERATION AV INFILEN.                                        
001400*          OM RESTERANDE POSTER FINNS LÄMNAS RETURKOD 8.RETURKODEN        
001500*        TESTAS SEDAN I JCL OCH OM DEN ÄR 8 BESTÄLLS JOBBET               
001600*        IGEN OCH DEN NYA GENERATIONEN TAS IN FÖR ATT                     
001700*        KUNNA SKICKA RESTERANDE POSTER OSV.                              
001800*                                                                         
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 - FEL COPYTEXTVERSION                                      
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400*    CHANGE LOG:                                                          
002500*                                                                         
002600*    DVK  17/04/02                                                        
002700*    FILE LAYOUT OF W33540V IS CHANGED TO HAVE THE IDLEVNR AND            
002800*    SLAG-IDLEVNR IN COMP-3 AND ALPHANUMERIC FORMAT. NEW COPY BOOK        
002900*    W335401B IS USED INSTEAD OF W335401A FOR W33540V.                    
003000*    MAX-POSTER IS CHANGED FROM 60000 TO 58000 SINCE RECORD LENGTH        
003100*    IS CHANGED FROM 160 TO 170.                                          
003200*                                                                         
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP2                                                                
003700 INPUT-OUTPUT SECTION.                                                    
003800                                                                          
003900 FILE-CONTROL.                                                            
004000     SKIP2                                                                
004100*          --- FIL MED FÖRÄNDR. PÅ ART.REG TILL MB                        
004200*          --- POSTER ATT SÄNDA TILL MB.                                  
004300     SELECT W33540I                    ASSIGN TO W33541D2.                
004400     SKIP2                                                                
004500*          --- FIL MED FÖRÄNDR. PÅ ART.REG TILL MB                        
004600*          --- POSTER KVAR ATT SÄNDA EFTER DENNA SÄNDNING.                
004700     SELECT W33540U                    ASSIGN TO W33541D3.                
004800     SKIP2                                                                
004900*          --- FIL MED FÖRÄNDR. PÅ ART.REG TILL MB                        
005000*          --- 'DEL-FIL' ATT SÄNDA VIA VCOM                               
005100     SELECT W33540V                    ASSIGN TO W33541D4.                
005200     SKIP2                                                                
005300*          --- FIL INNEHÅLLANDE EN POST.                                  
005400*          --- DENNA POST INNEHÅLLER DET TOTALA ANTALET TRANSAR.          
005500     SELECT W33541                     ASSIGN TO W33541D5.                
005600     EJECT                                                                
005700 DATA DIVISION.                                                           
005800     SKIP3                                                                
005900 FILE SECTION.                                                            
006000     SKIP3                                                                
006100 FD  W33540I                                                              
006200     RECORDING       V                                                    
006300     BLOCK CONTAINS  0.                                                   
006400     SKIP2                                                                
006500*01  -COPY W335401A      -L.                                              
006600     SKIP3                                                                
006700*01  -COPY W335402A      -L.                                              
006800     SKIP3                                                                
006900*01  -COPY W335403A      -L.                                              
007000     SKIP3                                                                
007100 FD  W33540U                                                              
007200     RECORDING       V                                                    
007300     BLOCK CONTAINS  0.                                                   
007400     SKIP2                                                                
007500*01  POST -COPY W335401A -PRE  UT1-  -L.                                  
007600*01  POST -COPY W335402A -PRE  UT2-  -L.                                  
007700*01  POST -COPY W335403A -PRE  UT3-  -L.                                  
007800     SKIP3                                                                
007900 FD  W33540V                                                              
008000     RECORDING       V                                                    
008100     BLOCK CONTAINS  0.                                                   
008200     SKIP2                                                                
008300*01  POST -COPY W335401B -PRE  UTVCOM1-  -L.                              
008400*01  POST -COPY W335400A -PRE  UTVCOM0-  -L.                              
008500*01  POST -COPY W335402A -PRE  UTVCOM2-  -L.                              
008600*01  POST -COPY W335403A -PRE  UTVCOM3-  -L.                              
008700     EJECT                                                                
008800 FD  W33541                                                               
008900     RECORDING       F                                                    
009000     BLOCK CONTAINS  0.                                                   
009100     SKIP2                                                                
009200*01  POST -COPY W335400A -PRE  ANTAL-  -L.                                
009300     EJECT                                                                
009400 WORKING-STORAGE SECTION.                                                 
009500     SKIP2                                                                
009600                                                                          
009700*    -- CHECKED BY WY2000                                                 
009800 77  IDPGM                       PIC X(8)    VALUE 'W3354100'.            
009900 77  JA                          PIC X       VALUE 'J'.                   
010000 77  NEJ                         PIC X       VALUE 'N'.                   
010100 77  WS-POSTRAKNARE              PIC 9(5)    VALUE ZERO.                  
010200 77  MAX-POSTER                  PIC 9(5)    VALUE 58000.                 
010300 77  RETURKOD                    PIC S9(2)   COMP-3 VALUE ZERO.           
010400                                                                          
010500                                                                          
010600 77  W33540I-EOF-SW              PIC X       VALUE 'N'.                   
010700     88  END-OF-W33540I                      VALUE 'J'.                   
010800     EJECT                                                                
010900 77  W33541-EOF-SW              PIC X       VALUE 'N'.                    
011000     88  END-OF-W33541                      VALUE 'J'.                    
011100     EJECT                                                                
011130     EJECT                                                                
011200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011300 01  FILLER REDEFINES DAGENS-DATUM.                                       
011400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
011600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
011700     EJECT                                                                
011800*    --- PARAMETERS FOR ALPHA-TO-NUM                                      
011900 77  W-IDLEVNR-NUM               PIC S9(5)   COMP-3.                      
012000 77  W-IDLEVNR-ALPHA             PIC X(5).                                
012100 77  W-TALLY                     PIC 9(5).                                
012200                                                                          
012300 01  DYNAMISKA-SUBPROGRAM.                                                
012400*                                                                         
012500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012700     SKIP2                                                                
012800*    --- PARAMETRAR TILL ABEND                                            
012900                                                                          
013000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013200     SKIP2                                                                
013300 01  FELTEXT.                                                             
013400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL POSTSUM                                          
013800*                                                                         
013900*01  -COPY W0005   -PRE  POSTSUM-                                         
014000     EJECT                                                                
014100 01  IN-AREA-START               PIC X(24)   VALUE                        
014200                                 'IN-AREA-START  '.                       
014300     SKIP2                                                                
014400 01  IN-AREA.                                                             
014500     03  FILLER                  PIC X(400).                              
014600*01  FILLER -COPY W335401A      -PRE IN1-   -RED  IN-AREA                 
014700*01  FILLER -COPY W335402A      -PRE IN2-   -RED  IN-AREA                 
014800*01  FILLER -COPY W335403A      -PRE IN3-   -RED  IN-AREA                 
014900     EJECT                                                                
015000 01  ANTAL-AREA-START               PIC X(24)   VALUE                     
015100                                 'ANTAL-AREA-START  '.                    
015200     SKIP2                                                                
015300 01  ANTAL-AREA.                                                          
015400     03  FILLER                  PIC X(50).                               
015500*01  FILLER -COPY W335400A      -PRE ANTAL-   -RED  ANTAL-AREA            
015600     EJECT                                                                
015700 01  UT-AREA-START               PIC X(24)   VALUE                        
015800                                 'UT-AREA-START  '.                       
015900     SKIP2                                                                
016000 01  UT-AREA.                                                             
016100     03  FILLER                  PIC X(400).                              
016200*01  FILLER -COPY W335401A      -PRE UT1-   -RED  UT-AREA                 
016300*01  FILLER -COPY W335402A      -PRE UT2-   -RED  UT-AREA                 
016400*01  FILLER -COPY W335403A      -PRE UT3-   -RED  UT-AREA                 
016500     EJECT                                                                
016600 01  UTVCOM-AREA-START           PIC X(24)   VALUE                        
016700                                 'UTVCOM-AREA-START  '.                   
016800     SKIP2                                                                
016900 01  UTVCOM-AREA.                                                         
017000     03  FILLER                  PIC X(400).                              
017100*01  FILLER -COPY W335400A      -PRE UTVCOM0-   -RED  UTVCOM-AREA         
017200*01  FILLER -COPY W335401B      -PRE UTVCOM1-   -RED  UTVCOM-AREA         
017300*01  FILLER -COPY W335402A      -PRE UTVCOM2-   -RED  UTVCOM-AREA         
017400*01  FILLER -COPY W335403A      -PRE UTVCOM3-   -RED  UTVCOM-AREA         
017500     EJECT                                                                
017600 PROCEDURE DIVISION.                                                      
017700                                                                          
017800     PERFORM A-INIT                                                       
017900     PERFORM S02-LAES-W33540I                                             
018000     PERFORM S03-LAES-W33541                                              
018100     PERFORM S10-SKRIV-W33540V-000                                        
018200     MOVE 1 TO WS-POSTRAKNARE                                             
018300                                                                          
018400     PERFORM UNTIL WS-POSTRAKNARE > MAX-POSTER OR                         
018500                   END-OF-W33540I                                         
018600                                                                          
018700       PERFORM B-FLYTTA-INPOST-TILL-VCOMFIL                               
018800                                                                          
018900       PERFORM S02-LAES-W33540I                                           
019000       ADD 1 TO WS-POSTRAKNARE                                            
019100     END-PERFORM                                                          
019200                                                                          
019300     IF NOT END-OF-W33540I                                                
019400*** FÖR ATT NÄSTA FIL SKA BÖRJA MED 401-POST.                             
019500       PERFORM UNTIL IN1-IDPTYP = '401'                                   
019600         EVALUATE IN1-IDPTYP                                              
019700           WHEN '402'                                                     
019800             PERFORM BB-FLYTTA-POST-402-TILL-VCOM                         
019900           WHEN '403'                                                     
020000             PERFORM BC-FLYTTA-POST-403-TILL-VCOM                         
020100         END-EVALUATE                                                     
020200         PERFORM S02-LAES-W33540I                                         
020300       END-PERFORM                                                        
020400     END-IF                                                               
020500     IF END-OF-W33540I                                                    
020600       MOVE ZERO TO RETURKOD                                              
020700     ELSE                                                                 
020800       MOVE +8   TO RETURKOD                                              
020900                                                                          
021000       PERFORM UNTIL END-OF-W33540I                                       
021100                                                                          
021200         PERFORM C-FLYTTA-INPOST-TILL-UTFIL                               
021300                                                                          
021400         PERFORM S02-LAES-W33540I                                         
021500       END-PERFORM                                                        
021600     END-IF                                                               
021700                                                                          
021800     PERFORM Z-FINIT                                                      
021900                                                                          
022000     MOVE RETURKOD TO RETURN-CODE                                         
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 A-INIT SECTION.                                                          
022500                                                                          
022600     OPEN INPUT                                                           
022700                 W33540I                                                  
022800                 W33541                                                   
022900                                                                          
023000     OPEN OUTPUT W33540U                                                  
023100                 W33540V                                                  
023200                                                                          
023300     ACCEPT DAGENS-DATUM FROM DATE                                        
023400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023500     .                                                                    
023600     EJECT                                                                
023700 B-FLYTTA-INPOST-TILL-VCOMFIL SECTION.                                    
023800                                                                          
023900     EVALUATE IN1-IDPTYP                                                  
024000       WHEN '401'                                                         
024100         PERFORM BA-FLYTTA-POST-401-TILL-VCOM                             
024200       WHEN '402'                                                         
024300         PERFORM BB-FLYTTA-POST-402-TILL-VCOM                             
024400       WHEN '403'                                                         
024500         PERFORM BC-FLYTTA-POST-403-TILL-VCOM                             
024600       WHEN OTHER                                                         
024700         STRING 'FEL COPYTEXTVERSION : '                                  
024800                IN1-IDPTYP                                                
024900           DELIMITED BY SIZE INTO FELTEXT-STR                             
025000         DISPLAY FELTEXT                                                  
025100         PERFORM S99-ABEND                                                
025200     END-EVALUATE                                                         
025300     .                                                                    
025400     EJECT                                                                
025500 BA-FLYTTA-POST-401-TILL-VCOM SECTION.                                    
025600                                                                          
025700     MOVE IN1-IDPTYP          TO UTVCOM1-IDPTYP                           
025800     MOVE IN1-IDVTYP          TO UTVCOM1-IDVTYP                           
025900     MOVE IN1-IDARTNR         TO UTVCOM1-IDARTNR                          
026000     MOVE IN1-IDFKNGRP        TO UTVCOM1-IDFKNGRP                         
026100     MOVE IN1-KDSRA           TO UTVCOM1-KDSRA                            
026200     MOVE IN1-KVQPACK-0       TO UTVCOM1-KVQPACK-0                        
026300     MOVE IN1-KDARTURS-NUM    TO UTVCOM1-KDARTURS-NUM                     
026400     MOVE IN1-KDPRODSL        TO UTVCOM1-KDPRODSL                         
026500     MOVE IN1-VLARTNTO        TO UTVCOM1-VLARTNTO                         
026600     MOVE IN1-VKART           TO UTVCOM1-VKART                            
026700     MOVE IN1-KDVSOP          TO UTVCOM1-KDVSOP                           
026800     MOVE IN1-IDSTATNR        TO UTVCOM1-IDSTATNR                         
026900     MOVE IN1-KDSORT          TO UTVCOM1-KDSORT                           
027000     MOVE IN1-KDERS           TO UTVCOM1-KDERS                            
027100     MOVE IN1-KDBPSR          TO UTVCOM1-KDBPSR                           
027200     MOVE IN1-KDBBCL          TO UTVCOM1-KDBBCL                           
027300     MOVE IN1-IDLEVNR         TO UTVCOM1-IDLEVNR                          
027400                                   W-IDLEVNR-ALPHA                        
027500****FIX FÖR ALFA LEVNR**************************                          
027600     IF W-IDLEVNR-ALPHA = 'BQ2QA'                                         
027700        MOVE '01385' TO W-IDLEVNR-ALPHA                                   
027800     END-IF                                                               
027900     IF W-IDLEVNR-ALPHA = 'D12YA'                                         
028000        MOVE '06414' TO W-IDLEVNR-ALPHA                                   
028100     END-IF                                                               
028200     IF W-IDLEVNR-ALPHA = 'D0KLA'                                         
028300        MOVE '06916' TO W-IDLEVNR-ALPHA                                   
028400     END-IF                                                               
028500*************************************************                         
028600     IF W-IDLEVNR-ALPHA = 'BJ7TB'                                         
028700        MOVE '10132' TO W-IDLEVNR-ALPHA  END-IF                           
028800     IF W-IDLEVNR-ALPHA = 'D6M4A'                                         
028900        MOVE '19064' TO W-IDLEVNR-ALPHA  END-IF                           
029000     IF W-IDLEVNR-ALPHA = 'BQCTA'                                         
029100        MOVE '19535' TO W-IDLEVNR-ALPHA  END-IF                           
029200     IF W-IDLEVNR-ALPHA = 'R14MA'                                         
029300        MOVE '19610' TO W-IDLEVNR-ALPHA  END-IF                           
029400     IF W-IDLEVNR-ALPHA = 'BQYGA'                                         
029500        MOVE '22   ' TO W-IDLEVNR-ALPHA  END-IF                           
029600     IF W-IDLEVNR-ALPHA = 'BLTLA'                                         
029700        MOVE '2591 ' TO W-IDLEVNR-ALPHA  END-IF                           
029800     IF W-IDLEVNR-ALPHA = 'F4P6B'                                         
029900        MOVE '4445 ' TO W-IDLEVNR-ALPHA  END-IF                           
030000     IF W-IDLEVNR-ALPHA = 'CN5CA'                                         
030100        MOVE '4893 ' TO W-IDLEVNR-ALPHA  END-IF                           
030200     IF W-IDLEVNR-ALPHA = 'BWMZA'                                         
030300        MOVE '7630 ' TO W-IDLEVNR-ALPHA  END-IF                           
030400     IF W-IDLEVNR-ALPHA = 'CFT2B'                                         
030500        MOVE '7949 ' TO W-IDLEVNR-ALPHA  END-IF                           
030600*************************************************                         
030700     IF W-IDLEVNR-ALPHA = 'BWS5A'                                         
030800        MOVE '132  ' TO W-IDLEVNR-ALPHA  END-IF                           
030900     IF W-IDLEVNR-ALPHA = 'BMBQA'                                         
031000        MOVE '226  ' TO W-IDLEVNR-ALPHA  END-IF                           
031100     IF W-IDLEVNR-ALPHA = 'BKFVA'                                         
031200        MOVE '346  ' TO W-IDLEVNR-ALPHA  END-IF                           
031300     IF W-IDLEVNR-ALPHA = 'BWS9A'                                         
031400        MOVE '350  ' TO W-IDLEVNR-ALPHA  END-IF                           
031500     IF W-IDLEVNR-ALPHA = 'R9KSA'                                         
031600        MOVE '500  ' TO W-IDLEVNR-ALPHA  END-IF                           
031700     IF W-IDLEVNR-ALPHA = 'BQ2MA'                                         
031800        MOVE '1362 ' TO W-IDLEVNR-ALPHA  END-IF                           
031900     IF W-IDLEVNR-ALPHA = 'BQ2RA'                                         
032000        MOVE '1389 ' TO W-IDLEVNR-ALPHA  END-IF                           
032100     IF W-IDLEVNR-ALPHA = 'BPW9A'                                         
032200        MOVE '1425 ' TO W-IDLEVNR-ALPHA  END-IF                           
032300     IF W-IDLEVNR-ALPHA = 'DDDPA'                                         
032400        MOVE '1594 ' TO W-IDLEVNR-ALPHA  END-IF                           
032500     IF W-IDLEVNR-ALPHA = 'BQ3BA'                                         
032600        MOVE '1603 ' TO W-IDLEVNR-ALPHA  END-IF                           
032700     IF W-IDLEVNR-ALPHA = 'CFNKA'                                         
032800        MOVE '1675 ' TO W-IDLEVNR-ALPHA  END-IF                           
032900     IF W-IDLEVNR-ALPHA = 'MNDLA'                                         
033000        MOVE '2299 ' TO W-IDLEVNR-ALPHA  END-IF                           
033100     IF W-IDLEVNR-ALPHA = 'BQ6AA'                                         
033200        MOVE '2507 ' TO W-IDLEVNR-ALPHA  END-IF                           
033300     IF W-IDLEVNR-ALPHA = 'CFN0A'                                         
033400        MOVE '3664 ' TO W-IDLEVNR-ALPHA  END-IF                           
033500     IF W-IDLEVNR-ALPHA = 'CFN9A'                                         
033600        MOVE '3718 ' TO W-IDLEVNR-ALPHA  END-IF                           
033700     IF W-IDLEVNR-ALPHA = 'C96AA'                                         
033800        MOVE '5183 ' TO W-IDLEVNR-ALPHA  END-IF                           
033900     IF W-IDLEVNR-ALPHA = 'D0RED'                                         
034000        MOVE '6323 ' TO W-IDLEVNR-ALPHA  END-IF                           
034100     IF W-IDLEVNR-ALPHA = 'D0REB'                                         
034200        MOVE '6704 ' TO W-IDLEVNR-ALPHA  END-IF                           
034300     IF W-IDLEVNR-ALPHA = 'D0REA'                                         
034400        MOVE '6770 ' TO W-IDLEVNR-ALPHA  END-IF                           
034500     IF W-IDLEVNR-ALPHA = 'BUA7A'                                         
034600        MOVE '10108' TO W-IDLEVNR-ALPHA  END-IF                           
034700     IF W-IDLEVNR-ALPHA = 'AGPBA'                                         
034800        MOVE '14829' TO W-IDLEVNR-ALPHA  END-IF                           
034900*************************************************                         
035000     IF W-IDLEVNR-ALPHA = 'BYLRA'                                         
035100        MOVE '839  ' TO W-IDLEVNR-ALPHA  END-IF                           
035200     IF W-IDLEVNR-ALPHA = 'BQ2DA'                                         
035300        MOVE '1100 ' TO W-IDLEVNR-ALPHA  END-IF                           
035400     IF W-IDLEVNR-ALPHA = 'BKWRA'                                         
035500        MOVE '1205 ' TO W-IDLEVNR-ALPHA  END-IF                           
035600     IF W-IDLEVNR-ALPHA = 'BQ2HA'                                         
035700        MOVE '1285 ' TO W-IDLEVNR-ALPHA  END-IF                           
035800     IF W-IDLEVNR-ALPHA = 'N81NA'                                         
035900        MOVE '1336 ' TO W-IDLEVNR-ALPHA  END-IF                           
036000     IF W-IDLEVNR-ALPHA = 'S51YA'                                         
036100        MOVE '1605 ' TO W-IDLEVNR-ALPHA  END-IF                           
036200     IF W-IDLEVNR-ALPHA = 'AHTXA'                                         
036300        MOVE '3948 ' TO W-IDLEVNR-ALPHA  END-IF                           
036400     IF W-IDLEVNR-ALPHA = 'R500F'                                         
036500        MOVE '4034 ' TO W-IDLEVNR-ALPHA  END-IF                           
036600     IF W-IDLEVNR-ALPHA = 'K4UKA'                                         
036700        MOVE '4937 ' TO W-IDLEVNR-ALPHA  END-IF                           
036800     IF W-IDLEVNR-ALPHA = 'S3ULA'                                         
036900        MOVE '4979 ' TO W-IDLEVNR-ALPHA  END-IF                           
037000     IF W-IDLEVNR-ALPHA = 'C9F8A'                                         
037100        MOVE '5012 ' TO W-IDLEVNR-ALPHA  END-IF                           
037200     IF W-IDLEVNR-ALPHA = 'CFN1A'                                         
037300        MOVE '5145 ' TO W-IDLEVNR-ALPHA  END-IF                           
037400     IF W-IDLEVNR-ALPHA = 'BVNUA'                                         
037500        MOVE '5197 ' TO W-IDLEVNR-ALPHA  END-IF                           
037600     IF W-IDLEVNR-ALPHA = 'BQ7BA'                                         
037700        MOVE '5356 ' TO W-IDLEVNR-ALPHA  END-IF                           
037800     IF W-IDLEVNR-ALPHA = 'T727Z'                                         
037900        MOVE '5645 ' TO W-IDLEVNR-ALPHA  END-IF                           
038000     IF W-IDLEVNR-ALPHA = 'U494Q'                                         
038100        MOVE '5868 ' TO W-IDLEVNR-ALPHA  END-IF                           
038200     IF W-IDLEVNR-ALPHA = 'D35VA'                                         
038300        MOVE '6543 ' TO W-IDLEVNR-ALPHA  END-IF                           
038400     IF W-IDLEVNR-ALPHA = 'FNT8A'                                         
038500        MOVE '6985 ' TO W-IDLEVNR-ALPHA  END-IF                           
038600     IF W-IDLEVNR-ALPHA = 'CFT0A'                                         
038700        MOVE '7139 ' TO W-IDLEVNR-ALPHA  END-IF                           
038800     IF W-IDLEVNR-ALPHA = 'CFT8A'                                         
038900        MOVE '8187 ' TO W-IDLEVNR-ALPHA  END-IF                           
039000     IF W-IDLEVNR-ALPHA = 'K4UKB'                                         
039100        MOVE '11577' TO W-IDLEVNR-ALPHA  END-IF                           
039200     IF W-IDLEVNR-ALPHA = 'H387D'                                         
039300        MOVE '14985' TO W-IDLEVNR-ALPHA  END-IF                           
039400     IF W-IDLEVNR-ALPHA = 'S12HA'                                         
039500        MOVE '16076' TO W-IDLEVNR-ALPHA  END-IF                           
039600     IF W-IDLEVNR-ALPHA = 'BPXJA'                                         
039700        MOVE '16265' TO W-IDLEVNR-ALPHA  END-IF                           
039800     IF W-IDLEVNR-ALPHA = 'CFT8B'                                         
039900        MOVE '22410' TO W-IDLEVNR-ALPHA  END-IF                           
040000     IF W-IDLEVNR-ALPHA = 'C9G6A'                                         
040100        MOVE '24030' TO W-IDLEVNR-ALPHA  END-IF                           
040200     IF W-IDLEVNR-ALPHA = 'S12HE'                                         
040300        MOVE '24331' TO W-IDLEVNR-ALPHA  END-IF                           
040400*************************************************                         
040500     IF W-IDLEVNR-ALPHA = 'BQ1FA'                                         
040600        MOVE '640  ' TO W-IDLEVNR-ALPHA  END-IF                           
040700     IF W-IDLEVNR-ALPHA = 'BK3DA'                                         
040800        MOVE '813  ' TO W-IDLEVNR-ALPHA  END-IF                           
040900     IF W-IDLEVNR-ALPHA = 'N81FA'                                         
041000        MOVE '836  ' TO W-IDLEVNR-ALPHA  END-IF                           
041100     IF W-IDLEVNR-ALPHA = 'BQ1JA'                                         
041200        MOVE '845  ' TO W-IDLEVNR-ALPHA  END-IF                           
041300     IF W-IDLEVNR-ALPHA = 'BQ1KA'                                         
041400        MOVE '850  ' TO W-IDLEVNR-ALPHA  END-IF                           
041500     IF W-IDLEVNR-ALPHA = 'BQ1LA'                                         
041600        MOVE '861  ' TO W-IDLEVNR-ALPHA  END-IF                           
041700     IF W-IDLEVNR-ALPHA = 'BHFCA'                                         
041800        MOVE '927  ' TO W-IDLEVNR-ALPHA  END-IF                           
041900     IF W-IDLEVNR-ALPHA = 'BKCKA'                                         
042000        MOVE '930  ' TO W-IDLEVNR-ALPHA  END-IF                           
042100     IF W-IDLEVNR-ALPHA = 'N81JA'                                         
042200        MOVE '933  ' TO W-IDLEVNR-ALPHA  END-IF                           
042300     IF W-IDLEVNR-ALPHA = 'BQ2XA'                                         
042400        MOVE '1326 ' TO W-IDLEVNR-ALPHA  END-IF                           
042500     IF W-IDLEVNR-ALPHA = 'BLU5A'                                         
042600        MOVE '1659 ' TO W-IDLEVNR-ALPHA  END-IF                           
042700     IF W-IDLEVNR-ALPHA = 'BQ2YA'                                         
042800        MOVE '1977 ' TO W-IDLEVNR-ALPHA  END-IF                           
042900     IF W-IDLEVNR-ALPHA = 'CN5GA'                                         
043000        MOVE '7186 ' TO W-IDLEVNR-ALPHA  END-IF                           
043100     IF W-IDLEVNR-ALPHA = 'D0FTA'                                         
043200        MOVE '7208 ' TO W-IDLEVNR-ALPHA  END-IF                           
043300     IF W-IDLEVNR-ALPHA = 'FLZ6B'                                         
043400        MOVE '16154' TO W-IDLEVNR-ALPHA  END-IF                           
043500     IF W-IDLEVNR-ALPHA = 'S9GAA'                                         
043600        MOVE '18086' TO W-IDLEVNR-ALPHA  END-IF                           
043700*************************************************                         
043800     IF W-IDLEVNR-ALPHA = 'BWTDA'                                         
043900        MOVE '548  ' TO W-IDLEVNR-ALPHA  END-IF                           
044000     IF W-IDLEVNR-ALPHA = 'BWTFA'                                         
044100        MOVE '598  ' TO W-IDLEVNR-ALPHA  END-IF                           
044200     IF W-IDLEVNR-ALPHA = 'BQ1CA'                                         
044300        MOVE '605  ' TO W-IDLEVNR-ALPHA  END-IF                           
044400     IF W-IDLEVNR-ALPHA = 'DBHJA'                                         
044500        MOVE '667  ' TO W-IDLEVNR-ALPHA  END-IF                           
044600     IF W-IDLEVNR-ALPHA = 'BQ1MA'                                         
044700        MOVE '890  ' TO W-IDLEVNR-ALPHA  END-IF                           
044800     IF W-IDLEVNR-ALPHA = 'CD2JA'                                         
044900        MOVE '897  ' TO W-IDLEVNR-ALPHA  END-IF                           
045000     IF W-IDLEVNR-ALPHA = 'BLRQA'                                         
045100        MOVE '929  ' TO W-IDLEVNR-ALPHA  END-IF                           
045200     IF W-IDLEVNR-ALPHA = 'BQ2PA'                                         
045300        MOVE '1380 ' TO W-IDLEVNR-ALPHA  END-IF                           
045400     IF W-IDLEVNR-ALPHA = 'BQ6QB'                                         
045500        MOVE '1386 ' TO W-IDLEVNR-ALPHA  END-IF                           
045600     IF W-IDLEVNR-ALPHA = 'BSBZA'                                         
045700        MOVE '1560 ' TO W-IDLEVNR-ALPHA  END-IF                           
045800     IF W-IDLEVNR-ALPHA = 'BQ3EA'                                         
045900        MOVE '1728 ' TO W-IDLEVNR-ALPHA  END-IF                           
046000     IF W-IDLEVNR-ALPHA = 'BQ3NA'                                         
046100        MOVE '1736 ' TO W-IDLEVNR-ALPHA  END-IF                           
046200     IF W-IDLEVNR-ALPHA = 'BQ8AA'                                         
046300        MOVE '1783 ' TO W-IDLEVNR-ALPHA  END-IF                           
046400     IF W-IDLEVNR-ALPHA = 'BK6ZA'                                         
046500        MOVE '1809 ' TO W-IDLEVNR-ALPHA  END-IF                           
046600     IF W-IDLEVNR-ALPHA = 'BQ5MA'                                         
046700        MOVE '1978 ' TO W-IDLEVNR-ALPHA  END-IF                           
046800     IF W-IDLEVNR-ALPHA = 'BQ5NA'                                         
046900        MOVE '1982 ' TO W-IDLEVNR-ALPHA  END-IF                           
047000     IF W-IDLEVNR-ALPHA = 'S52HA'                                         
047100        MOVE '2087 ' TO W-IDLEVNR-ALPHA  END-IF                           
047200     IF W-IDLEVNR-ALPHA = 'CFNYA'                                         
047300        MOVE '3380 ' TO W-IDLEVNR-ALPHA  END-IF                           
047400     IF W-IDLEVNR-ALPHA = 'E622D'                                         
047500        MOVE '3654 ' TO W-IDLEVNR-ALPHA  END-IF                           
047600     IF W-IDLEVNR-ALPHA = 'D3D4A'                                         
047700        MOVE '6881 ' TO W-IDLEVNR-ALPHA  END-IF                           
047800     IF W-IDLEVNR-ALPHA = 'BQ7YC'                                         
047900        MOVE '7314 ' TO W-IDLEVNR-ALPHA  END-IF                           
048000     IF W-IDLEVNR-ALPHA = 'BQ7YB'                                         
048100        MOVE '7369 ' TO W-IDLEVNR-ALPHA  END-IF                           
048200     IF W-IDLEVNR-ALPHA = 'BQ7YA'                                         
048300        MOVE '7420 ' TO W-IDLEVNR-ALPHA  END-IF                           
048400     IF W-IDLEVNR-ALPHA = 'BPVBC'                                         
048500        MOVE '7881 ' TO W-IDLEVNR-ALPHA  END-IF                           
048600     IF W-IDLEVNR-ALPHA = 'BPVBB'                                         
048700        MOVE '10096' TO W-IDLEVNR-ALPHA  END-IF                           
048800     IF W-IDLEVNR-ALPHA = 'CFNZB'                                         
048900        MOVE '13691' TO W-IDLEVNR-ALPHA  END-IF                           
049000     IF W-IDLEVNR-ALPHA = 'U7SAD'                                         
049100        MOVE '14996' TO W-IDLEVNR-ALPHA  END-IF                           
049200     IF W-IDLEVNR-ALPHA = 'AN3AB'                                         
049300        MOVE '16226' TO W-IDLEVNR-ALPHA  END-IF                           
049400     IF W-IDLEVNR-ALPHA = 'U910A'                                         
049500        MOVE '24719' TO W-IDLEVNR-ALPHA  END-IF                           
049600     IF W-IDLEVNR-ALPHA = 'J5C2B'                                         
049700        MOVE '25016' TO W-IDLEVNR-ALPHA  END-IF                           
049800     IF W-IDLEVNR-ALPHA = 'LMJGA'                                         
049900        MOVE '25676' TO W-IDLEVNR-ALPHA  END-IF                           
050000     IF W-IDLEVNR-ALPHA = 'BPVBA'                                         
050100        MOVE '13445' TO W-IDLEVNR-ALPHA  END-IF                           
050200*************************************************                         
050300*************************************************                         
050400     IF W-IDLEVNR-ALPHA = 'BQ8ZD'                                         
050500        MOVE '93   ' TO W-IDLEVNR-ALPHA  END-IF                           
050600     IF W-IDLEVNR-ALPHA = 'BQ9CB'                                         
050700        MOVE '234  ' TO W-IDLEVNR-ALPHA  END-IF                           
050800     IF W-IDLEVNR-ALPHA = 'J2A6B'                                         
050900        MOVE '386  ' TO W-IDLEVNR-ALPHA  END-IF                           
051000     IF W-IDLEVNR-ALPHA = 'BQ8ZB'                                         
051100        MOVE '607  ' TO W-IDLEVNR-ALPHA  END-IF                           
051200     IF W-IDLEVNR-ALPHA = 'CXC6A'                                         
051300        MOVE '780  ' TO W-IDLEVNR-ALPHA  END-IF                           
051400     IF W-IDLEVNR-ALPHA = 'C7Q2D'                                         
051500        MOVE '835  ' TO W-IDLEVNR-ALPHA  END-IF                           
051600     IF W-IDLEVNR-ALPHA = 'BQ9AA'                                         
051700        MOVE '1187 ' TO W-IDLEVNR-ALPHA  END-IF                           
051800     IF W-IDLEVNR-ALPHA = 'C7Q2B'                                         
051900        MOVE '1228 ' TO W-IDLEVNR-ALPHA  END-IF                           
052000     IF W-IDLEVNR-ALPHA = 'BQ9CC'                                         
052100        MOVE '1393 ' TO W-IDLEVNR-ALPHA  END-IF                           
052200     IF W-IDLEVNR-ALPHA = 'BPU0A'                                         
052300        MOVE '2065 ' TO W-IDLEVNR-ALPHA  END-IF                           
052400     IF W-IDLEVNR-ALPHA = 'BQ5PA'                                         
052500        MOVE '2108 ' TO W-IDLEVNR-ALPHA  END-IF                           
052600     IF W-IDLEVNR-ALPHA = 'BPU0B'                                         
052700        MOVE '2157 ' TO W-IDLEVNR-ALPHA  END-IF                           
052800     IF W-IDLEVNR-ALPHA = 'BKVVA'                                         
052900        MOVE '2220 ' TO W-IDLEVNR-ALPHA  END-IF                           
053000     IF W-IDLEVNR-ALPHA = 'J2A6A'                                         
053100        MOVE '2288 ' TO W-IDLEVNR-ALPHA  END-IF                           
053200     IF W-IDLEVNR-ALPHA = 'BX0ZA'                                         
053300        MOVE '2289 ' TO W-IDLEVNR-ALPHA  END-IF                           
053400     IF W-IDLEVNR-ALPHA = 'BQ5RA'                                         
053500        MOVE '2315 ' TO W-IDLEVNR-ALPHA  END-IF                           
053600     IF W-IDLEVNR-ALPHA = 'D3F1A'                                         
053700        MOVE '2379 ' TO W-IDLEVNR-ALPHA  END-IF                           
053800     IF W-IDLEVNR-ALPHA = 'CXC6B'                                         
053900        MOVE '2545 ' TO W-IDLEVNR-ALPHA  END-IF                           
054000     IF W-IDLEVNR-ALPHA = 'D3F1B'                                         
054100        MOVE '2560 ' TO W-IDLEVNR-ALPHA  END-IF                           
054200     IF W-IDLEVNR-ALPHA = 'S053A'                                         
054300        MOVE '5033 ' TO W-IDLEVNR-ALPHA  END-IF                           
054400     IF W-IDLEVNR-ALPHA = 'P90CA'                                         
054500        MOVE '6664 ' TO W-IDLEVNR-ALPHA  END-IF                           
054600     IF W-IDLEVNR-ALPHA = 'BQ9CD'                                         
054700        MOVE '8294 ' TO W-IDLEVNR-ALPHA  END-IF                           
054800     IF W-IDLEVNR-ALPHA = 'BQ1QB'                                         
054900        MOVE '10220' TO W-IDLEVNR-ALPHA  END-IF                           
055000     IF W-IDLEVNR-ALPHA = 'BQ8UA'                                         
055100        MOVE '10374' TO W-IDLEVNR-ALPHA  END-IF                           
055200     IF W-IDLEVNR-ALPHA = 'N5MXB'                                         
055300        MOVE '14032' TO W-IDLEVNR-ALPHA  END-IF                           
055400     IF W-IDLEVNR-ALPHA = 'BQ9CE'                                         
055500        MOVE '19623' TO W-IDLEVNR-ALPHA  END-IF                           
055600     IF W-IDLEVNR-ALPHA = 'D3F1C'                                         
055700        MOVE '25979' TO W-IDLEVNR-ALPHA  END-IF                           
055800     IF W-IDLEVNR-ALPHA = 'D3F1D'                                         
055900        MOVE '25980' TO W-IDLEVNR-ALPHA  END-IF                           
056000     IF W-IDLEVNR-ALPHA = 'DMS1A'                                         
056100        MOVE '25923' TO W-IDLEVNR-ALPHA  END-IF                           
056200*************************************************                         
056300*************************************************                         
056400     IF W-IDLEVNR-ALPHA = 'BHFAA'                                         
056500        MOVE '4    ' TO W-IDLEVNR-ALPHA  END-IF                           
056600     IF W-IDLEVNR-ALPHA = 'CFM6A'                                         
056700        MOVE '32   ' TO W-IDLEVNR-ALPHA  END-IF                           
056800     IF W-IDLEVNR-ALPHA = 'BKR3A'                                         
056900        MOVE '64   ' TO W-IDLEVNR-ALPHA  END-IF                           
057000     IF W-IDLEVNR-ALPHA = 'CL3WA'                                         
057100        MOVE '75   ' TO W-IDLEVNR-ALPHA  END-IF                           
057200     IF W-IDLEVNR-ALPHA = 'BJPKA'                                         
057300        MOVE '81   ' TO W-IDLEVNR-ALPHA  END-IF                           
057400     IF W-IDLEVNR-ALPHA = 'CL3ZA'                                         
057500        MOVE '682  ' TO W-IDLEVNR-ALPHA  END-IF                           
057600     IF W-IDLEVNR-ALPHA = 'CXN9A'                                         
057700        MOVE '777  ' TO W-IDLEVNR-ALPHA  END-IF                           
057800     IF W-IDLEVNR-ALPHA = 'BLRUA'                                         
057900        MOVE '1952 ' TO W-IDLEVNR-ALPHA  END-IF                           
058000     IF W-IDLEVNR-ALPHA = 'BQ5UA'                                         
058100        MOVE '2387 ' TO W-IDLEVNR-ALPHA  END-IF                           
058200     IF W-IDLEVNR-ALPHA = 'BQ5YA'                                         
058300        MOVE '2437 ' TO W-IDLEVNR-ALPHA  END-IF                           
058400     IF W-IDLEVNR-ALPHA = 'BJR1A'                                         
058500        MOVE '2483 ' TO W-IDLEVNR-ALPHA  END-IF                           
058600     IF W-IDLEVNR-ALPHA = 'BQ5ZA'                                         
058700        MOVE '2495 ' TO W-IDLEVNR-ALPHA  END-IF                           
058800     IF W-IDLEVNR-ALPHA = 'BQ6BA'                                         
058900        MOVE '2539 ' TO W-IDLEVNR-ALPHA  END-IF                           
059000     IF W-IDLEVNR-ALPHA = 'CZ1SA'                                         
059100        MOVE '3730 ' TO W-IDLEVNR-ALPHA  END-IF                           
059200     IF W-IDLEVNR-ALPHA = 'F842A'                                         
059300        MOVE '4961 ' TO W-IDLEVNR-ALPHA  END-IF                           
059400     IF W-IDLEVNR-ALPHA = 'P7ZFA'                                         
059500        MOVE '4969 ' TO W-IDLEVNR-ALPHA  END-IF                           
059600     IF W-IDLEVNR-ALPHA = 'C9B7B'                                         
059700        MOVE '6757 ' TO W-IDLEVNR-ALPHA  END-IF                           
059800     IF W-IDLEVNR-ALPHA = 'BQ5VA'                                         
059900        MOVE '10370' TO W-IDLEVNR-ALPHA  END-IF                           
060000     IF W-IDLEVNR-ALPHA = 'R2FLG'                                         
060100        MOVE '11096' TO W-IDLEVNR-ALPHA  END-IF                           
060200     IF W-IDLEVNR-ALPHA = 'BPUKA'                                         
060300        MOVE '13360' TO W-IDLEVNR-ALPHA  END-IF                           
060400     IF W-IDLEVNR-ALPHA = 'BPUMA'                                         
060500        MOVE '13538' TO W-IDLEVNR-ALPHA  END-IF                           
060600     IF W-IDLEVNR-ALPHA = 'E019A'                                         
060700        MOVE '50535' TO W-IDLEVNR-ALPHA  END-IF                           
060800*************************************************                         
060900*************************************************                         
061000     IF W-IDLEVNR-ALPHA = 'LCWLA'                                         
061100        MOVE '139  ' TO W-IDLEVNR-ALPHA  END-IF                           
061200     IF W-IDLEVNR-ALPHA = 'CN4TA'                                         
061300        MOVE '793  ' TO W-IDLEVNR-ALPHA  END-IF                           
061400     IF W-IDLEVNR-ALPHA = 'DL2QA'                                         
061500        MOVE '3911 ' TO W-IDLEVNR-ALPHA  END-IF                           
061600     IF W-IDLEVNR-ALPHA = 'CBVLA'                                         
061700        MOVE '808  ' TO W-IDLEVNR-ALPHA  END-IF                           
061800     IF W-IDLEVNR-ALPHA = 'BMBMA'                                         
061900        MOVE '855  ' TO W-IDLEVNR-ALPHA  END-IF                           
062000     IF W-IDLEVNR-ALPHA = 'BQ3AA'                                         
062100        MOVE '1566 ' TO W-IDLEVNR-ALPHA  END-IF                           
062200     IF W-IDLEVNR-ALPHA = 'CFNMA'                                         
062300        MOVE '1859 ' TO W-IDLEVNR-ALPHA  END-IF                           
062400     IF W-IDLEVNR-ALPHA = 'CBSDA'                                         
062500        MOVE '1863 ' TO W-IDLEVNR-ALPHA  END-IF                           
062600     IF W-IDLEVNR-ALPHA = 'BUF5A'                                         
062700        MOVE '1902 ' TO W-IDLEVNR-ALPHA  END-IF                           
062800     IF W-IDLEVNR-ALPHA = 'BQ6EA'                                         
062900        MOVE '2609 ' TO W-IDLEVNR-ALPHA  END-IF                           
063000     IF W-IDLEVNR-ALPHA = 'BLUDA'                                         
063100        MOVE '3034 ' TO W-IDLEVNR-ALPHA  END-IF                           
063200     IF W-IDLEVNR-ALPHA = 'BKJDA'                                         
063300        MOVE '3152 ' TO W-IDLEVNR-ALPHA  END-IF                           
063400     IF W-IDLEVNR-ALPHA = 'BL1UA'                                         
063500        MOVE '3167 ' TO W-IDLEVNR-ALPHA  END-IF                           
063600     IF W-IDLEVNR-ALPHA = 'BQ6RA'                                         
063700        MOVE '3370 ' TO W-IDLEVNR-ALPHA  END-IF                           
063800     IF W-IDLEVNR-ALPHA = 'D3U3A'                                         
063900        MOVE '3538 ' TO W-IDLEVNR-ALPHA  END-IF                           
064000     IF W-IDLEVNR-ALPHA = 'S0H5D'                                         
064100        MOVE '3669 ' TO W-IDLEVNR-ALPHA  END-IF                           
064200     IF W-IDLEVNR-ALPHA = 'G5FPC'                                         
064300        MOVE '3722 ' TO W-IDLEVNR-ALPHA  END-IF                           
064400     IF W-IDLEVNR-ALPHA = 'V136C'                                         
064500        MOVE '3770 ' TO W-IDLEVNR-ALPHA  END-IF                           
064600     IF W-IDLEVNR-ALPHA = 'P38NA'                                         
064700        MOVE '3988 ' TO W-IDLEVNR-ALPHA  END-IF                           
064800     IF W-IDLEVNR-ALPHA = 'G5FPD'                                         
064900        MOVE '3994 ' TO W-IDLEVNR-ALPHA  END-IF                           
065000     IF W-IDLEVNR-ALPHA = 'E623F'                                         
065100        MOVE '4880 ' TO W-IDLEVNR-ALPHA  END-IF                           
065200     IF W-IDLEVNR-ALPHA = 'D3U2A'                                         
065300        MOVE '4942 ' TO W-IDLEVNR-ALPHA  END-IF                           
065400     IF W-IDLEVNR-ALPHA = 'D3K6A'                                         
065500        MOVE '5314 ' TO W-IDLEVNR-ALPHA  END-IF                           
065600     IF W-IDLEVNR-ALPHA = 'D3L3A'                                         
065700        MOVE '5362 ' TO W-IDLEVNR-ALPHA  END-IF                           
065800     IF W-IDLEVNR-ALPHA = 'BQYEA'                                         
065900        MOVE '25982' TO W-IDLEVNR-ALPHA  END-IF                           
066000     IF W-IDLEVNR-ALPHA = 'V2NYA'                                         
066100        MOVE '25847' TO W-IDLEVNR-ALPHA  END-IF                           
066200     IF W-IDLEVNR-ALPHA = 'DRTHA'                                         
066300        MOVE '25996' TO W-IDLEVNR-ALPHA  END-IF                           
066400     IF W-IDLEVNR-ALPHA = 'D3D0A'                                         
066500        MOVE '6014 ' TO W-IDLEVNR-ALPHA  END-IF                           
066600     IF W-IDLEVNR-ALPHA = 'CRGJA'                                         
066700        MOVE '6101 ' TO W-IDLEVNR-ALPHA  END-IF                           
066800     IF W-IDLEVNR-ALPHA = 'C6S4A'                                         
066900        MOVE '6200 ' TO W-IDLEVNR-ALPHA  END-IF                           
067000     IF W-IDLEVNR-ALPHA = 'Q6QLA'                                         
067100        MOVE '6346 ' TO W-IDLEVNR-ALPHA  END-IF                           
067200     IF W-IDLEVNR-ALPHA = 'D25KA'                                         
067300        MOVE '6810 ' TO W-IDLEVNR-ALPHA  END-IF                           
067400     IF W-IDLEVNR-ALPHA = 'E623B'                                         
067500        MOVE '6842 ' TO W-IDLEVNR-ALPHA  END-IF                           
067600     IF W-IDLEVNR-ALPHA = 'T7WFA'                                         
067700        MOVE '6996 ' TO W-IDLEVNR-ALPHA  END-IF                           
067800     IF W-IDLEVNR-ALPHA = 'BP8JA'                                         
067900        MOVE '10135' TO W-IDLEVNR-ALPHA  END-IF                           
068000     IF W-IDLEVNR-ALPHA = 'BP8JE'                                         
068100        MOVE '10483' TO W-IDLEVNR-ALPHA  END-IF                           
068200     IF W-IDLEVNR-ALPHA = 'R5YYA'                                         
068300        MOVE '10488' TO W-IDLEVNR-ALPHA  END-IF                           
068400     IF W-IDLEVNR-ALPHA = 'BP8JC'                                         
068500        MOVE '10493' TO W-IDLEVNR-ALPHA  END-IF                           
068600     IF W-IDLEVNR-ALPHA = 'Q5EGB'                                         
068700        MOVE '10494' TO W-IDLEVNR-ALPHA  END-IF                           
068800     IF W-IDLEVNR-ALPHA = 'Q5EGB'                                         
068900        MOVE '10494' TO W-IDLEVNR-ALPHA  END-IF                           
069000     IF W-IDLEVNR-ALPHA = 'BQ6RB'                                         
069100        MOVE '13389' TO W-IDLEVNR-ALPHA  END-IF                           
069200     IF W-IDLEVNR-ALPHA = 'M9TMB'                                         
069300        MOVE '13578' TO W-IDLEVNR-ALPHA  END-IF                           
069400     IF W-IDLEVNR-ALPHA = 'F432J'                                         
069500        MOVE '13594' TO W-IDLEVNR-ALPHA  END-IF                           
069600     IF W-IDLEVNR-ALPHA = 'D07LG'                                         
069700        MOVE '13612' TO W-IDLEVNR-ALPHA  END-IF                           
069800     IF W-IDLEVNR-ALPHA = 'D07LB'                                         
069900        MOVE '13614' TO W-IDLEVNR-ALPHA  END-IF                           
070000     IF W-IDLEVNR-ALPHA = 'CR9GA'                                         
070100        MOVE '13741' TO W-IDLEVNR-ALPHA  END-IF                           
070200     IF W-IDLEVNR-ALPHA = 'AN1JE'                                         
070300        MOVE '13793' TO W-IDLEVNR-ALPHA  END-IF                           
070400     IF W-IDLEVNR-ALPHA = 'BQJFA'                                         
070500        MOVE '15451' TO W-IDLEVNR-ALPHA  END-IF                           
070600     IF W-IDLEVNR-ALPHA = 'D25KD'                                         
070700        MOVE '16004' TO W-IDLEVNR-ALPHA  END-IF                           
070800     IF W-IDLEVNR-ALPHA = 'D25KE'                                         
070900        MOVE '16005' TO W-IDLEVNR-ALPHA  END-IF                           
071000     IF W-IDLEVNR-ALPHA = 'E623C'                                         
071100        MOVE '16039' TO W-IDLEVNR-ALPHA  END-IF                           
071200     IF W-IDLEVNR-ALPHA = 'B41YA'                                         
071300        MOVE '16080' TO W-IDLEVNR-ALPHA  END-IF                           
071400     IF W-IDLEVNR-ALPHA = 'E623E'                                         
071500        MOVE '16268' TO W-IDLEVNR-ALPHA  END-IF                           
071600     IF W-IDLEVNR-ALPHA = 'EKM4A'                                         
071700        MOVE '16403' TO W-IDLEVNR-ALPHA  END-IF                           
071800     IF W-IDLEVNR-ALPHA = 'BP8JD'                                         
071900        MOVE '17715' TO W-IDLEVNR-ALPHA  END-IF                           
072000     IF W-IDLEVNR-ALPHA = 'BKPTA'                                         
072100        MOVE '18203' TO W-IDLEVNR-ALPHA  END-IF                           
072200     IF W-IDLEVNR-ALPHA = 'ANEBA'                                         
072300        MOVE '19907' TO W-IDLEVNR-ALPHA  END-IF                           
072400     IF W-IDLEVNR-ALPHA = 'C72DC'                                         
072500        MOVE '22292' TO W-IDLEVNR-ALPHA  END-IF                           
072600     IF W-IDLEVNR-ALPHA = 'DDC3A'                                         
072700        MOVE '25729' TO W-IDLEVNR-ALPHA  END-IF                           
072800     IF W-IDLEVNR-ALPHA = 'AUJ7B'                                         
072900        MOVE '25749' TO W-IDLEVNR-ALPHA  END-IF                           
073000     IF W-IDLEVNR-ALPHA = 'AUJ7C'                                         
073100        MOVE '25846' TO W-IDLEVNR-ALPHA  END-IF                           
073200     IF W-IDLEVNR-ALPHA = 'DNK9A'                                         
073300        MOVE '25931' TO W-IDLEVNR-ALPHA  END-IF                           
073400*************************************************                         
073500**********************************************                            
073600     IF W-IDLEVNR-ALPHA = 'BQEFA'                                         
073700        MOVE '29   ' TO W-IDLEVNR-ALPHA  END-IF                           
073800     IF W-IDLEVNR-ALPHA = 'BQZ7A'                                         
073900        MOVE '280  ' TO W-IDLEVNR-ALPHA  END-IF                           
074000     IF W-IDLEVNR-ALPHA = 'BQZ9A'                                         
074100        MOVE '320  ' TO W-IDLEVNR-ALPHA  END-IF                           
074200     IF W-IDLEVNR-ALPHA = 'BQ0DA'                                         
074300        MOVE '372  ' TO W-IDLEVNR-ALPHA  END-IF                           
074400     IF W-IDLEVNR-ALPHA = 'BQ0EA'                                         
074500        MOVE '495  ' TO W-IDLEVNR-ALPHA  END-IF                           
074600     IF W-IDLEVNR-ALPHA = 'BQ6K1'                                         
074700        MOVE '579  ' TO W-IDLEVNR-ALPHA  END-IF                           
074800     IF W-IDLEVNR-ALPHA = 'CFNEA'                                         
074900        MOVE '710  ' TO W-IDLEVNR-ALPHA  END-IF                           
075000     IF W-IDLEVNR-ALPHA = 'BQ5LA'                                         
075100        MOVE '1912 ' TO W-IDLEVNR-ALPHA  END-IF                           
075200     IF W-IDLEVNR-ALPHA = 'CN4WA'                                         
075300        MOVE '1944 ' TO W-IDLEVNR-ALPHA  END-IF                           
075400     IF W-IDLEVNR-ALPHA = 'CW6BA'                                         
075500        MOVE '2020 ' TO W-IDLEVNR-ALPHA  END-IF                           
075600     IF W-IDLEVNR-ALPHA = 'C6T3A'                                         
075700        MOVE '2351 ' TO W-IDLEVNR-ALPHA  END-IF                           
075800     IF W-IDLEVNR-ALPHA = 'CFNSA'                                         
075900        MOVE '2410 ' TO W-IDLEVNR-ALPHA  END-IF                           
076000     IF W-IDLEVNR-ALPHA = 'P112B'                                         
076100        MOVE '3559 ' TO W-IDLEVNR-ALPHA  END-IF                           
076200     IF W-IDLEVNR-ALPHA = 'C75RA'                                         
076300        MOVE '3606 ' TO W-IDLEVNR-ALPHA  END-IF                           
076400     IF W-IDLEVNR-ALPHA = 'P112L'                                         
076500        MOVE '3705 ' TO W-IDLEVNR-ALPHA  END-IF                           
076600     IF W-IDLEVNR-ALPHA = 'R7NAB'                                         
076700        MOVE '3865 ' TO W-IDLEVNR-ALPHA  END-IF                           
076800     IF W-IDLEVNR-ALPHA = 'B2N4A'                                         
076900        MOVE '4964 ' TO W-IDLEVNR-ALPHA  END-IF                           
077000     IF W-IDLEVNR-ALPHA = 'C8Z7A'                                         
077100        MOVE '6605 ' TO W-IDLEVNR-ALPHA  END-IF                           
077200     IF W-IDLEVNR-ALPHA = 'D0U2A'                                         
077300        MOVE '6765 ' TO W-IDLEVNR-ALPHA  END-IF                           
077400     IF W-IDLEVNR-ALPHA = 'C97GA'                                         
077500        MOVE '6947 ' TO W-IDLEVNR-ALPHA  END-IF                           
077600     IF W-IDLEVNR-ALPHA = 'AZJLA'                                         
077700        MOVE '11148' TO W-IDLEVNR-ALPHA  END-IF                           
077800     IF W-IDLEVNR-ALPHA = 'K0R6F'                                         
077900        MOVE '11326' TO W-IDLEVNR-ALPHA  END-IF                           
078000     IF W-IDLEVNR-ALPHA = 'Q18RA'                                         
078100        MOVE '13382' TO W-IDLEVNR-ALPHA  END-IF                           
078200     IF W-IDLEVNR-ALPHA = 'P112D'                                         
078300        MOVE '13540' TO W-IDLEVNR-ALPHA  END-IF                           
078400     IF W-IDLEVNR-ALPHA = 'P112M'                                         
078500        MOVE '13541' TO W-IDLEVNR-ALPHA  END-IF                           
078600     IF W-IDLEVNR-ALPHA = 'T0CJA'                                         
078700        MOVE '13548' TO W-IDLEVNR-ALPHA  END-IF                           
078800     IF W-IDLEVNR-ALPHA = 'C75RB'                                         
078900        MOVE '13579' TO W-IDLEVNR-ALPHA  END-IF                           
079000     IF W-IDLEVNR-ALPHA = 'K0R6G'                                         
079100        MOVE '13587' TO W-IDLEVNR-ALPHA  END-IF                           
079200     IF W-IDLEVNR-ALPHA = 'BP8HB'                                         
079300        MOVE '14621' TO W-IDLEVNR-ALPHA  END-IF                           
079400     IF W-IDLEVNR-ALPHA = 'D0U2B'                                         
079500        MOVE '16172' TO W-IDLEVNR-ALPHA  END-IF                           
079600     IF W-IDLEVNR-ALPHA = 'D17KA'                                         
079700        MOVE '19255' TO W-IDLEVNR-ALPHA  END-IF                           
079800     IF W-IDLEVNR-ALPHA = 'R76JA'                                         
079900        MOVE '25936' TO W-IDLEVNR-ALPHA  END-IF                           
080000     IF W-IDLEVNR-ALPHA = 'D26QC'                                         
080100        MOVE '25937' TO W-IDLEVNR-ALPHA  END-IF                           
080200     IF W-IDLEVNR-ALPHA = 'BP8HA'                                         
080300        MOVE '34621' TO W-IDLEVNR-ALPHA  END-IF                           
080400*************************************************                         
080500**********************************************                            
080600     IF W-IDLEVNR-ALPHA = 'BJWQA'                                         
080700        MOVE '354  ' TO W-IDLEVNR-ALPHA  END-IF                           
080800     IF W-IDLEVNR-ALPHA = 'BQ3DA'                                         
080900        MOVE '1612 ' TO W-IDLEVNR-ALPHA  END-IF                           
081000     IF W-IDLEVNR-ALPHA = 'CFNLA'                                         
081100        MOVE '1755 ' TO W-IDLEVNR-ALPHA  END-IF                           
081200     IF W-IDLEVNR-ALPHA = 'C96UD'                                         
081300        MOVE '1800 ' TO W-IDLEVNR-ALPHA  END-IF                           
081400     IF W-IDLEVNR-ALPHA = 'BK9KA'                                         
081500        MOVE '2248 ' TO W-IDLEVNR-ALPHA  END-IF                           
081600     IF W-IDLEVNR-ALPHA = 'GPR5A'                                         
081700        MOVE '5425 ' TO W-IDLEVNR-ALPHA  END-IF                           
081800     IF W-IDLEVNR-ALPHA = 'Q0ERA'                                         
081900        MOVE '5489 ' TO W-IDLEVNR-ALPHA  END-IF                           
082000     IF W-IDLEVNR-ALPHA = 'AHHSA'                                         
082100        MOVE '5679 ' TO W-IDLEVNR-ALPHA  END-IF                           
082200     IF W-IDLEVNR-ALPHA = 'S2ZLA'                                         
082300        MOVE '6756 ' TO W-IDLEVNR-ALPHA  END-IF                           
082400     IF W-IDLEVNR-ALPHA = 'C7B1A'                                         
082500        MOVE '6775 ' TO W-IDLEVNR-ALPHA  END-IF                           
082600     IF W-IDLEVNR-ALPHA = 'D2S2A'                                         
082700        MOVE '6795 ' TO W-IDLEVNR-ALPHA  END-IF                           
082800     IF W-IDLEVNR-ALPHA = 'D23YA'                                         
082900        MOVE '6807 ' TO W-IDLEVNR-ALPHA  END-IF                           
083000     IF W-IDLEVNR-ALPHA = 'BQ7PA'                                         
083100        MOVE '7218 ' TO W-IDLEVNR-ALPHA  END-IF                           
083200     IF W-IDLEVNR-ALPHA = 'CDHSA'                                         
083300        MOVE '7757 ' TO W-IDLEVNR-ALPHA  END-IF                           
083400     IF W-IDLEVNR-ALPHA = 'D23YB'                                         
083500        MOVE '7922 ' TO W-IDLEVNR-ALPHA  END-IF                           
083600     IF W-IDLEVNR-ALPHA = 'P8C8A'                                         
083700        MOVE '14615' TO W-IDLEVNR-ALPHA  END-IF                           
083800     IF W-IDLEVNR-ALPHA = 'N2M5A'                                         
083900        MOVE '16088' TO W-IDLEVNR-ALPHA  END-IF                           
084000     IF W-IDLEVNR-ALPHA = 'DAFTA'                                         
084100        MOVE '16171' TO W-IDLEVNR-ALPHA  END-IF                           
084200     IF W-IDLEVNR-ALPHA = 'BQ9VA'                                         
084300        MOVE '16388' TO W-IDLEVNR-ALPHA  END-IF                           
084400     IF W-IDLEVNR-ALPHA = 'BHZ3A'                                         
084500        MOVE '18688' TO W-IDLEVNR-ALPHA  END-IF                           
084600     IF W-IDLEVNR-ALPHA = 'V4FVA'                                         
084700        MOVE '19454' TO W-IDLEVNR-ALPHA  END-IF                           
084800     IF W-IDLEVNR-ALPHA = 'S3JJA'                                         
084900        MOVE '19455' TO W-IDLEVNR-ALPHA  END-IF                           
085000     IF W-IDLEVNR-ALPHA = 'BQ7PB'                                         
085100        MOVE '24065' TO W-IDLEVNR-ALPHA  END-IF                           
085200     IF W-IDLEVNR-ALPHA = 'BQ7RB'                                         
085300        MOVE '24066' TO W-IDLEVNR-ALPHA  END-IF                           
085400     IF W-IDLEVNR-ALPHA = 'G13FC'                                         
085500        MOVE '24078' TO W-IDLEVNR-ALPHA  END-IF                           
085600     IF W-IDLEVNR-ALPHA = 'BQ7RC'                                         
085700        MOVE '25752' TO W-IDLEVNR-ALPHA  END-IF                           
085800*************************************************                         
085900     IF W-IDLEVNR-ALPHA = 'BMLWA'                                         
086000        MOVE '143  ' TO W-IDLEVNR-ALPHA  END-IF                           
086100     IF W-IDLEVNR-ALPHA = 'BQ1NA'                                         
086200        MOVE '934  ' TO W-IDLEVNR-ALPHA  END-IF                           
086300     IF W-IDLEVNR-ALPHA = 'BPUYA'                                         
086400        MOVE '1008 ' TO W-IDLEVNR-ALPHA  END-IF                           
086500     IF W-IDLEVNR-ALPHA = 'BKRZA'                                         
086600        MOVE '1062 ' TO W-IDLEVNR-ALPHA  END-IF                           
086700     IF W-IDLEVNR-ALPHA = 'S34XF'                                         
086800        MOVE '1134 ' TO W-IDLEVNR-ALPHA  END-IF                           
086900     IF W-IDLEVNR-ALPHA = 'BKDQA'                                         
087000        MOVE '1196 ' TO W-IDLEVNR-ALPHA  END-IF                           
087100     IF W-IDLEVNR-ALPHA = 'U7ABB'                                         
087200        MOVE '1244 ' TO W-IDLEVNR-ALPHA  END-IF                           
087300     IF W-IDLEVNR-ALPHA = 'U7ABC'                                         
087400        MOVE '1269 ' TO W-IDLEVNR-ALPHA  END-IF                           
087500     IF W-IDLEVNR-ALPHA = 'BSKYA'                                         
087600        MOVE '1449 ' TO W-IDLEVNR-ALPHA  END-IF                           
087700     IF W-IDLEVNR-ALPHA = 'BKMMA'                                         
087800        MOVE '1662 ' TO W-IDLEVNR-ALPHA  END-IF                           
087900     IF W-IDLEVNR-ALPHA = 'U7ABA'                                         
088000        MOVE '1847 ' TO W-IDLEVNR-ALPHA  END-IF                           
088100     IF W-IDLEVNR-ALPHA = 'S34XD'                                         
088200        MOVE '2500 ' TO W-IDLEVNR-ALPHA  END-IF                           
088300     IF W-IDLEVNR-ALPHA = 'BQAJA'                                         
088400        MOVE '2552 ' TO W-IDLEVNR-ALPHA  END-IF                           
088500     IF W-IDLEVNR-ALPHA = 'BWKGA'                                         
088600        MOVE '2553 ' TO W-IDLEVNR-ALPHA  END-IF                           
088700     IF W-IDLEVNR-ALPHA = 'BWTCA'                                         
088800        MOVE '2558 ' TO W-IDLEVNR-ALPHA  END-IF                           
088900     IF W-IDLEVNR-ALPHA = 'CFNVA'                                         
089000        MOVE '2633 ' TO W-IDLEVNR-ALPHA  END-IF                           
089100     IF W-IDLEVNR-ALPHA = 'BQAJB'                                         
089200        MOVE '2684 ' TO W-IDLEVNR-ALPHA  END-IF                           
089300     IF W-IDLEVNR-ALPHA = 'BWKSA'                                         
089400        MOVE '3054 ' TO W-IDLEVNR-ALPHA  END-IF                           
089500     IF W-IDLEVNR-ALPHA = 'CN4YA'                                         
089600        MOVE '3165 ' TO W-IDLEVNR-ALPHA  END-IF                           
089700     IF W-IDLEVNR-ALPHA = 'CFNWA'                                         
089800        MOVE '3197 ' TO W-IDLEVNR-ALPHA  END-IF                           
089900     IF W-IDLEVNR-ALPHA = 'D3C5A'                                         
090000        MOVE '3468 ' TO W-IDLEVNR-ALPHA  END-IF                           
090100     IF W-IDLEVNR-ALPHA = 'BP9ZA'                                         
090200        MOVE '3558 ' TO W-IDLEVNR-ALPHA  END-IF                           
090300     IF W-IDLEVNR-ALPHA = 'Q6TFA'                                         
090400        MOVE '3662 ' TO W-IDLEVNR-ALPHA  END-IF                           
090500     IF W-IDLEVNR-ALPHA = 'D5H4A'                                         
090600        MOVE '3807 ' TO W-IDLEVNR-ALPHA  END-IF                           
090700     IF W-IDLEVNR-ALPHA = 'BQ6VA'                                         
090800        MOVE '3982 ' TO W-IDLEVNR-ALPHA  END-IF                           
090900     IF W-IDLEVNR-ALPHA = 'AHMPA'                                         
091000        MOVE '4172 ' TO W-IDLEVNR-ALPHA  END-IF                           
091100     IF W-IDLEVNR-ALPHA = 'BPFNA'                                         
091200        MOVE '4721 ' TO W-IDLEVNR-ALPHA  END-IF                           
091300     IF W-IDLEVNR-ALPHA = 'ATNNA'                                         
091400        MOVE '4724 ' TO W-IDLEVNR-ALPHA  END-IF                           
091500     IF W-IDLEVNR-ALPHA = 'BCJSA'                                         
091600        MOVE '4988 ' TO W-IDLEVNR-ALPHA  END-IF                           
091700     IF W-IDLEVNR-ALPHA = 'C7L2A'                                         
091800        MOVE '5281 ' TO W-IDLEVNR-ALPHA  END-IF                           
091900     IF W-IDLEVNR-ALPHA = 'E3B2B'                                         
092000        MOVE '6305 ' TO W-IDLEVNR-ALPHA  END-IF                           
092100     IF W-IDLEVNR-ALPHA = 'C68JA'                                         
092200        MOVE '6350 ' TO W-IDLEVNR-ALPHA  END-IF                           
092300     IF W-IDLEVNR-ALPHA = 'D0MGA'                                         
092400        MOVE '6554 ' TO W-IDLEVNR-ALPHA  END-IF                           
092500     IF W-IDLEVNR-ALPHA = 'BPX3B'                                         
092600        MOVE '6555 ' TO W-IDLEVNR-ALPHA  END-IF                           
092700     IF W-IDLEVNR-ALPHA = 'C8T1A'                                         
092800        MOVE '6556 ' TO W-IDLEVNR-ALPHA  END-IF                           
092900     IF W-IDLEVNR-ALPHA = 'C68JC'                                         
093000        MOVE '6597 ' TO W-IDLEVNR-ALPHA  END-IF                           
093100     IF W-IDLEVNR-ALPHA = 'G944E'                                         
093200        MOVE '6911 ' TO W-IDLEVNR-ALPHA  END-IF                           
093300     IF W-IDLEVNR-ALPHA = 'CDNXA'                                         
093400        MOVE '6958 ' TO W-IDLEVNR-ALPHA  END-IF                           
093500     IF W-IDLEVNR-ALPHA = 'D38KA'                                         
093600        MOVE '6961 ' TO W-IDLEVNR-ALPHA  END-IF                           
093700     IF W-IDLEVNR-ALPHA = 'D38KD'                                         
093800        MOVE '6962 ' TO W-IDLEVNR-ALPHA  END-IF                           
093900     IF W-IDLEVNR-ALPHA = 'MTSFA'                                         
094000        MOVE '7462 ' TO W-IDLEVNR-ALPHA  END-IF                           
094100     IF W-IDLEVNR-ALPHA = 'BP9ZC'                                         
094200        MOVE '13672' TO W-IDLEVNR-ALPHA  END-IF                           
094300     IF W-IDLEVNR-ALPHA = 'BP9ZB'                                         
094400        MOVE '13709' TO W-IDLEVNR-ALPHA  END-IF                           
094500     IF W-IDLEVNR-ALPHA = 'D38KF'                                         
094600        MOVE '16044' TO W-IDLEVNR-ALPHA  END-IF                           
094700     IF W-IDLEVNR-ALPHA = 'R7GNA'                                         
094800        MOVE '16092' TO W-IDLEVNR-ALPHA  END-IF                           
094900     IF W-IDLEVNR-ALPHA = 'T2GCC'                                         
095000        MOVE '16141' TO W-IDLEVNR-ALPHA  END-IF                           
095100     IF W-IDLEVNR-ALPHA = 'D0W5D'                                         
095200        MOVE '16142' TO W-IDLEVNR-ALPHA  END-IF                           
095300     IF W-IDLEVNR-ALPHA = 'BPX3C'                                         
095400        MOVE '16365' TO W-IDLEVNR-ALPHA  END-IF                           
095500     IF W-IDLEVNR-ALPHA = 'T2GCA'                                         
095600        MOVE '16406' TO W-IDLEVNR-ALPHA  END-IF                           
095700     IF W-IDLEVNR-ALPHA = 'AD3XA'                                         
095800        MOVE '21256' TO W-IDLEVNR-ALPHA  END-IF                           
095900     IF W-IDLEVNR-ALPHA = 'D38KG'                                         
096000        MOVE '25618' TO W-IDLEVNR-ALPHA  END-IF                           
096100     IF W-IDLEVNR-ALPHA = 'BVPDA'                                         
096200        MOVE '25920' TO W-IDLEVNR-ALPHA  END-IF                           
096300     IF W-IDLEVNR-ALPHA = 'C68JF'                                         
096400        MOVE '26350' TO W-IDLEVNR-ALPHA  END-IF                           
096500*************************************************                         
096600     IF W-IDLEVNR-ALPHA = 'BP8JB'                                         
096700        MOVE '912  ' TO W-IDLEVNR-ALPHA  END-IF                           
096800     IF W-IDLEVNR-ALPHA = 'BMJGA'                                         
096900        MOVE '1005 ' TO W-IDLEVNR-ALPHA  END-IF                           
097000     IF W-IDLEVNR-ALPHA = 'AY0CA'                                         
097100        MOVE '1720 ' TO W-IDLEVNR-ALPHA  END-IF                           
097200     IF W-IDLEVNR-ALPHA = 'BPUNA'                                         
097300        MOVE '2442 ' TO W-IDLEVNR-ALPHA  END-IF                           
097400     IF W-IDLEVNR-ALPHA = 'BKTXA'                                         
097500        MOVE '3074 ' TO W-IDLEVNR-ALPHA  END-IF                           
097600     IF W-IDLEVNR-ALPHA = 'BPA1A'                                         
097700        MOVE '3163 ' TO W-IDLEVNR-ALPHA  END-IF                           
097800     IF W-IDLEVNR-ALPHA = 'C62FA'                                         
097900        MOVE '3575 ' TO W-IDLEVNR-ALPHA  END-IF                           
098000     IF W-IDLEVNR-ALPHA = 'J3CQA'                                         
098100        MOVE '4319 ' TO W-IDLEVNR-ALPHA  END-IF                           
098200     IF W-IDLEVNR-ALPHA = 'D026P'                                         
098300        MOVE '4382 ' TO W-IDLEVNR-ALPHA  END-IF                           
098400     IF W-IDLEVNR-ALPHA = 'MLMZA'                                         
098500        MOVE '4448 ' TO W-IDLEVNR-ALPHA  END-IF                           
098600     IF W-IDLEVNR-ALPHA = 'BQ6WA'                                         
098700        MOVE '4585 ' TO W-IDLEVNR-ALPHA  END-IF                           
098800     IF W-IDLEVNR-ALPHA = 'BQ6WB'                                         
098900        MOVE '4628 ' TO W-IDLEVNR-ALPHA  END-IF                           
099000     IF W-IDLEVNR-ALPHA = 'A426K'                                         
099100        MOVE '4894 ' TO W-IDLEVNR-ALPHA  END-IF                           
099200     IF W-IDLEVNR-ALPHA = 'C0VAG'                                         
099300        MOVE '4965 ' TO W-IDLEVNR-ALPHA  END-IF                           
099400     IF W-IDLEVNR-ALPHA = 'AVG9A'                                         
099500        MOVE '5065 ' TO W-IDLEVNR-ALPHA  END-IF                           
099600     IF W-IDLEVNR-ALPHA = 'D24DA'                                         
099700        MOVE '5647 ' TO W-IDLEVNR-ALPHA  END-IF                           
099800     IF W-IDLEVNR-ALPHA = 'D0RYA'                                         
099900        MOVE '6030 ' TO W-IDLEVNR-ALPHA  END-IF                           
100000     IF W-IDLEVNR-ALPHA = 'BT7WA'                                         
100100        MOVE '6279 ' TO W-IDLEVNR-ALPHA  END-IF                           
100200     IF W-IDLEVNR-ALPHA = 'A426G'                                         
100300        MOVE '6840 ' TO W-IDLEVNR-ALPHA  END-IF                           
100400     IF W-IDLEVNR-ALPHA = 'F488A'                                         
100500        MOVE '6992 ' TO W-IDLEVNR-ALPHA  END-IF                           
100600     IF W-IDLEVNR-ALPHA = 'BP3HA'                                         
100700        MOVE '7349 ' TO W-IDLEVNR-ALPHA  END-IF                           
100800     IF W-IDLEVNR-ALPHA = 'CL3VA'                                         
100900        MOVE '10453' TO W-IDLEVNR-ALPHA  END-IF                           
101000     IF W-IDLEVNR-ALPHA = 'A426S'                                         
101100        MOVE '10814' TO W-IDLEVNR-ALPHA  END-IF                           
101200     IF W-IDLEVNR-ALPHA = 'CFT4B'                                         
101300        MOVE '12543' TO W-IDLEVNR-ALPHA  END-IF                           
101400     IF W-IDLEVNR-ALPHA = 'M09EA'                                         
101500        MOVE '14616' TO W-IDLEVNR-ALPHA  END-IF                           
101600     IF W-IDLEVNR-ALPHA = 'BQ6WC'                                         
101700        MOVE '14643' TO W-IDLEVNR-ALPHA  END-IF                           
101800     IF W-IDLEVNR-ALPHA = 'A426R'                                         
101900        MOVE '14647' TO W-IDLEVNR-ALPHA  END-IF                           
102000     IF W-IDLEVNR-ALPHA = 'N718D'                                         
102100        MOVE '14963' TO W-IDLEVNR-ALPHA  END-IF                           
102200     IF W-IDLEVNR-ALPHA = 'N718B'                                         
102300        MOVE '14988' TO W-IDLEVNR-ALPHA  END-IF                           
102400     IF W-IDLEVNR-ALPHA = 'B492C'                                         
102500        MOVE '16134' TO W-IDLEVNR-ALPHA  END-IF                           
102600     IF W-IDLEVNR-ALPHA = 'F488X'                                         
102700        MOVE '16137' TO W-IDLEVNR-ALPHA  END-IF                           
102800     IF W-IDLEVNR-ALPHA = 'AYZ4A'                                         
102900        MOVE '16210' TO W-IDLEVNR-ALPHA  END-IF                           
103000     IF W-IDLEVNR-ALPHA = 'C685Y'                                         
103100        MOVE '16211' TO W-IDLEVNR-ALPHA  END-IF                           
103200     IF W-IDLEVNR-ALPHA = 'C685C'                                         
103300        MOVE '16222' TO W-IDLEVNR-ALPHA  END-IF                           
103400     IF W-IDLEVNR-ALPHA = 'A426C'                                         
103500        MOVE '16237' TO W-IDLEVNR-ALPHA  END-IF                           
103600     IF W-IDLEVNR-ALPHA = 'A426M'                                         
103700        MOVE '16279' TO W-IDLEVNR-ALPHA  END-IF                           
103800     IF W-IDLEVNR-ALPHA = 'F488Z'                                         
103900        MOVE '16283' TO W-IDLEVNR-ALPHA  END-IF                           
104000     IF W-IDLEVNR-ALPHA = 'BA8YA'                                         
104100        MOVE '20094' TO W-IDLEVNR-ALPHA  END-IF                           
104200     IF W-IDLEVNR-ALPHA = 'AUE4A'                                         
104300        MOVE '21873' TO W-IDLEVNR-ALPHA  END-IF                           
104400     IF W-IDLEVNR-ALPHA = 'M09EB'                                         
104500        MOVE '25851' TO W-IDLEVNR-ALPHA  END-IF                           
104600     IF W-IDLEVNR-ALPHA = 'BARJA'                                         
104700        MOVE '25907' TO W-IDLEVNR-ALPHA  END-IF                           
104800     IF W-IDLEVNR-ALPHA = 'BARJB'                                         
104900        MOVE '25908' TO W-IDLEVNR-ALPHA  END-IF                           
105000     IF W-IDLEVNR-ALPHA = 'CUTBA'                                         
105100        MOVE '25909' TO W-IDLEVNR-ALPHA  END-IF                           
105200     IF W-IDLEVNR-ALPHA = 'A426U'                                         
105300        MOVE '26840' TO W-IDLEVNR-ALPHA  END-IF                           
105400     IF W-IDLEVNR-ALPHA = 'BQ7RB'                                         
105500        MOVE '24066' TO W-IDLEVNR-ALPHA  END-IF                           
105600     IF W-IDLEVNR-ALPHA = 'BQ7RC'                                         
105700        MOVE '25752' TO W-IDLEVNR-ALPHA  END-IF                           
105800*************************************************                         
105900     IF W-IDLEVNR-ALPHA = 'BMP1A'                                         
106000        MOVE '856  ' TO W-IDLEVNR-ALPHA  END-IF                           
106100     IF W-IDLEVNR-ALPHA = 'D07FA'                                         
106200        MOVE '3330 ' TO W-IDLEVNR-ALPHA  END-IF                           
106300     IF W-IDLEVNR-ALPHA = 'T2VGA'                                         
106400        MOVE '5453 ' TO W-IDLEVNR-ALPHA  END-IF                           
106500     IF W-IDLEVNR-ALPHA = 'D07FD'                                         
106600        MOVE '6013 ' TO W-IDLEVNR-ALPHA  END-IF                           
106700     IF W-IDLEVNR-ALPHA = 'E510D'                                         
106800        MOVE '6160 ' TO W-IDLEVNR-ALPHA  END-IF                           
106900     IF W-IDLEVNR-ALPHA = 'D07FF'                                         
107000        MOVE '6193 ' TO W-IDLEVNR-ALPHA  END-IF                           
107100     IF W-IDLEVNR-ALPHA = 'BQ7MA'                                         
107200        MOVE '6285 ' TO W-IDLEVNR-ALPHA  END-IF                           
107300     IF W-IDLEVNR-ALPHA = 'CGSWA'                                         
107400        MOVE '7622 ' TO W-IDLEVNR-ALPHA  END-IF                           
107500     IF W-IDLEVNR-ALPHA = 'BZMDA'                                         
107600        MOVE '10965' TO W-IDLEVNR-ALPHA  END-IF                           
107700     IF W-IDLEVNR-ALPHA = 'BEF1A'                                         
107800        MOVE '20303' TO W-IDLEVNR-ALPHA  END-IF                           
107900*************************************************                         
108000     IF W-IDLEVNR-ALPHA = 'BWTAA'                                         
108100        MOVE '520  ' TO W-IDLEVNR-ALPHA  END-IF                           
108200     IF W-IDLEVNR-ALPHA = 'BQ00A'                                         
108300        MOVE '546  ' TO W-IDLEVNR-ALPHA  END-IF                           
108400     IF W-IDLEVNR-ALPHA = 'AHFGA'                                         
108500        MOVE '547  ' TO W-IDLEVNR-ALPHA  END-IF                           
108600     IF W-IDLEVNR-ALPHA = 'BQ1BA'                                         
108700        MOVE '550  ' TO W-IDLEVNR-ALPHA  END-IF                           
108800     IF W-IDLEVNR-ALPHA = 'BQ3YA'                                         
108900        MOVE '1883 ' TO W-IDLEVNR-ALPHA  END-IF                           
109000     IF W-IDLEVNR-ALPHA = 'S3HXA'                                         
109100        MOVE '4610 ' TO W-IDLEVNR-ALPHA  END-IF                           
109200     IF W-IDLEVNR-ALPHA = 'AKTAD'                                         
109300        MOVE '6110 ' TO W-IDLEVNR-ALPHA  END-IF                           
109400     IF W-IDLEVNR-ALPHA = 'BQAWA'                                         
109500        MOVE '10087' TO W-IDLEVNR-ALPHA  END-IF                           
109600     IF W-IDLEVNR-ALPHA = 'BAM8D'                                         
109700        MOVE '11328' TO W-IDLEVNR-ALPHA  END-IF                           
109800     IF W-IDLEVNR-ALPHA = 'BAM8A'                                         
109900        MOVE '19956' TO W-IDLEVNR-ALPHA  END-IF                           
110000     IF W-IDLEVNR-ALPHA = 'CEPHA'                                         
110100        MOVE '19986' TO W-IDLEVNR-ALPHA  END-IF                           
110200*************************************************                         
110300     IF W-IDLEVNR-ALPHA = 'BQAHA'                                         
110400        MOVE '114  ' TO W-IDLEVNR-ALPHA  END-IF                           
110500     IF W-IDLEVNR-ALPHA = 'S3SQA'                                         
110600        MOVE '642  ' TO W-IDLEVNR-ALPHA  END-IF                           
110700     IF W-IDLEVNR-ALPHA = 'S3SQB'                                         
110800        MOVE '1297 ' TO W-IDLEVNR-ALPHA  END-IF                           
110900     IF W-IDLEVNR-ALPHA = 'BQAHD'                                         
111000        MOVE '2639 ' TO W-IDLEVNR-ALPHA  END-IF                           
111100     IF W-IDLEVNR-ALPHA = 'BT7RA'                                         
111200        MOVE '2647 ' TO W-IDLEVNR-ALPHA  END-IF                           
111300     IF W-IDLEVNR-ALPHA = 'BEFYA'                                         
111400        MOVE '3671 ' TO W-IDLEVNR-ALPHA  END-IF                           
111500     IF W-IDLEVNR-ALPHA = 'K0R6A'                                         
111600        MOVE '3787 ' TO W-IDLEVNR-ALPHA  END-IF                           
111700     IF W-IDLEVNR-ALPHA = 'C84QA'                                         
111800        MOVE '3883 ' TO W-IDLEVNR-ALPHA  END-IF                           
111900     IF W-IDLEVNR-ALPHA = 'P38NA'                                         
112000        MOVE '3988 ' TO W-IDLEVNR-ALPHA  END-IF                           
112100     IF W-IDLEVNR-ALPHA = 'MMV4A'                                         
112200        MOVE '4737 ' TO W-IDLEVNR-ALPHA  END-IF                           
112300     IF W-IDLEVNR-ALPHA = 'F842A'                                         
112400        MOVE '4961 ' TO W-IDLEVNR-ALPHA  END-IF                           
112500     IF W-IDLEVNR-ALPHA = 'E1SLB'                                         
112600        MOVE '5160 ' TO W-IDLEVNR-ALPHA  END-IF                           
112700     IF W-IDLEVNR-ALPHA = 'D2R5A'                                         
112800        MOVE '5162 ' TO W-IDLEVNR-ALPHA  END-IF                           
112900     IF W-IDLEVNR-ALPHA = 'C93SA'                                         
113000        MOVE '5171 ' TO W-IDLEVNR-ALPHA  END-IF                           
113100     IF W-IDLEVNR-ALPHA = 'Q749A'                                         
113200        MOVE '5240 ' TO W-IDLEVNR-ALPHA  END-IF                           
113300     IF W-IDLEVNR-ALPHA = 'H268X'                                         
113400        MOVE '5383 ' TO W-IDLEVNR-ALPHA  END-IF                           
113500     IF W-IDLEVNR-ALPHA = 'AXVNA'                                         
113600        MOVE '5662 ' TO W-IDLEVNR-ALPHA  END-IF                           
113700     IF W-IDLEVNR-ALPHA = 'D16AA'                                         
113800        MOVE '6083 ' TO W-IDLEVNR-ALPHA  END-IF                           
113900     IF W-IDLEVNR-ALPHA = 'E521A'                                         
114000        MOVE '6090 ' TO W-IDLEVNR-ALPHA  END-IF                           
114100     IF W-IDLEVNR-ALPHA = 'D04HA'                                         
114200        MOVE '6096 ' TO W-IDLEVNR-ALPHA  END-IF                           
114300     IF W-IDLEVNR-ALPHA = 'EGX2A'                                         
114400        MOVE '6146 ' TO W-IDLEVNR-ALPHA  END-IF                           
114500     IF W-IDLEVNR-ALPHA = 'C6S4A'                                         
114600        MOVE '6200 ' TO W-IDLEVNR-ALPHA  END-IF                           
114700     IF W-IDLEVNR-ALPHA = 'C9U3A'                                         
114800        MOVE '6226 ' TO W-IDLEVNR-ALPHA  END-IF                           
114900     IF W-IDLEVNR-ALPHA = 'D1C6A'                                         
115000        MOVE '6310 ' TO W-IDLEVNR-ALPHA  END-IF                           
115100     IF W-IDLEVNR-ALPHA = 'F6W7A'                                         
115200        MOVE '6321 ' TO W-IDLEVNR-ALPHA  END-IF                           
115300     IF W-IDLEVNR-ALPHA = 'CFTXA'                                         
115400        MOVE '6328 ' TO W-IDLEVNR-ALPHA  END-IF                           
115500     IF W-IDLEVNR-ALPHA = 'AECTA'                                         
115600        MOVE '6360 ' TO W-IDLEVNR-ALPHA  END-IF                           
115700     IF W-IDLEVNR-ALPHA = 'BPU8A'                                         
115800        MOVE '6421 ' TO W-IDLEVNR-ALPHA  END-IF                           
115900     IF W-IDLEVNR-ALPHA = 'X345A'                                         
116000        MOVE '6443 ' TO W-IDLEVNR-ALPHA  END-IF                           
116100     IF W-IDLEVNR-ALPHA = 'D16MA'                                         
116200        MOVE '6468 ' TO W-IDLEVNR-ALPHA  END-IF                           
116300     IF W-IDLEVNR-ALPHA = 'BB4SA'                                         
116400        MOVE '6488 ' TO W-IDLEVNR-ALPHA  END-IF                           
116500     IF W-IDLEVNR-ALPHA = 'BZFFA'                                         
116600        MOVE '6492 ' TO W-IDLEVNR-ALPHA  END-IF                           
116700     IF W-IDLEVNR-ALPHA = 'C9T9A'                                         
116800        MOVE '6764 ' TO W-IDLEVNR-ALPHA  END-IF                           
116900     IF W-IDLEVNR-ALPHA = 'D3L4A'                                         
117000        MOVE '6903 ' TO W-IDLEVNR-ALPHA  END-IF                           
117100     IF W-IDLEVNR-ALPHA = 'DNR5A'                                         
117200        MOVE '6968 ' TO W-IDLEVNR-ALPHA  END-IF                           
117300     IF W-IDLEVNR-ALPHA = 'D0DMA'                                         
117400        MOVE '8040 ' TO W-IDLEVNR-ALPHA  END-IF                           
117500     IF W-IDLEVNR-ALPHA = 'BPXEB'                                         
117600        MOVE '10103' TO W-IDLEVNR-ALPHA  END-IF                           
117700     IF W-IDLEVNR-ALPHA = 'R2FLG'                                         
117800        MOVE '11096' TO W-IDLEVNR-ALPHA  END-IF                           
117900     IF W-IDLEVNR-ALPHA = 'F432J'                                         
118000        MOVE '13594' TO W-IDLEVNR-ALPHA  END-IF                           
118100     IF W-IDLEVNR-ALPHA = 'F432B'                                         
118200        MOVE '13690' TO W-IDLEVNR-ALPHA  END-IF                           
118300     IF W-IDLEVNR-ALPHA = 'K0R6E'                                         
118400        MOVE '13701' TO W-IDLEVNR-ALPHA  END-IF                           
118500     IF W-IDLEVNR-ALPHA = 'E2L9A'                                         
118600        MOVE '16051' TO W-IDLEVNR-ALPHA  END-IF                           
118700     IF W-IDLEVNR-ALPHA = 'C6S4B'                                         
118800        MOVE '16115' TO W-IDLEVNR-ALPHA  END-IF                           
118900     IF W-IDLEVNR-ALPHA = 'H681K'                                         
119000        MOVE '16383' TO W-IDLEVNR-ALPHA  END-IF                           
119100     IF W-IDLEVNR-ALPHA = 'D0V6C'                                         
119200        MOVE '25047' TO W-IDLEVNR-ALPHA  END-IF                           
119300     IF W-IDLEVNR-ALPHA = 'E521J'                                         
119400        MOVE '25403' TO W-IDLEVNR-ALPHA  END-IF                           
119500     IF W-IDLEVNR-ALPHA = 'E019A'                                         
119600        MOVE '50535' TO W-IDLEVNR-ALPHA  END-IF                           
119700     IF W-IDLEVNR-ALPHA = 'CL3XA'                                         
119800        MOVE '63567' TO W-IDLEVNR-ALPHA  END-IF                           
119900*************************************************                         
120000     IF W-IDLEVNR-ALPHA = 'BSK5A'                                         
120100        MOVE '84   ' TO W-IDLEVNR-ALPHA  END-IF                           
120200     IF W-IDLEVNR-ALPHA = 'DJAYA'                                         
120300        MOVE '157  ' TO W-IDLEVNR-ALPHA  END-IF                           
120400     IF W-IDLEVNR-ALPHA = 'BV9NA'                                         
120500        MOVE '173  ' TO W-IDLEVNR-ALPHA  END-IF                           
120600     IF W-IDLEVNR-ALPHA = 'BU4GA'                                         
120700        MOVE '233  ' TO W-IDLEVNR-ALPHA  END-IF                           
120800     IF W-IDLEVNR-ALPHA = 'DJA8A'                                         
120900        MOVE '235  ' TO W-IDLEVNR-ALPHA  END-IF                           
121000     IF W-IDLEVNR-ALPHA = 'DL0KA'                                         
121100        MOVE '282  ' TO W-IDLEVNR-ALPHA  END-IF                           
121200     IF W-IDLEVNR-ALPHA = 'BXPRA'                                         
121300        MOVE '294  ' TO W-IDLEVNR-ALPHA  END-IF                           
121400     IF W-IDLEVNR-ALPHA = 'DJBBA'                                         
121500        MOVE '357  ' TO W-IDLEVNR-ALPHA  END-IF                           
121600     IF W-IDLEVNR-ALPHA = 'DL0LA'                                         
121700        MOVE '370  ' TO W-IDLEVNR-ALPHA  END-IF                           
121800     IF W-IDLEVNR-ALPHA = 'DJBCA'                                         
121900        MOVE '376  ' TO W-IDLEVNR-ALPHA  END-IF                           
122000     IF W-IDLEVNR-ALPHA = 'DL0MA'                                         
122100        MOVE '428  ' TO W-IDLEVNR-ALPHA  END-IF                           
122200     IF W-IDLEVNR-ALPHA = 'BRQFA'                                         
122300        MOVE '449  ' TO W-IDLEVNR-ALPHA  END-IF                           
122400     IF W-IDLEVNR-ALPHA = 'BSK2A'                                         
122500        MOVE '466  ' TO W-IDLEVNR-ALPHA  END-IF                           
122600     IF W-IDLEVNR-ALPHA = 'S6RPA'                                         
122700        MOVE '498  ' TO W-IDLEVNR-ALPHA  END-IF                           
122800     IF W-IDLEVNR-ALPHA = 'BLMNA'                                         
122900        MOVE '505  ' TO W-IDLEVNR-ALPHA  END-IF                           
123000     IF W-IDLEVNR-ALPHA = 'BJ9QA'                                         
123100        MOVE '523  ' TO W-IDLEVNR-ALPHA  END-IF                           
123200     IF W-IDLEVNR-ALPHA = 'DJBDA'                                         
123300        MOVE '524  ' TO W-IDLEVNR-ALPHA  END-IF                           
123400     IF W-IDLEVNR-ALPHA = 'CVESB'                                         
123500        MOVE '555  ' TO W-IDLEVNR-ALPHA  END-IF                           
123600     IF W-IDLEVNR-ALPHA = 'CFH3A'                                         
123700        MOVE '652  ' TO W-IDLEVNR-ALPHA  END-IF                           
123800     IF W-IDLEVNR-ALPHA = 'DJBEA'                                         
123900        MOVE '670  ' TO W-IDLEVNR-ALPHA  END-IF                           
124000     IF W-IDLEVNR-ALPHA = 'BMKTA'                                         
124100        MOVE '671  ' TO W-IDLEVNR-ALPHA  END-IF                           
124200     IF W-IDLEVNR-ALPHA = 'DJBFA'                                         
124300        MOVE '719  ' TO W-IDLEVNR-ALPHA  END-IF                           
124400     IF W-IDLEVNR-ALPHA = 'CDT1A'                                         
124500        MOVE '753  ' TO W-IDLEVNR-ALPHA  END-IF                           
124600     IF W-IDLEVNR-ALPHA = 'DJBGA'                                         
124700        MOVE '794  ' TO W-IDLEVNR-ALPHA  END-IF                           
124800     IF W-IDLEVNR-ALPHA = 'DJKLA'                                         
124900        MOVE '804  ' TO W-IDLEVNR-ALPHA  END-IF                           
125000     IF W-IDLEVNR-ALPHA = 'BN6ZA'                                         
125100        MOVE '944  ' TO W-IDLEVNR-ALPHA  END-IF                           
125200     IF W-IDLEVNR-ALPHA = 'DL0PA'                                         
125300        MOVE '948  ' TO W-IDLEVNR-ALPHA  END-IF                           
125400     IF W-IDLEVNR-ALPHA = 'BSKZA'                                         
125500        MOVE '971  ' TO W-IDLEVNR-ALPHA  END-IF                           
125600     IF W-IDLEVNR-ALPHA = 'DL0TA'                                         
125700        MOVE '1025 ' TO W-IDLEVNR-ALPHA  END-IF                           
125800     IF W-IDLEVNR-ALPHA = 'BZCEA'                                         
125900        MOVE '1068 ' TO W-IDLEVNR-ALPHA  END-IF                           
126000     IF W-IDLEVNR-ALPHA = 'DJKMA'                                         
126100        MOVE '1102 ' TO W-IDLEVNR-ALPHA  END-IF                           
126200     IF W-IDLEVNR-ALPHA = 'DL0UA'                                         
126300        MOVE '1160 ' TO W-IDLEVNR-ALPHA  END-IF                           
126400     IF W-IDLEVNR-ALPHA = 'BKMEA'                                         
126500        MOVE '1232 ' TO W-IDLEVNR-ALPHA  END-IF                           
126600     IF W-IDLEVNR-ALPHA = 'BJTTA'                                         
126700        MOVE '1293 ' TO W-IDLEVNR-ALPHA  END-IF                           
126800     IF W-IDLEVNR-ALPHA = 'BKMJA'                                         
126900        MOVE '1304 ' TO W-IDLEVNR-ALPHA  END-IF                           
127000     IF W-IDLEVNR-ALPHA = 'BMN7B'                                         
127100        MOVE '1331 ' TO W-IDLEVNR-ALPHA  END-IF                           
127200     IF W-IDLEVNR-ALPHA = 'BX4TA'                                         
127300        MOVE '1512 ' TO W-IDLEVNR-ALPHA  END-IF                           
127400     IF W-IDLEVNR-ALPHA = 'DJKNA'                                         
127500        MOVE '1523 ' TO W-IDLEVNR-ALPHA  END-IF                           
127600     IF W-IDLEVNR-ALPHA = 'DJKPA'                                         
127700        MOVE '1542 ' TO W-IDLEVNR-ALPHA  END-IF                           
127800     IF W-IDLEVNR-ALPHA = 'DJKQA'                                         
127900        MOVE '1606 ' TO W-IDLEVNR-ALPHA  END-IF                           
128000     IF W-IDLEVNR-ALPHA = 'BKEBA'                                         
128100        MOVE '1633 ' TO W-IDLEVNR-ALPHA  END-IF                           
128200     IF W-IDLEVNR-ALPHA = 'BJZQA'                                         
128300        MOVE '1699 ' TO W-IDLEVNR-ALPHA  END-IF                           
128400     IF W-IDLEVNR-ALPHA = 'DJKRA'                                         
128500        MOVE '1756 ' TO W-IDLEVNR-ALPHA  END-IF                           
128600     IF W-IDLEVNR-ALPHA = 'DJKSA'                                         
128700        MOVE '1764 ' TO W-IDLEVNR-ALPHA  END-IF                           
128800     IF W-IDLEVNR-ALPHA = 'BJ9ZA'                                         
128900        MOVE '1887 ' TO W-IDLEVNR-ALPHA  END-IF                           
129000     IF W-IDLEVNR-ALPHA = 'BKMUA'                                         
129100        MOVE '1916 ' TO W-IDLEVNR-ALPHA  END-IF                           
129200     IF W-IDLEVNR-ALPHA = 'DL0XA'                                         
129300        MOVE '2028 ' TO W-IDLEVNR-ALPHA  END-IF                           
129400     IF W-IDLEVNR-ALPHA = 'BLWMA'                                         
129500        MOVE '2055 ' TO W-IDLEVNR-ALPHA  END-IF                           
129600     IF W-IDLEVNR-ALPHA = 'BJQTA'                                         
129700        MOVE '2177 ' TO W-IDLEVNR-ALPHA  END-IF                           
129800     IF W-IDLEVNR-ALPHA = 'BJPDA'                                         
129900        MOVE '2229 ' TO W-IDLEVNR-ALPHA  END-IF                           
130000     IF W-IDLEVNR-ALPHA = 'CZ4BA'                                         
130100        MOVE '2243 ' TO W-IDLEVNR-ALPHA  END-IF                           
130200     IF W-IDLEVNR-ALPHA = 'DJKTA'                                         
130300        MOVE '2284 ' TO W-IDLEVNR-ALPHA  END-IF                           
130400     IF W-IDLEVNR-ALPHA = 'BK4LA'                                         
130500        MOVE '2322 ' TO W-IDLEVNR-ALPHA  END-IF                           
130600     IF W-IDLEVNR-ALPHA = 'DL0ZA'                                         
130700        MOVE '2344 ' TO W-IDLEVNR-ALPHA  END-IF                           
130800     IF W-IDLEVNR-ALPHA = 'DJKVA'                                         
130900        MOVE '2446 ' TO W-IDLEVNR-ALPHA  END-IF                           
131000     IF W-IDLEVNR-ALPHA = 'BJKNA'                                         
131100        MOVE '2451 ' TO W-IDLEVNR-ALPHA  END-IF                           
131200     IF W-IDLEVNR-ALPHA = 'CD2HA'                                         
131300        MOVE '2480 ' TO W-IDLEVNR-ALPHA  END-IF                           
131400     IF W-IDLEVNR-ALPHA = 'BJYVA'                                         
131500        MOVE '2619 ' TO W-IDLEVNR-ALPHA  END-IF                           
131600     IF W-IDLEVNR-ALPHA = 'BL3AA'                                         
131700        MOVE '2669 ' TO W-IDLEVNR-ALPHA  END-IF                           
131800     IF W-IDLEVNR-ALPHA = 'BLNLA'                                         
131900        MOVE '3105 ' TO W-IDLEVNR-ALPHA  END-IF                           
132000     IF W-IDLEVNR-ALPHA = 'DJKWA'                                         
132100        MOVE '3304 ' TO W-IDLEVNR-ALPHA  END-IF                           
132200     IF W-IDLEVNR-ALPHA = 'DL1RA'                                         
132300        MOVE '3310 ' TO W-IDLEVNR-ALPHA  END-IF                           
132400     IF W-IDLEVNR-ALPHA = 'DL1SA'                                         
132500        MOVE '3332 ' TO W-IDLEVNR-ALPHA  END-IF                           
132600     IF W-IDLEVNR-ALPHA = 'DEV4A'                                         
132700        MOVE '3342 ' TO W-IDLEVNR-ALPHA  END-IF                           
132800     IF W-IDLEVNR-ALPHA = 'DJKXA'                                         
132900        MOVE '3343 ' TO W-IDLEVNR-ALPHA  END-IF                           
133000     IF W-IDLEVNR-ALPHA = 'DJKYA'                                         
133100        MOVE '3345 ' TO W-IDLEVNR-ALPHA  END-IF                           
133200     IF W-IDLEVNR-ALPHA = 'DL1TA'                                         
133300        MOVE '3349 ' TO W-IDLEVNR-ALPHA  END-IF                           
133400     IF W-IDLEVNR-ALPHA = 'DL1UA'                                         
133500        MOVE '3354 ' TO W-IDLEVNR-ALPHA  END-IF                           
133600     IF W-IDLEVNR-ALPHA = 'DJKZA'                                         
133700        MOVE '3369 ' TO W-IDLEVNR-ALPHA  END-IF                           
133800     IF W-IDLEVNR-ALPHA = 'DJK0A'                                         
133900        MOVE '3375 ' TO W-IDLEVNR-ALPHA  END-IF                           
134000     IF W-IDLEVNR-ALPHA = 'DL1VA'                                         
134100        MOVE '3376 ' TO W-IDLEVNR-ALPHA  END-IF                           
134200     IF W-IDLEVNR-ALPHA = 'BQ6SA'                                         
134300        MOVE '3389 ' TO W-IDLEVNR-ALPHA  END-IF                           
134400     IF W-IDLEVNR-ALPHA = 'DJK9A'                                         
134500        MOVE '3401 ' TO W-IDLEVNR-ALPHA  END-IF                           
134600     IF W-IDLEVNR-ALPHA = 'CFJCA'                                         
134700        MOVE '3405 ' TO W-IDLEVNR-ALPHA  END-IF                           
134800     IF W-IDLEVNR-ALPHA = 'BK1EA'                                         
134900        MOVE '3421 ' TO W-IDLEVNR-ALPHA  END-IF                           
135000     IF W-IDLEVNR-ALPHA = 'DJLAA'                                         
135100        MOVE '3423 ' TO W-IDLEVNR-ALPHA  END-IF                           
135200     IF W-IDLEVNR-ALPHA = 'DL1XA'                                         
135300        MOVE '3424 ' TO W-IDLEVNR-ALPHA  END-IF                           
135400*************************************************                         
135500     IF W-IDLEVNR-ALPHA = 'BP8CA'                                         
135600        MOVE '1335 ' TO W-IDLEVNR-ALPHA  END-IF                           
135700     IF W-IDLEVNR-ALPHA = 'BWKFA'                                         
135800        MOVE '1649 ' TO W-IDLEVNR-ALPHA  END-IF                           
135900     IF W-IDLEVNR-ALPHA = 'G13FA'                                         
136000        MOVE '1797 ' TO W-IDLEVNR-ALPHA  END-IF                           
136100     IF W-IDLEVNR-ALPHA = 'BLMPA'                                         
136200        MOVE '2113 ' TO W-IDLEVNR-ALPHA  END-IF                           
136300     IF W-IDLEVNR-ALPHA = 'BQ7HA'                                         
136400        MOVE '2302 ' TO W-IDLEVNR-ALPHA  END-IF                           
136500     IF W-IDLEVNR-ALPHA = 'M279D'                                         
136600        MOVE '3556 ' TO W-IDLEVNR-ALPHA  END-IF                           
136700     IF W-IDLEVNR-ALPHA = 'AB7ZA'                                         
136800        MOVE '4760 ' TO W-IDLEVNR-ALPHA  END-IF                           
136900     IF W-IDLEVNR-ALPHA = 'D0UQA'                                         
137000        MOVE '6191 ' TO W-IDLEVNR-ALPHA  END-IF                           
137100     IF W-IDLEVNR-ALPHA = 'P511A'                                         
137200        MOVE '6354 ' TO W-IDLEVNR-ALPHA  END-IF                           
137300     IF W-IDLEVNR-ALPHA = 'ADBTA'                                         
137400        MOVE '6505 ' TO W-IDLEVNR-ALPHA  END-IF                           
137500     IF W-IDLEVNR-ALPHA = 'B42KA'                                         
137600        MOVE '6512 ' TO W-IDLEVNR-ALPHA  END-IF                           
137700     IF W-IDLEVNR-ALPHA = 'K1FWA'                                         
137800        MOVE '6589 ' TO W-IDLEVNR-ALPHA  END-IF                           
137900     IF W-IDLEVNR-ALPHA = 'M1F7A'                                         
138000        MOVE '6608 ' TO W-IDLEVNR-ALPHA  END-IF                           
138100     IF W-IDLEVNR-ALPHA = 'CX9XA'                                         
138200        MOVE '6650 ' TO W-IDLEVNR-ALPHA  END-IF                           
138300     IF W-IDLEVNR-ALPHA = 'P790A'                                         
138400        MOVE '6665 ' TO W-IDLEVNR-ALPHA  END-IF                           
138500     IF W-IDLEVNR-ALPHA = 'S106A'                                         
138600        MOVE '6669 ' TO W-IDLEVNR-ALPHA  END-IF                           
138700     IF W-IDLEVNR-ALPHA = 'J613A'                                         
138800        MOVE '6684 ' TO W-IDLEVNR-ALPHA  END-IF                           
138900     IF W-IDLEVNR-ALPHA = 'BZ02A'                                         
139000        MOVE '6692 ' TO W-IDLEVNR-ALPHA  END-IF                           
139100     IF W-IDLEVNR-ALPHA = 'B4W7A'                                         
139200        MOVE '6823 ' TO W-IDLEVNR-ALPHA  END-IF                           
139300     IF W-IDLEVNR-ALPHA = 'D0QWA'                                         
139400        MOVE '7277 ' TO W-IDLEVNR-ALPHA  END-IF                           
139500     IF W-IDLEVNR-ALPHA = 'BCSFA'                                         
139600        MOVE '7317 ' TO W-IDLEVNR-ALPHA  END-IF                           
139700     IF W-IDLEVNR-ALPHA = 'BPTMB'                                         
139800        MOVE '7900 ' TO W-IDLEVNR-ALPHA  END-IF                           
139900     IF W-IDLEVNR-ALPHA = 'BPTMD'                                         
140000        MOVE '10159' TO W-IDLEVNR-ALPHA  END-IF                           
140100     IF W-IDLEVNR-ALPHA = 'BPTMA'                                         
140200        MOVE '10160' TO W-IDLEVNR-ALPHA  END-IF                           
140300     IF W-IDLEVNR-ALPHA = 'BPTMC'                                         
140400        MOVE '11377' TO W-IDLEVNR-ALPHA  END-IF                           
140500     IF W-IDLEVNR-ALPHA = 'BP8DB'                                         
140600        MOVE '13585' TO W-IDLEVNR-ALPHA  END-IF                           
140700     IF W-IDLEVNR-ALPHA = 'BP8DD'                                         
140800        MOVE '13622' TO W-IDLEVNR-ALPHA  END-IF                           
140900     IF W-IDLEVNR-ALPHA = 'T8LLA'                                         
141000        MOVE '13801' TO W-IDLEVNR-ALPHA  END-IF                           
141100     IF W-IDLEVNR-ALPHA = 'D06MA'                                         
141200        MOVE '13849' TO W-IDLEVNR-ALPHA  END-IF                           
141300     IF W-IDLEVNR-ALPHA = 'B47PG'                                         
141400        MOVE '14280' TO W-IDLEVNR-ALPHA  END-IF                           
141500     IF W-IDLEVNR-ALPHA = 'CNXXA'                                         
141600        MOVE '14500' TO W-IDLEVNR-ALPHA  END-IF                           
141700     IF W-IDLEVNR-ALPHA = 'BPW0A'                                         
141800        MOVE '14519' TO W-IDLEVNR-ALPHA  END-IF                           
141900     IF W-IDLEVNR-ALPHA = 'C9D2A'                                         
142000        MOVE '14944' TO W-IDLEVNR-ALPHA  END-IF                           
142100     IF W-IDLEVNR-ALPHA = 'BPTME'                                         
142200        MOVE '16036' TO W-IDLEVNR-ALPHA  END-IF                           
142300     IF W-IDLEVNR-ALPHA = 'M279C'                                         
142400        MOVE '16144' TO W-IDLEVNR-ALPHA  END-IF                           
142500     IF W-IDLEVNR-ALPHA = 'M279E'                                         
142600        MOVE '16145' TO W-IDLEVNR-ALPHA  END-IF                           
142700     IF W-IDLEVNR-ALPHA = 'U2W5B'                                         
142800        MOVE '16274' TO W-IDLEVNR-ALPHA  END-IF                           
142900     IF W-IDLEVNR-ALPHA = 'K1FWB'                                         
143000        MOVE '16332' TO W-IDLEVNR-ALPHA  END-IF                           
143100     IF W-IDLEVNR-ALPHA = 'D059D'                                         
143200        MOVE '19564' TO W-IDLEVNR-ALPHA  END-IF                           
143300     IF W-IDLEVNR-ALPHA = 'BP8DC'                                         
143400        MOVE '19609' TO W-IDLEVNR-ALPHA  END-IF                           
143500     IF W-IDLEVNR-ALPHA = 'G1UHN'                                         
143600        MOVE '21590' TO W-IDLEVNR-ALPHA  END-IF                           
143700     IF W-IDLEVNR-ALPHA = 'D30FA'                                         
143800        MOVE '22419' TO W-IDLEVNR-ALPHA  END-IF                           
143900     IF W-IDLEVNR-ALPHA = 'D01QA'                                         
144000        MOVE '22420' TO W-IDLEVNR-ALPHA  END-IF                           
144100     IF W-IDLEVNR-ALPHA = 'D059E'                                         
144200        MOVE '23375' TO W-IDLEVNR-ALPHA  END-IF                           
144300     IF W-IDLEVNR-ALPHA = 'BQ1ZC'                                         
144400        MOVE '23919' TO W-IDLEVNR-ALPHA  END-IF                           
144500     IF W-IDLEVNR-ALPHA = 'D059F'                                         
144600        MOVE '23926' TO W-IDLEVNR-ALPHA  END-IF                           
144700     IF W-IDLEVNR-ALPHA = 'ABD3A'                                         
144800        MOVE '25944' TO W-IDLEVNR-ALPHA  END-IF                           
144900     IF W-IDLEVNR-ALPHA = 'P790M'                                         
145000        MOVE '16149' TO W-IDLEVNR-ALPHA  END-IF                           
145100*************************************************                         
145200     IF W-IDLEVNR-ALPHA = 'DL1YA'                                         
145300        MOVE '3445 ' TO W-IDLEVNR-ALPHA  END-IF                           
145400     IF W-IDLEVNR-ALPHA = 'DLH6A'                                         
145500        MOVE '3449 ' TO W-IDLEVNR-ALPHA  END-IF                           
145600     IF W-IDLEVNR-ALPHA = 'DLH7A'                                         
145700        MOVE '3463 ' TO W-IDLEVNR-ALPHA  END-IF                           
145800     IF W-IDLEVNR-ALPHA = 'DL1ZA'                                         
145900        MOVE '3470 ' TO W-IDLEVNR-ALPHA  END-IF                           
146000     IF W-IDLEVNR-ALPHA = 'DLJAA'                                         
146100        MOVE '3474 ' TO W-IDLEVNR-ALPHA  END-IF                           
146200     IF W-IDLEVNR-ALPHA = 'DLJBA'                                         
146300        MOVE '3482 ' TO W-IDLEVNR-ALPHA  END-IF                           
146400     IF W-IDLEVNR-ALPHA = 'DLJCA'                                         
146500        MOVE '3485 ' TO W-IDLEVNR-ALPHA  END-IF                           
146600     IF W-IDLEVNR-ALPHA = 'DLJDA'                                         
146700        MOVE '3487 ' TO W-IDLEVNR-ALPHA  END-IF                           
146800     IF W-IDLEVNR-ALPHA = 'DLJEA'                                         
146900        MOVE '3495 ' TO W-IDLEVNR-ALPHA  END-IF                           
147000     IF W-IDLEVNR-ALPHA = 'DL2BA'                                         
147100        MOVE '3505 ' TO W-IDLEVNR-ALPHA  END-IF                           
147200     IF W-IDLEVNR-ALPHA = 'BEFXA'                                         
147300        MOVE '3511 ' TO W-IDLEVNR-ALPHA  END-IF                           
147400     IF W-IDLEVNR-ALPHA = 'LYSDA'                                         
147500        MOVE '3537 ' TO W-IDLEVNR-ALPHA  END-IF                           
147600     IF W-IDLEVNR-ALPHA = 'DL2DA'                                         
147700        MOVE '3581 ' TO W-IDLEVNR-ALPHA  END-IF                           
147800     IF W-IDLEVNR-ALPHA = 'KYBHA'                                         
147900        MOVE '3634 ' TO W-IDLEVNR-ALPHA  END-IF                           
148000     IF W-IDLEVNR-ALPHA = 'DLJFA'                                         
148100        MOVE '3641 ' TO W-IDLEVNR-ALPHA  END-IF                           
148200     IF W-IDLEVNR-ALPHA = 'F745A'                                         
148300        MOVE '3660 ' TO W-IDLEVNR-ALPHA  END-IF                           
148400     IF W-IDLEVNR-ALPHA = 'E23VB'                                         
148500        MOVE '3663 ' TO W-IDLEVNR-ALPHA  END-IF                           
148600     IF W-IDLEVNR-ALPHA = 'DLJHA'                                         
148700        MOVE '3681 ' TO W-IDLEVNR-ALPHA  END-IF                           
148800     IF W-IDLEVNR-ALPHA = 'DL2EA'                                         
148900        MOVE '3723 ' TO W-IDLEVNR-ALPHA  END-IF                           
149000     IF W-IDLEVNR-ALPHA = 'DL2FA'                                         
149100        MOVE '3728 ' TO W-IDLEVNR-ALPHA  END-IF                           
149200     IF W-IDLEVNR-ALPHA = 'DL2GA'                                         
149300        MOVE '3732 ' TO W-IDLEVNR-ALPHA  END-IF                           
149400     IF W-IDLEVNR-ALPHA = 'CYMBD'                                         
149500        MOVE '3751 ' TO W-IDLEVNR-ALPHA  END-IF                           
149600     IF W-IDLEVNR-ALPHA = 'DL2HA'                                         
149700        MOVE '3771 ' TO W-IDLEVNR-ALPHA  END-IF                           
149800     IF W-IDLEVNR-ALPHA = 'DLJJA'                                         
149900        MOVE '3790 ' TO W-IDLEVNR-ALPHA  END-IF                           
150000     IF W-IDLEVNR-ALPHA = 'DL2JA'                                         
150100        MOVE '3793 ' TO W-IDLEVNR-ALPHA  END-IF                           
150200     IF W-IDLEVNR-ALPHA = 'DL2KA'                                         
150300        MOVE '3799 ' TO W-IDLEVNR-ALPHA  END-IF                           
150400     IF W-IDLEVNR-ALPHA = 'DL2LA'                                         
150500        MOVE '3814 ' TO W-IDLEVNR-ALPHA  END-IF                           
150600     IF W-IDLEVNR-ALPHA = 'DL2MA'                                         
150700        MOVE '3830 ' TO W-IDLEVNR-ALPHA  END-IF                           
150800     IF W-IDLEVNR-ALPHA = 'DL2NA'                                         
150900        MOVE '3833 ' TO W-IDLEVNR-ALPHA  END-IF                           
151000     IF W-IDLEVNR-ALPHA = 'DLJKA'                                         
151100        MOVE '3855 ' TO W-IDLEVNR-ALPHA  END-IF                           
151200     IF W-IDLEVNR-ALPHA = 'CP6JB'                                         
151300        MOVE '3861 ' TO W-IDLEVNR-ALPHA  END-IF                           
151400     IF W-IDLEVNR-ALPHA = 'DL2PA'                                         
151500        MOVE '3866 ' TO W-IDLEVNR-ALPHA  END-IF                           
151600     IF W-IDLEVNR-ALPHA = 'R57KA'                                         
151700        MOVE '3925 ' TO W-IDLEVNR-ALPHA  END-IF                           
151800     IF W-IDLEVNR-ALPHA = 'D0UCC'                                         
151900        MOVE '3933 ' TO W-IDLEVNR-ALPHA  END-IF                           
152000     IF W-IDLEVNR-ALPHA = 'DL4SA'                                         
152100        MOVE '3938 ' TO W-IDLEVNR-ALPHA  END-IF                           
152200     IF W-IDLEVNR-ALPHA = 'DLJMA'                                         
152300        MOVE '3941 ' TO W-IDLEVNR-ALPHA  END-IF                           
152400     IF W-IDLEVNR-ALPHA = 'DL4TA'                                         
152500        MOVE '3952 ' TO W-IDLEVNR-ALPHA  END-IF                           
152600     IF W-IDLEVNR-ALPHA = 'DLJPA'                                         
152700        MOVE '3957 ' TO W-IDLEVNR-ALPHA  END-IF                           
152800     IF W-IDLEVNR-ALPHA = 'DL5GB'                                         
152900        MOVE '3970 ' TO W-IDLEVNR-ALPHA  END-IF                           
153000     IF W-IDLEVNR-ALPHA = 'LEMWA'                                         
153100        MOVE '3977 ' TO W-IDLEVNR-ALPHA  END-IF                           
153200     IF W-IDLEVNR-ALPHA = 'S601E'                                         
153300        MOVE '4148 ' TO W-IDLEVNR-ALPHA  END-IF                           
153400     IF W-IDLEVNR-ALPHA = 'G8KDA'                                         
153500        MOVE '4256 ' TO W-IDLEVNR-ALPHA  END-IF                           
153600     IF W-IDLEVNR-ALPHA = 'LRT1A'                                         
153700        MOVE '4488 ' TO W-IDLEVNR-ALPHA  END-IF                           
153800     IF W-IDLEVNR-ALPHA = 'DL5HA'                                         
153900        MOVE '4542 ' TO W-IDLEVNR-ALPHA  END-IF                           
154000     IF W-IDLEVNR-ALPHA = 'CFJBA'                                         
154100        MOVE '4637 ' TO W-IDLEVNR-ALPHA  END-IF                           
154200     IF W-IDLEVNR-ALPHA = 'DLJRA'                                         
154300        MOVE '4700 ' TO W-IDLEVNR-ALPHA  END-IF                           
154400     IF W-IDLEVNR-ALPHA = 'JHZZA'                                         
154500        MOVE '4749 ' TO W-IDLEVNR-ALPHA  END-IF                           
154600     IF W-IDLEVNR-ALPHA = 'R6PFB'                                         
154700        MOVE '4845 ' TO W-IDLEVNR-ALPHA  END-IF                           
154800     IF W-IDLEVNR-ALPHA = 'D5Q3F'                                         
154900        MOVE '4934 ' TO W-IDLEVNR-ALPHA  END-IF                           
155000     IF W-IDLEVNR-ALPHA = 'DL6FA'                                         
155100        MOVE '4968 ' TO W-IDLEVNR-ALPHA  END-IF                           
155200     IF W-IDLEVNR-ALPHA = 'DL6JA'                                         
155300        MOVE '5122 ' TO W-IDLEVNR-ALPHA  END-IF                           
155400     IF W-IDLEVNR-ALPHA = 'LRVLA'                                         
155500        MOVE '5161 ' TO W-IDLEVNR-ALPHA  END-IF                           
155600     IF W-IDLEVNR-ALPHA = 'L3PGE'                                         
155700        MOVE '5182 ' TO W-IDLEVNR-ALPHA  END-IF                           
155800     IF W-IDLEVNR-ALPHA = 'D21XA'                                         
155900        MOVE '5233 ' TO W-IDLEVNR-ALPHA  END-IF                           
156000     IF W-IDLEVNR-ALPHA = 'C8W0A'                                         
156100        MOVE '5295 ' TO W-IDLEVNR-ALPHA  END-IF                           
156200     IF W-IDLEVNR-ALPHA = 'LHSYA'                                         
156300        MOVE '5354 ' TO W-IDLEVNR-ALPHA  END-IF                           
156400     IF W-IDLEVNR-ALPHA = 'AADLA'                                         
156500        MOVE '5436 ' TO W-IDLEVNR-ALPHA  END-IF                           
156600     IF W-IDLEVNR-ALPHA = 'DLJSA'                                         
156700        MOVE '5443 ' TO W-IDLEVNR-ALPHA  END-IF                           
156800     IF W-IDLEVNR-ALPHA = 'DLJTA'                                         
156900        MOVE '5637 ' TO W-IDLEVNR-ALPHA  END-IF                           
157000     IF W-IDLEVNR-ALPHA = 'D26YA'                                         
157100        MOVE '5671 ' TO W-IDLEVNR-ALPHA  END-IF                           
157200     IF W-IDLEVNR-ALPHA = 'MRZ4A'                                         
157300        MOVE '5678 ' TO W-IDLEVNR-ALPHA  END-IF                           
157400     IF W-IDLEVNR-ALPHA = 'DLK7A'                                         
157500        MOVE '6016 ' TO W-IDLEVNR-ALPHA  END-IF                           
157600     IF W-IDLEVNR-ALPHA = 'D1H7A'                                         
157700        MOVE '6024 ' TO W-IDLEVNR-ALPHA  END-IF                           
157800     IF W-IDLEVNR-ALPHA = 'DLLAA'                                         
157900        MOVE '6045 ' TO W-IDLEVNR-ALPHA  END-IF                           
158000     IF W-IDLEVNR-ALPHA = 'B42DA'                                         
158100        MOVE '6053 ' TO W-IDLEVNR-ALPHA  END-IF                           
158200     IF W-IDLEVNR-ALPHA = 'C8F4A'                                         
158300        MOVE '6061 ' TO W-IDLEVNR-ALPHA  END-IF                           
158400     IF W-IDLEVNR-ALPHA = 'EGX6K'                                         
158500        MOVE '6066 ' TO W-IDLEVNR-ALPHA  END-IF                           
158600     IF W-IDLEVNR-ALPHA = 'DLLBA'                                         
158700        MOVE '6069 ' TO W-IDLEVNR-ALPHA  END-IF                           
158800     IF W-IDLEVNR-ALPHA = 'B40WB'                                         
158900        MOVE '6080 ' TO W-IDLEVNR-ALPHA  END-IF                           
159000     IF W-IDLEVNR-ALPHA = 'D04DA'                                         
159100        MOVE '6098 ' TO W-IDLEVNR-ALPHA  END-IF                           
159200     IF W-IDLEVNR-ALPHA = 'D33QB'                                         
159300        MOVE '6120 ' TO W-IDLEVNR-ALPHA  END-IF                           
159400     IF W-IDLEVNR-ALPHA = 'D23JB'                                         
159500        MOVE '6151 ' TO W-IDLEVNR-ALPHA  END-IF                           
159600     IF W-IDLEVNR-ALPHA = 'DZK7A'                                         
159700        MOVE '6158 ' TO W-IDLEVNR-ALPHA  END-IF                           
159800     IF W-IDLEVNR-ALPHA = 'C8S2B'                                         
159900        MOVE '6202 ' TO W-IDLEVNR-ALPHA  END-IF                           
160000     IF W-IDLEVNR-ALPHA = 'D8NLJ'                                         
160100        MOVE '6228 ' TO W-IDLEVNR-ALPHA  END-IF                           
160200     IF W-IDLEVNR-ALPHA = 'DLLCA'                                         
160300        MOVE '6245 ' TO W-IDLEVNR-ALPHA  END-IF                           
160400     IF W-IDLEVNR-ALPHA = 'D0NNA'                                         
160500        MOVE '6269 ' TO W-IDLEVNR-ALPHA  END-IF                           
160600     IF W-IDLEVNR-ALPHA = 'D0SYA'                                         
160700        MOVE '6283 ' TO W-IDLEVNR-ALPHA  END-IF                           
160800*************************************************                         
160900     IF W-IDLEVNR-ALPHA = 'BQ8YA'                                         
161000        MOVE '80   ' TO W-IDLEVNR-ALPHA  END-IF                           
161100     IF W-IDLEVNR-ALPHA = 'BQ0FA'                                         
161200        MOVE '511  ' TO W-IDLEVNR-ALPHA  END-IF                           
161300     IF W-IDLEVNR-ALPHA = 'BQAGA'                                         
161400        MOVE '894  ' TO W-IDLEVNR-ALPHA  END-IF                           
161500     IF W-IDLEVNR-ALPHA = 'S5PQB'                                         
161600        MOVE '1345 ' TO W-IDLEVNR-ALPHA  END-IF                           
161700     IF W-IDLEVNR-ALPHA = 'LESLA'                                         
161800        MOVE '2333 ' TO W-IDLEVNR-ALPHA  END-IF                           
161900     IF W-IDLEVNR-ALPHA = 'BQ6MA'                                         
162000        MOVE '3050 ' TO W-IDLEVNR-ALPHA  END-IF                           
162100     IF W-IDLEVNR-ALPHA = 'L9GXB'                                         
162200        MOVE '3135 ' TO W-IDLEVNR-ALPHA  END-IF                           
162300     IF W-IDLEVNR-ALPHA = 'L9GXA'                                         
162400        MOVE '3594 ' TO W-IDLEVNR-ALPHA  END-IF                           
162500     IF W-IDLEVNR-ALPHA = 'C9A3A'                                         
162600        MOVE '3616 ' TO W-IDLEVNR-ALPHA  END-IF                           
162700     IF W-IDLEVNR-ALPHA = 'S356A'                                         
162800        MOVE '3752 ' TO W-IDLEVNR-ALPHA  END-IF                           
162900     IF W-IDLEVNR-ALPHA = 'AYSCB'                                         
163000        MOVE '3755 ' TO W-IDLEVNR-ALPHA  END-IF                           
163100     IF W-IDLEVNR-ALPHA = 'E23LA'                                         
163200        MOVE '3912 ' TO W-IDLEVNR-ALPHA  END-IF                           
163300     IF W-IDLEVNR-ALPHA = 'D04JA'                                         
163400        MOVE '4515 ' TO W-IDLEVNR-ALPHA  END-IF                           
163500     IF W-IDLEVNR-ALPHA = 'A628A'                                         
163600        MOVE '5049 ' TO W-IDLEVNR-ALPHA  END-IF                           
163700     IF W-IDLEVNR-ALPHA = 'C66SJ'                                         
163800        MOVE '5085 ' TO W-IDLEVNR-ALPHA  END-IF                           
163900     IF W-IDLEVNR-ALPHA = 'D3C3A'                                         
164000        MOVE '5093 ' TO W-IDLEVNR-ALPHA  END-IF                           
164100     IF W-IDLEVNR-ALPHA = 'C8V8A'                                         
164200        MOVE '5744 ' TO W-IDLEVNR-ALPHA  END-IF                           
164300     IF W-IDLEVNR-ALPHA = 'F4SWG'                                         
164400        MOVE '6022 ' TO W-IDLEVNR-ALPHA  END-IF                           
164500     IF W-IDLEVNR-ALPHA = 'AN3AA'                                         
164600        MOVE '6118 ' TO W-IDLEVNR-ALPHA  END-IF                           
164700     IF W-IDLEVNR-ALPHA = 'H518X'                                         
164800        MOVE '6215 ' TO W-IDLEVNR-ALPHA  END-IF                           
164900     IF W-IDLEVNR-ALPHA = 'CJ6EA'                                         
165000        MOVE '6345 ' TO W-IDLEVNR-ALPHA  END-IF                           
165100     IF W-IDLEVNR-ALPHA = 'B492E'                                         
165200        MOVE '6538 ' TO W-IDLEVNR-ALPHA  END-IF                           
165300     IF W-IDLEVNR-ALPHA = 'B492A'                                         
165400        MOVE '6587 ' TO W-IDLEVNR-ALPHA  END-IF                           
165500     IF W-IDLEVNR-ALPHA = 'C8W2A'                                         
165600        MOVE '7213 ' TO W-IDLEVNR-ALPHA  END-IF                           
165700     IF W-IDLEVNR-ALPHA = 'CFT5A'                                         
165800        MOVE '7609 ' TO W-IDLEVNR-ALPHA  END-IF                           
165900     IF W-IDLEVNR-ALPHA = 'BPLDA'                                         
166000        MOVE '10131' TO W-IDLEVNR-ALPHA  END-IF                           
166100     IF W-IDLEVNR-ALPHA = 'BPLDD'                                         
166200        MOVE '10138' TO W-IDLEVNR-ALPHA  END-IF                           
166300     IF W-IDLEVNR-ALPHA = 'CRK8A'                                         
166400        MOVE '11099' TO W-IDLEVNR-ALPHA  END-IF                           
166500     IF W-IDLEVNR-ALPHA = 'BPLDC'                                         
166600        MOVE '13633' TO W-IDLEVNR-ALPHA  END-IF                           
166700     IF W-IDLEVNR-ALPHA = 'C7U7A'                                         
166800        MOVE '14493' TO W-IDLEVNR-ALPHA  END-IF                           
166900     IF W-IDLEVNR-ALPHA = 'C685B'                                         
167000        MOVE '16213' TO W-IDLEVNR-ALPHA  END-IF                           
167100     IF W-IDLEVNR-ALPHA = 'R19YA'                                         
167200        MOVE '17779' TO W-IDLEVNR-ALPHA  END-IF                           
167300     IF W-IDLEVNR-ALPHA = 'BTTTA'                                         
167400        MOVE '23071' TO W-IDLEVNR-ALPHA  END-IF                           
167500     IF W-IDLEVNR-ALPHA = 'CYVBA'                                         
167600        MOVE '23937' TO W-IDLEVNR-ALPHA  END-IF                           
167700     IF W-IDLEVNR-ALPHA = 'DSKFA'                                         
167800        MOVE '26013' TO W-IDLEVNR-ALPHA  END-IF                           
167900*************************************************                         
168000     IF W-IDLEVNR-ALPHA = 'E520A'                                         
168100        MOVE '6293 ' TO W-IDLEVNR-ALPHA  END-IF                           
168200     IF W-IDLEVNR-ALPHA = 'DL6MA'                                         
168300        MOVE '6307 ' TO W-IDLEVNR-ALPHA  END-IF                           
168400     IF W-IDLEVNR-ALPHA = 'H8Z2A'                                         
168500        MOVE '6326 ' TO W-IDLEVNR-ALPHA  END-IF                           
168600     IF W-IDLEVNR-ALPHA = 'DL6PA'                                         
168700        MOVE '6357 ' TO W-IDLEVNR-ALPHA  END-IF                           
168800     IF W-IDLEVNR-ALPHA = 'DMZCA'                                         
168900        MOVE '6410 ' TO W-IDLEVNR-ALPHA  END-IF                           
169000     IF W-IDLEVNR-ALPHA = 'U0VSA'                                         
169100        MOVE '6425 ' TO W-IDLEVNR-ALPHA  END-IF                           
169200     IF W-IDLEVNR-ALPHA = 'C91WA'                                         
169300        MOVE '6429 ' TO W-IDLEVNR-ALPHA  END-IF                           
169400     IF W-IDLEVNR-ALPHA = 'DLLFA'                                         
169500        MOVE '6430 ' TO W-IDLEVNR-ALPHA  END-IF                           
169600     IF W-IDLEVNR-ALPHA = 'V04BA'                                         
169700        MOVE '6510 ' TO W-IDLEVNR-ALPHA  END-IF                           
169800     IF W-IDLEVNR-ALPHA = 'C7F4A'                                         
169900        MOVE '6524 ' TO W-IDLEVNR-ALPHA  END-IF                           
170000     IF W-IDLEVNR-ALPHA = 'DLLGA'                                         
170100        MOVE '6562 ' TO W-IDLEVNR-ALPHA  END-IF                           
170200     IF W-IDLEVNR-ALPHA = 'L9SYA'                                         
170300        MOVE '6595 ' TO W-IDLEVNR-ALPHA  END-IF                           
170400     IF W-IDLEVNR-ALPHA = 'DLLJA'                                         
170500        MOVE '6603 ' TO W-IDLEVNR-ALPHA  END-IF                           
170600     IF W-IDLEVNR-ALPHA = 'DLLKA'                                         
170700        MOVE '6641 ' TO W-IDLEVNR-ALPHA  END-IF                           
170800     IF W-IDLEVNR-ALPHA = 'DLLLA'                                         
170900        MOVE '6643 ' TO W-IDLEVNR-ALPHA  END-IF                           
171000     IF W-IDLEVNR-ALPHA = 'CKBRA'                                         
171100        MOVE '6656 ' TO W-IDLEVNR-ALPHA  END-IF                           
171200     IF W-IDLEVNR-ALPHA = 'JFYXA'                                         
171300        MOVE '6691 ' TO W-IDLEVNR-ALPHA  END-IF                           
171400     IF W-IDLEVNR-ALPHA = 'C8Q3A'                                         
171500        MOVE '6698 ' TO W-IDLEVNR-ALPHA  END-IF                           
171600     IF W-IDLEVNR-ALPHA = 'DL6QA'                                         
171700        MOVE '6714 ' TO W-IDLEVNR-ALPHA  END-IF                           
171800     IF W-IDLEVNR-ALPHA = 'DLLNA'                                         
171900        MOVE '6761 ' TO W-IDLEVNR-ALPHA  END-IF                           
172000     IF W-IDLEVNR-ALPHA = 'V0TAA'                                         
172100        MOVE '6783 ' TO W-IDLEVNR-ALPHA  END-IF                           
172200     IF W-IDLEVNR-ALPHA = 'C92KA'                                         
172300        MOVE '6812 ' TO W-IDLEVNR-ALPHA  END-IF                           
172400     IF W-IDLEVNR-ALPHA = 'D25WB'                                         
172500        MOVE '6825 ' TO W-IDLEVNR-ALPHA  END-IF                           
172600     IF W-IDLEVNR-ALPHA = 'P4WSA'                                         
172700        MOVE '6847 ' TO W-IDLEVNR-ALPHA  END-IF                           
172800     IF W-IDLEVNR-ALPHA = 'A708A'                                         
172900        MOVE '6869 ' TO W-IDLEVNR-ALPHA  END-IF                           
173000     IF W-IDLEVNR-ALPHA = 'S3C3A'                                         
173100        MOVE '6870 ' TO W-IDLEVNR-ALPHA  END-IF                           
173200     IF W-IDLEVNR-ALPHA = 'DL6TA'                                         
173300        MOVE '6885 ' TO W-IDLEVNR-ALPHA  END-IF                           
173400     IF W-IDLEVNR-ALPHA = 'DL6VA'                                         
173500        MOVE '6892 ' TO W-IDLEVNR-ALPHA  END-IF                           
173600     IF W-IDLEVNR-ALPHA = 'CN5FA'                                         
173700        MOVE '6899 ' TO W-IDLEVNR-ALPHA  END-IF                           
173800     IF W-IDLEVNR-ALPHA = 'N82PA'                                         
173900        MOVE '7038 ' TO W-IDLEVNR-ALPHA  END-IF                           
174000     IF W-IDLEVNR-ALPHA = 'DLLPA'                                         
174100        MOVE '7106 ' TO W-IDLEVNR-ALPHA  END-IF                           
174200     IF W-IDLEVNR-ALPHA = 'HCR1A'                                         
174300        MOVE '7115 ' TO W-IDLEVNR-ALPHA  END-IF                           
174400     IF W-IDLEVNR-ALPHA = 'DLLQA'                                         
174500        MOVE '7132 ' TO W-IDLEVNR-ALPHA  END-IF                           
174600     IF W-IDLEVNR-ALPHA = 'MTJFA'                                         
174700        MOVE '7134 ' TO W-IDLEVNR-ALPHA  END-IF                           
174800     IF W-IDLEVNR-ALPHA = 'MWAJB'                                         
174900        MOVE '7229 ' TO W-IDLEVNR-ALPHA  END-IF                           
175000     IF W-IDLEVNR-ALPHA = 'DL6WA'                                         
175100        MOVE '7235 ' TO W-IDLEVNR-ALPHA  END-IF                           
175200     IF W-IDLEVNR-ALPHA = 'DLLRA'                                         
175300        MOVE '7357 ' TO W-IDLEVNR-ALPHA  END-IF                           
175400     IF W-IDLEVNR-ALPHA = 'DL6YA'                                         
175500        MOVE '7831 ' TO W-IDLEVNR-ALPHA  END-IF                           
175600     IF W-IDLEVNR-ALPHA = 'MHQSA'                                         
175700        MOVE '7955 ' TO W-IDLEVNR-ALPHA  END-IF                           
175800     IF W-IDLEVNR-ALPHA = 'BLLMA'                                         
175900        MOVE '8004 ' TO W-IDLEVNR-ALPHA  END-IF                           
176000     IF W-IDLEVNR-ALPHA = 'BJW6A'                                         
176100        MOVE '8010 ' TO W-IDLEVNR-ALPHA  END-IF                           
176200     IF W-IDLEVNR-ALPHA = 'BKXUA'                                         
176300        MOVE '8023 ' TO W-IDLEVNR-ALPHA  END-IF                           
176400     IF W-IDLEVNR-ALPHA = 'BLMJA'                                         
176500        MOVE '8061 ' TO W-IDLEVNR-ALPHA  END-IF                           
176600     IF W-IDLEVNR-ALPHA = 'DLLTA'                                         
176700        MOVE '8086 ' TO W-IDLEVNR-ALPHA  END-IF                           
176800     IF W-IDLEVNR-ALPHA = 'BKYNA'                                         
176900        MOVE '8094 ' TO W-IDLEVNR-ALPHA  END-IF                           
177000     IF W-IDLEVNR-ALPHA = 'BK5LA'                                         
177100        MOVE '8103 ' TO W-IDLEVNR-ALPHA  END-IF                           
177200     IF W-IDLEVNR-ALPHA = 'DLLUA'                                         
177300        MOVE '8107 ' TO W-IDLEVNR-ALPHA  END-IF                           
177400     IF W-IDLEVNR-ALPHA = 'DL6ZA'                                         
177500        MOVE '8123 ' TO W-IDLEVNR-ALPHA  END-IF                           
177600     IF W-IDLEVNR-ALPHA = 'D13DA'                                         
177700        MOVE '8138 ' TO W-IDLEVNR-ALPHA  END-IF                           
177800     IF W-IDLEVNR-ALPHA = 'BSN7A'                                         
177900        MOVE '8150 ' TO W-IDLEVNR-ALPHA  END-IF                           
178000     IF W-IDLEVNR-ALPHA = 'DLLVA'                                         
178100        MOVE '8151 ' TO W-IDLEVNR-ALPHA  END-IF                           
178200     IF W-IDLEVNR-ALPHA = 'S6LPA'                                         
178300        MOVE '8168 ' TO W-IDLEVNR-ALPHA  END-IF                           
178400     IF W-IDLEVNR-ALPHA = 'CZTVA'                                         
178500        MOVE '8181 ' TO W-IDLEVNR-ALPHA  END-IF                           
178600     IF W-IDLEVNR-ALPHA = 'BSFPA'                                         
178700        MOVE '8190 ' TO W-IDLEVNR-ALPHA  END-IF                           
178800     IF W-IDLEVNR-ALPHA = 'DLLWA'                                         
178900        MOVE '8192 ' TO W-IDLEVNR-ALPHA  END-IF                           
179000     IF W-IDLEVNR-ALPHA = 'BSKXA'                                         
179100        MOVE '8199 ' TO W-IDLEVNR-ALPHA  END-IF                           
179200     IF W-IDLEVNR-ALPHA = 'DL7AA'                                         
179300        MOVE '8203 ' TO W-IDLEVNR-ALPHA  END-IF                           
179400     IF W-IDLEVNR-ALPHA = 'DLLXA'                                         
179500        MOVE '8212 ' TO W-IDLEVNR-ALPHA  END-IF                           
179600     IF W-IDLEVNR-ALPHA = 'S98JA'                                         
179700        MOVE '8216 ' TO W-IDLEVNR-ALPHA  END-IF                           
179800     IF W-IDLEVNR-ALPHA = 'DLLYA'                                         
179900        MOVE '8233 ' TO W-IDLEVNR-ALPHA  END-IF                           
180000     IF W-IDLEVNR-ALPHA = 'DLLZA'                                         
180100        MOVE '8234 ' TO W-IDLEVNR-ALPHA  END-IF                           
180200     IF W-IDLEVNR-ALPHA = 'BAG3A'                                         
180300        MOVE '8860 ' TO W-IDLEVNR-ALPHA  END-IF                           
180400     IF W-IDLEVNR-ALPHA = 'S7YJA'                                         
180500        MOVE '10139' TO W-IDLEVNR-ALPHA  END-IF                           
180600     IF W-IDLEVNR-ALPHA = 'DL7CA'                                         
180700        MOVE '10355' TO W-IDLEVNR-ALPHA  END-IF                           
180800     IF W-IDLEVNR-ALPHA = 'DLL0A'                                         
180900        MOVE '10803' TO W-IDLEVNR-ALPHA  END-IF                           
181000     IF W-IDLEVNR-ALPHA = 'CRJ8A'                                         
181100        MOVE '10910' TO W-IDLEVNR-ALPHA  END-IF                           
181200     IF W-IDLEVNR-ALPHA = 'Q5FPA'                                         
181300        MOVE '11117' TO W-IDLEVNR-ALPHA  END-IF                           
181400     IF W-IDLEVNR-ALPHA = 'D1K4E'                                         
181500        MOVE '12089' TO W-IDLEVNR-ALPHA  END-IF                           
181600     IF W-IDLEVNR-ALPHA = 'BA7ZA'                                         
181700        MOVE '12098' TO W-IDLEVNR-ALPHA  END-IF                           
181800     IF W-IDLEVNR-ALPHA = 'DLL5A'                                         
181900        MOVE '13311' TO W-IDLEVNR-ALPHA  END-IF                           
182000     IF W-IDLEVNR-ALPHA = 'DL7GA'                                         
182100        MOVE '13344' TO W-IDLEVNR-ALPHA  END-IF                           
182200     IF W-IDLEVNR-ALPHA = 'DL7HA'                                         
182300        MOVE '13346' TO W-IDLEVNR-ALPHA  END-IF                           
182400     IF W-IDLEVNR-ALPHA = 'DL7JA'                                         
182500        MOVE '13348' TO W-IDLEVNR-ALPHA  END-IF                           
182600     IF W-IDLEVNR-ALPHA = 'C62JC'                                         
182700        MOVE '13362' TO W-IDLEVNR-ALPHA  END-IF                           
182800     IF W-IDLEVNR-ALPHA = 'DLMEA'                                         
182900        MOVE '13374' TO W-IDLEVNR-ALPHA  END-IF                           
183000     IF W-IDLEVNR-ALPHA = 'DLMFA'                                         
183100        MOVE '13375' TO W-IDLEVNR-ALPHA  END-IF                           
183200     IF W-IDLEVNR-ALPHA = 'DLMGA'                                         
183300        MOVE '13376' TO W-IDLEVNR-ALPHA  END-IF                           
183400*************************************************                         
183500     IF W-IDLEVNR-ALPHA = 'DLMHA'                                         
183600        MOVE '13379' TO W-IDLEVNR-ALPHA  END-IF                           
183700     IF W-IDLEVNR-ALPHA = 'D3W5A'                                         
183800        MOVE '13381' TO W-IDLEVNR-ALPHA  END-IF                           
183900     IF W-IDLEVNR-ALPHA = 'CT3NA'                                         
184000        MOVE '13383' TO W-IDLEVNR-ALPHA  END-IF                           
184100     IF W-IDLEVNR-ALPHA = 'DLMJA'                                         
184200        MOVE '13384' TO W-IDLEVNR-ALPHA  END-IF                           
184300     IF W-IDLEVNR-ALPHA = 'CGECA'                                         
184400        MOVE '13385' TO W-IDLEVNR-ALPHA  END-IF                           
184500     IF W-IDLEVNR-ALPHA = 'DLMLA'                                         
184600        MOVE '13390' TO W-IDLEVNR-ALPHA  END-IF                           
184700     IF W-IDLEVNR-ALPHA = 'DLMMA'                                         
184800        MOVE '13392' TO W-IDLEVNR-ALPHA  END-IF                           
184900     IF W-IDLEVNR-ALPHA = 'DLMPA'                                         
185000        MOVE '13401' TO W-IDLEVNR-ALPHA  END-IF                           
185100     IF W-IDLEVNR-ALPHA = 'DL7MA'                                         
185200        MOVE '13402' TO W-IDLEVNR-ALPHA  END-IF                           
185300     IF W-IDLEVNR-ALPHA = 'CFJDA'                                         
185400        MOVE '13403' TO W-IDLEVNR-ALPHA  END-IF                           
185500     IF W-IDLEVNR-ALPHA = 'DLMSA'                                         
185600        MOVE '13408' TO W-IDLEVNR-ALPHA  END-IF                           
185700     IF W-IDLEVNR-ALPHA = 'DLMTA'                                         
185800        MOVE '13409' TO W-IDLEVNR-ALPHA  END-IF                           
185900     IF W-IDLEVNR-ALPHA = 'DL7NA'                                         
186000        MOVE '13411' TO W-IDLEVNR-ALPHA  END-IF                           
186100     IF W-IDLEVNR-ALPHA = 'DLMUA'                                         
186200        MOVE '13416' TO W-IDLEVNR-ALPHA  END-IF                           
186300     IF W-IDLEVNR-ALPHA = 'DLMVA'                                         
186400        MOVE '13463' TO W-IDLEVNR-ALPHA  END-IF                           
186500     IF W-IDLEVNR-ALPHA = 'DLMWA'                                         
186600        MOVE '13465' TO W-IDLEVNR-ALPHA  END-IF                           
186700     IF W-IDLEVNR-ALPHA = 'BQMJA'                                         
186800        MOVE '13467' TO W-IDLEVNR-ALPHA  END-IF                           
186900     IF W-IDLEVNR-ALPHA = 'DL7RA'                                         
187000        MOVE '13475' TO W-IDLEVNR-ALPHA  END-IF                           
187100     IF W-IDLEVNR-ALPHA = 'AMAAC'                                         
187200        MOVE '13529' TO W-IDLEVNR-ALPHA  END-IF                           
187300     IF W-IDLEVNR-ALPHA = 'LRZ7A'                                         
187400        MOVE '13561' TO W-IDLEVNR-ALPHA  END-IF                           
187500     IF W-IDLEVNR-ALPHA = 'DL7VA'                                         
187600        MOVE '13565' TO W-IDLEVNR-ALPHA  END-IF                           
187700     IF W-IDLEVNR-ALPHA = 'BVNUC'                                         
187800        MOVE '13576' TO W-IDLEVNR-ALPHA  END-IF                           
187900     IF W-IDLEVNR-ALPHA = 'DLMYA'                                         
188000        MOVE '13621' TO W-IDLEVNR-ALPHA  END-IF                           
188100     IF W-IDLEVNR-ALPHA = 'DL7XA'                                         
188200        MOVE '13776' TO W-IDLEVNR-ALPHA  END-IF                           
188300     IF W-IDLEVNR-ALPHA = 'Q99AA'                                         
188400        MOVE '14483' TO W-IDLEVNR-ALPHA  END-IF                           
188500     IF W-IDLEVNR-ALPHA = 'CFH6A'                                         
188600        MOVE '14578' TO W-IDLEVNR-ALPHA  END-IF                           
188700     IF W-IDLEVNR-ALPHA = 'DLMZA'                                         
188800        MOVE '14592' TO W-IDLEVNR-ALPHA  END-IF                           
188900     IF W-IDLEVNR-ALPHA = 'DLM1A'                                         
189000        MOVE '14921' TO W-IDLEVNR-ALPHA  END-IF                           
189100     IF W-IDLEVNR-ALPHA = 'DLM3A'                                         
189200        MOVE '14935' TO W-IDLEVNR-ALPHA  END-IF                           
189300     IF W-IDLEVNR-ALPHA = 'DLNBA'                                         
189400        MOVE '14935' TO W-IDLEVNR-ALPHA  END-IF                           
189500     IF W-IDLEVNR-ALPHA = 'MHLTA'                                         
189600        MOVE '15065' TO W-IDLEVNR-ALPHA  END-IF                           
189700     IF W-IDLEVNR-ALPHA = 'DLNCA'                                         
189800        MOVE '15180' TO W-IDLEVNR-ALPHA  END-IF                           
189900     IF W-IDLEVNR-ALPHA = 'DLNDA'                                         
190000        MOVE '15207' TO W-IDLEVNR-ALPHA  END-IF                           
190100     IF W-IDLEVNR-ALPHA = 'N6WEA'                                         
190200        MOVE '15227' TO W-IDLEVNR-ALPHA  END-IF                           
190300     IF W-IDLEVNR-ALPHA = 'N7381'                                         
190400        MOVE '15256' TO W-IDLEVNR-ALPHA  END-IF                           
190500     IF W-IDLEVNR-ALPHA = 'DLNEA'                                         
190600        MOVE '15260' TO W-IDLEVNR-ALPHA  END-IF                           
190700     IF W-IDLEVNR-ALPHA = 'DLNFA'                                         
190800        MOVE '15266' TO W-IDLEVNR-ALPHA  END-IF                           
190900     IF W-IDLEVNR-ALPHA = 'DLNGA'                                         
191000        MOVE '15310' TO W-IDLEVNR-ALPHA  END-IF                           
191100     IF W-IDLEVNR-ALPHA = 'DLNHA'                                         
191200        MOVE '15322' TO W-IDLEVNR-ALPHA  END-IF                           
191300     IF W-IDLEVNR-ALPHA = 'DL8BA'                                         
191400        MOVE '15325' TO W-IDLEVNR-ALPHA  END-IF                           
191500     IF W-IDLEVNR-ALPHA = 'DL8CA'                                         
191600        MOVE '16087' TO W-IDLEVNR-ALPHA  END-IF                           
191700     IF W-IDLEVNR-ALPHA = 'S3MZA'                                         
191800        MOVE '16090' TO W-IDLEVNR-ALPHA  END-IF                           
191900     IF W-IDLEVNR-ALPHA = 'CN5PA'                                         
192000        MOVE '16116' TO W-IDLEVNR-ALPHA  END-IF                           
192100     IF W-IDLEVNR-ALPHA = 'T93AB'                                         
192200        MOVE '16123' TO W-IDLEVNR-ALPHA  END-IF                           
192300     IF W-IDLEVNR-ALPHA = 'K760B'                                         
192400        MOVE '16367' TO W-IDLEVNR-ALPHA  END-IF                           
192500     IF W-IDLEVNR-ALPHA = 'S5S2A'                                         
192600        MOVE '16466' TO W-IDLEVNR-ALPHA  END-IF                           
192700     IF W-IDLEVNR-ALPHA = 'D3R3A'                                         
192800        MOVE '17782' TO W-IDLEVNR-ALPHA  END-IF                           
192900     IF W-IDLEVNR-ALPHA = 'C9G4A'                                         
193000        MOVE '17784' TO W-IDLEVNR-ALPHA  END-IF                           
193100     IF W-IDLEVNR-ALPHA = 'DLNLA'                                         
193200        MOVE '17785' TO W-IDLEVNR-ALPHA  END-IF                           
193300     IF W-IDLEVNR-ALPHA = 'DLNMA'                                         
193400        MOVE '17792' TO W-IDLEVNR-ALPHA  END-IF                           
193500     IF W-IDLEVNR-ALPHA = 'DLNNA'                                         
193600        MOVE '17813' TO W-IDLEVNR-ALPHA  END-IF                           
193700     IF W-IDLEVNR-ALPHA = 'BKPQA'                                         
193800        MOVE '18000' TO W-IDLEVNR-ALPHA  END-IF                           
193900     IF W-IDLEVNR-ALPHA = 'BKHYA'                                         
194000        MOVE '18012' TO W-IDLEVNR-ALPHA  END-IF                           
194100     IF W-IDLEVNR-ALPHA = 'BSK0A'                                         
194200        MOVE '18060' TO W-IDLEVNR-ALPHA  END-IF                           
194300     IF W-IDLEVNR-ALPHA = 'DLNQA'                                         
194400        MOVE '18067' TO W-IDLEVNR-ALPHA  END-IF                           
194500     IF W-IDLEVNR-ALPHA = 'DLNRA'                                         
194600        MOVE '18075' TO W-IDLEVNR-ALPHA  END-IF                           
194700     IF W-IDLEVNR-ALPHA = 'CXC8A'                                         
194800        MOVE '18120' TO W-IDLEVNR-ALPHA  END-IF                           
194900     IF W-IDLEVNR-ALPHA = 'DLNSA'                                         
195000        MOVE '18977' TO W-IDLEVNR-ALPHA  END-IF                           
195100     IF W-IDLEVNR-ALPHA = 'DLNTA'                                         
195200        MOVE '19052' TO W-IDLEVNR-ALPHA  END-IF                           
195300     IF W-IDLEVNR-ALPHA = 'BK4FA'                                         
195400        MOVE '19235' TO W-IDLEVNR-ALPHA  END-IF                           
195700     IF W-IDLEVNR-ALPHA = 'AYGHA'                                         
195800        MOVE '20895' TO W-IDLEVNR-ALPHA  END-IF                           
195900     IF W-IDLEVNR-ALPHA = 'CECDA'                                         
196000        MOVE '21580' TO W-IDLEVNR-ALPHA  END-IF                           
196100     IF W-IDLEVNR-ALPHA = 'CGSCA'                                         
196200        MOVE '23939' TO W-IDLEVNR-ALPHA  END-IF                           
196300     IF W-IDLEVNR-ALPHA = 'DL8EA'                                         
196400        MOVE '23941' TO W-IDLEVNR-ALPHA  END-IF                           
196500     IF W-IDLEVNR-ALPHA = 'ATPUB'                                         
196600        MOVE '24633' TO W-IDLEVNR-ALPHA  END-IF                           
196700     IF W-IDLEVNR-ALPHA = 'DL8FA'                                         
196800        MOVE '24837' TO W-IDLEVNR-ALPHA  END-IF                           
196900     IF W-IDLEVNR-ALPHA = 'DLNWA'                                         
197000        MOVE '25012' TO W-IDLEVNR-ALPHA  END-IF                           
197100     IF W-IDLEVNR-ALPHA = 'BLWQA'                                         
197200        MOVE '25745' TO W-IDLEVNR-ALPHA  END-IF                           
197300     IF W-IDLEVNR-ALPHA = 'DN6EA'                                         
197400        MOVE '25955' TO W-IDLEVNR-ALPHA  END-IF                           
197500     IF W-IDLEVNR-ALPHA = 'A224A'                                         
197600        MOVE '50049' TO W-IDLEVNR-ALPHA  END-IF                           
197700     IF W-IDLEVNR-ALPHA = 'G8VUE'                                         
197800        MOVE '55005' TO W-IDLEVNR-ALPHA  END-IF                           
197900     IF W-IDLEVNR-ALPHA = 'CZ6AB'                                         
198000        MOVE '80096' TO W-IDLEVNR-ALPHA  END-IF                           
198100*************************************************                         
198200     IF W-IDLEVNR-ALPHA = 'BQYJA'                                         
198300        MOVE '87   ' TO W-IDLEVNR-ALPHA  END-IF                           
198400     IF W-IDLEVNR-ALPHA = 'N81ZA'                                         
198500        MOVE '1186 ' TO W-IDLEVNR-ALPHA  END-IF                           
198600     IF W-IDLEVNR-ALPHA = 'S6NKA'                                         
198700        MOVE '1447 ' TO W-IDLEVNR-ALPHA  END-IF                           
198800     IF W-IDLEVNR-ALPHA = 'BVNQA'                                         
198900        MOVE '2318 ' TO W-IDLEVNR-ALPHA  END-IF                           
199000     IF W-IDLEVNR-ALPHA = 'BQEKA'                                         
199100        MOVE '2356 ' TO W-IDLEVNR-ALPHA  END-IF                           
199200     IF W-IDLEVNR-ALPHA = 'BWKTA'                                         
199300        MOVE '3169 ' TO W-IDLEVNR-ALPHA  END-IF                           
199400     IF W-IDLEVNR-ALPHA = 'T2FBB'                                         
199500        MOVE '3803 ' TO W-IDLEVNR-ALPHA  END-IF                           
199600     IF W-IDLEVNR-ALPHA = 'D0DAA'                                         
199700        MOVE '3894 ' TO W-IDLEVNR-ALPHA  END-IF                           
199800     IF W-IDLEVNR-ALPHA = 'S17VA'                                         
199900        MOVE '3989 ' TO W-IDLEVNR-ALPHA  END-IF                           
200000     IF W-IDLEVNR-ALPHA = 'J3ZLA'                                         
200100        MOVE '4618 ' TO W-IDLEVNR-ALPHA  END-IF                           
200200     IF W-IDLEVNR-ALPHA = 'R8LDA'                                         
200300        MOVE '5212 ' TO W-IDLEVNR-ALPHA  END-IF                           
200400     IF W-IDLEVNR-ALPHA = 'V0QEA'                                         
200500        MOVE '5684 ' TO W-IDLEVNR-ALPHA  END-IF                           
200600     IF W-IDLEVNR-ALPHA = 'G769B'                                         
200700        MOVE '6063 ' TO W-IDLEVNR-ALPHA  END-IF                           
200800     IF W-IDLEVNR-ALPHA = 'C99ZA'                                         
200900        MOVE '6119 ' TO W-IDLEVNR-ALPHA  END-IF                           
201000     IF W-IDLEVNR-ALPHA = 'D1E4A'                                         
201100        MOVE '6166 ' TO W-IDLEVNR-ALPHA  END-IF                           
201200     IF W-IDLEVNR-ALPHA = 'D2H8A'                                         
201300        MOVE '6437 ' TO W-IDLEVNR-ALPHA  END-IF                           
201400     IF W-IDLEVNR-ALPHA = 'G952A'                                         
201500        MOVE '6515 ' TO W-IDLEVNR-ALPHA  END-IF                           
201600     IF W-IDLEVNR-ALPHA = 'Q520A'                                         
201700        MOVE '6522 ' TO W-IDLEVNR-ALPHA  END-IF                           
201800     IF W-IDLEVNR-ALPHA = 'EFR5A'                                         
201900        MOVE '6719 ' TO W-IDLEVNR-ALPHA  END-IF                           
202000     IF W-IDLEVNR-ALPHA = 'L905D'                                         
202100        MOVE '6771 ' TO W-IDLEVNR-ALPHA  END-IF                           
202200     IF W-IDLEVNR-ALPHA = 'BQ7RA'                                         
202300        MOVE '6828 ' TO W-IDLEVNR-ALPHA  END-IF                           
202400     IF W-IDLEVNR-ALPHA = 'U5P7A'                                         
202500        MOVE '6836 ' TO W-IDLEVNR-ALPHA  END-IF                           
202600     IF W-IDLEVNR-ALPHA = 'U1LFD'                                         
202700        MOVE '6857 ' TO W-IDLEVNR-ALPHA  END-IF                           
202800     IF W-IDLEVNR-ALPHA = 'ABYHA'                                         
202900        MOVE '6950 ' TO W-IDLEVNR-ALPHA  END-IF                           
203000     IF W-IDLEVNR-ALPHA = 'BPXDA'                                         
203100        MOVE '7040 ' TO W-IDLEVNR-ALPHA  END-IF                           
203200     IF W-IDLEVNR-ALPHA = 'T4UVA'                                         
203300        MOVE '7258 ' TO W-IDLEVNR-ALPHA  END-IF                           
203400     IF W-IDLEVNR-ALPHA = 'J5VHA'                                         
203500        MOVE '7919 ' TO W-IDLEVNR-ALPHA  END-IF                           
203600     IF W-IDLEVNR-ALPHA = 'N5LQA'                                         
203700        MOVE '7927 ' TO W-IDLEVNR-ALPHA  END-IF                           
203800     IF W-IDLEVNR-ALPHA = 'BQJDD'                                         
203900        MOVE '7954 ' TO W-IDLEVNR-ALPHA  END-IF                           
204000     IF W-IDLEVNR-ALPHA = 'BKAMB'                                         
204100        MOVE '10056' TO W-IDLEVNR-ALPHA  END-IF                           
204200     IF W-IDLEVNR-ALPHA = 'BPEVA'                                         
204300        MOVE '11369' TO W-IDLEVNR-ALPHA  END-IF                           
204400     IF W-IDLEVNR-ALPHA = 'T3DVA'                                         
204500        MOVE '13619' TO W-IDLEVNR-ALPHA  END-IF                           
204600     IF W-IDLEVNR-ALPHA = 'D797B'                                         
204700        MOVE '16096' TO W-IDLEVNR-ALPHA  END-IF                           
204800     IF W-IDLEVNR-ALPHA = 'ALTXA'                                         
204900        MOVE '16238' TO W-IDLEVNR-ALPHA  END-IF                           
205000     IF W-IDLEVNR-ALPHA = 'B45JC'                                         
205100        MOVE '16275' TO W-IDLEVNR-ALPHA  END-IF                           
205200     IF W-IDLEVNR-ALPHA = 'DP5UA'                                         
205300        MOVE '16301' TO W-IDLEVNR-ALPHA  END-IF                           
205400     IF W-IDLEVNR-ALPHA = 'BH0ZA'                                         
205500        MOVE '17253' TO W-IDLEVNR-ALPHA  END-IF                           
205600     IF W-IDLEVNR-ALPHA = 'H82NE'                                         
205700        MOVE '17825' TO W-IDLEVNR-ALPHA  END-IF                           
205800     IF W-IDLEVNR-ALPHA = 'H137P'                                         
205900        MOVE '17826' TO W-IDLEVNR-ALPHA  END-IF                           
206000     IF W-IDLEVNR-ALPHA = 'N508A'                                         
206100        MOVE '18127' TO W-IDLEVNR-ALPHA  END-IF                           
206200     IF W-IDLEVNR-ALPHA = 'AMMCB'                                         
206300        MOVE '19959' TO W-IDLEVNR-ALPHA  END-IF                           
206400     IF W-IDLEVNR-ALPHA = 'CF9JA'                                         
206500        MOVE '23271' TO W-IDLEVNR-ALPHA  END-IF                           
206600     IF W-IDLEVNR-ALPHA = 'CZQ7A'                                         
206700        MOVE '23914' TO W-IDLEVNR-ALPHA  END-IF                           
206800     IF W-IDLEVNR-ALPHA = 'H5PLA'                                         
206900        MOVE '23947' TO W-IDLEVNR-ALPHA  END-IF                           
207000     IF W-IDLEVNR-ALPHA = 'CU8WB'                                         
207100        MOVE '24374' TO W-IDLEVNR-ALPHA  END-IF                           
207200     IF W-IDLEVNR-ALPHA = 'AYSPA'                                         
207300        MOVE '24630' TO W-IDLEVNR-ALPHA  END-IF                           
207400     IF W-IDLEVNR-ALPHA = 'BHD8A'                                         
207500        MOVE '25763' TO W-IDLEVNR-ALPHA  END-IF                           
207600     IF W-IDLEVNR-ALPHA = 'DLKCA'                                         
207700        MOVE '1525 ' TO W-IDLEVNR-ALPHA  END-IF                           
207800     IF W-IDLEVNR-ALPHA = 'A76VA'                                         
207900        MOVE '1592 ' TO W-IDLEVNR-ALPHA  END-IF                           
208000     IF W-IDLEVNR-ALPHA = 'D33HE'                                         
208100        MOVE '3963 ' TO W-IDLEVNR-ALPHA  END-IF                           
208200     IF W-IDLEVNR-ALPHA = 'C69HA'                                         
208300        MOVE '6386 ' TO W-IDLEVNR-ALPHA  END-IF                           
208400     IF W-IDLEVNR-ALPHA = 'D0MNA'                                         
208500        MOVE '6808 ' TO W-IDLEVNR-ALPHA  END-IF                           
208600     IF W-IDLEVNR-ALPHA = 'C97RA'                                         
208700        MOVE '6914 ' TO W-IDLEVNR-ALPHA  END-IF                           
208800     IF W-IDLEVNR-ALPHA = 'BK2EA'                                         
208900        MOVE '8186 ' TO W-IDLEVNR-ALPHA  END-IF                           
209000     IF W-IDLEVNR-ALPHA = 'BQ8SA'                                         
209100        MOVE '10221' TO W-IDLEVNR-ALPHA  END-IF                           
209200     IF W-IDLEVNR-ALPHA = 'BUWJA'                                         
209300        MOVE '10511' TO W-IDLEVNR-ALPHA  END-IF                           
209400     IF W-IDLEVNR-ALPHA = 'BEJZA'                                         
209500        MOVE '10659' TO W-IDLEVNR-ALPHA  END-IF                           
209600     IF W-IDLEVNR-ALPHA = 'BNSRA'                                         
209700        MOVE '10813' TO W-IDLEVNR-ALPHA  END-IF                           
209800     IF W-IDLEVNR-ALPHA = 'D0BEB'                                         
209900        MOVE '11332' TO W-IDLEVNR-ALPHA  END-IF                           
210000     IF W-IDLEVNR-ALPHA = 'DL7SA'                                         
210100        MOVE '13484' TO W-IDLEVNR-ALPHA  END-IF                           
210200     IF W-IDLEVNR-ALPHA = 'D0MNE'                                         
210300        MOVE '13508' TO W-IDLEVNR-ALPHA  END-IF                           
210400     IF W-IDLEVNR-ALPHA = 'N0KFA'                                         
210500        MOVE '14465' TO W-IDLEVNR-ALPHA  END-IF                           
210600     IF W-IDLEVNR-ALPHA = 'MWZFA'                                         
210700        MOVE '14658' TO W-IDLEVNR-ALPHA  END-IF                           
210800     IF W-IDLEVNR-ALPHA = 'AYG1A'                                         
210900        MOVE '14756' TO W-IDLEVNR-ALPHA  END-IF                           
211000     IF W-IDLEVNR-ALPHA = 'BQ9RA'                                         
211100        MOVE '15053' TO W-IDLEVNR-ALPHA  END-IF                           
211200     IF W-IDLEVNR-ALPHA = 'D0MND'                                         
211300        MOVE '16049' TO W-IDLEVNR-ALPHA  END-IF                           
211400     IF W-IDLEVNR-ALPHA = 'D0MNF'                                         
211500        MOVE '16075' TO W-IDLEVNR-ALPHA  END-IF                           
211600     IF W-IDLEVNR-ALPHA = 'D33HA'                                         
211700        MOVE '17789' TO W-IDLEVNR-ALPHA  END-IF                           
211800     IF W-IDLEVNR-ALPHA = 'D0MNG'                                         
211900        MOVE '17945' TO W-IDLEVNR-ALPHA  END-IF                           
212000     IF W-IDLEVNR-ALPHA = 'S4HWB'                                         
212100        MOVE '19739' TO W-IDLEVNR-ALPHA  END-IF                           
212200     IF W-IDLEVNR-ALPHA = 'AJ2AA'                                         
212300        MOVE '21004' TO W-IDLEVNR-ALPHA  END-IF                           
212400     IF W-IDLEVNR-ALPHA = 'C8Q8A'                                         
212500        MOVE '22389' TO W-IDLEVNR-ALPHA  END-IF                           
212600     IF W-IDLEVNR-ALPHA = 'C94MA'                                         
212700        MOVE '23335' TO W-IDLEVNR-ALPHA  END-IF                           
212800     IF W-IDLEVNR-ALPHA = 'F477B'                                         
212900        MOVE '23862' TO W-IDLEVNR-ALPHA  END-IF                           
213000     IF W-IDLEVNR-ALPHA = 'P8TWA'                                         
213100        MOVE '25916' TO W-IDLEVNR-ALPHA  END-IF                           
213200     IF W-IDLEVNR-ALPHA = 'DNK9A'                                         
213300        MOVE '25931' TO W-IDLEVNR-ALPHA  END-IF                           
213400*************************************************                         
213500     IF W-IDLEVNR-ALPHA = 'BUPDC'                                         
213600        MOVE '31138' TO W-IDLEVNR-ALPHA  END-IF                           
213700     IF W-IDLEVNR-ALPHA = 'BUPDD'                                         
213800        MOVE '41138' TO W-IDLEVNR-ALPHA  END-IF                           
213900     IF W-IDLEVNR-ALPHA = 'L8K5H'                                         
214000        MOVE '11138' TO W-IDLEVNR-ALPHA  END-IF                           
214100*************************************************                         
214200     IF W-IDLEVNR-ALPHA = 'Q99AA'                                         
214300        MOVE '14483' TO W-IDLEVNR-ALPHA  END-IF                           
214400     IF W-IDLEVNR-ALPHA = 'D38KF'                                         
214500        MOVE '16044' TO W-IDLEVNR-ALPHA  END-IF                           
214600     IF W-IDLEVNR-ALPHA = 'S3MZA'                                         
214700        MOVE '16090' TO W-IDLEVNR-ALPHA  END-IF                           
214800     IF W-IDLEVNR-ALPHA = 'C95YC'                                         
214900        MOVE '16284' TO W-IDLEVNR-ALPHA  END-IF                           
215000     IF W-IDLEVNR-ALPHA = 'C95YB'                                         
215100        MOVE '16378' TO W-IDLEVNR-ALPHA  END-IF                           
215200     IF W-IDLEVNR-ALPHA = 'C95YJ'                                         
215300        MOVE '20501' TO W-IDLEVNR-ALPHA  END-IF                           
215400     IF W-IDLEVNR-ALPHA = 'D02KA'                                         
215500        MOVE '26942' TO W-IDLEVNR-ALPHA  END-IF                           
215600     IF W-IDLEVNR-ALPHA = 'D0UCA'                                         
215700        MOVE '3742 ' TO W-IDLEVNR-ALPHA  END-IF                           
215800     IF W-IDLEVNR-ALPHA = 'B4V0A'                                         
215900        MOVE '6685 ' TO W-IDLEVNR-ALPHA  END-IF                           
216000     IF W-IDLEVNR-ALPHA = 'C95YA'                                         
216100        MOVE '6942 ' TO W-IDLEVNR-ALPHA  END-IF                           
216200     IF W-IDLEVNR-ALPHA = 'S63MA'                                         
216300        MOVE '7683 ' TO W-IDLEVNR-ALPHA  END-IF                           
216400     IF W-IDLEVNR-ALPHA = 'BPFNB'                                         
216500        MOVE '34666' TO W-IDLEVNR-ALPHA  END-IF                           
216600     IF W-IDLEVNR-ALPHA = 'CDCHA'                                         
216700        MOVE '10030' TO W-IDLEVNR-ALPHA  END-IF                           
216800     IF W-IDLEVNR-ALPHA = 'L8K5W'                                         
216900        MOVE '10181' TO W-IDLEVNR-ALPHA  END-IF                           
217000     IF W-IDLEVNR-ALPHA = 'CDV5A'                                         
217100        MOVE '10347' TO W-IDLEVNR-ALPHA  END-IF                           
217200     IF W-IDLEVNR-ALPHA = 'S4MZD'                                         
217300        MOVE '11518' TO W-IDLEVNR-ALPHA  END-IF                           
217400     IF W-IDLEVNR-ALPHA = 'S1XMC'                                         
217500        MOVE '11753' TO W-IDLEVNR-ALPHA  END-IF                           
217600     IF W-IDLEVNR-ALPHA = 'ALRHA'                                         
217700        MOVE '12539' TO W-IDLEVNR-ALPHA  END-IF                           
217800     IF W-IDLEVNR-ALPHA = 'BQ9FA'                                         
217900        MOVE '13444' TO W-IDLEVNR-ALPHA  END-IF                           
218000     IF W-IDLEVNR-ALPHA = 'S1XMD'                                         
218100        MOVE '13543' TO W-IDLEVNR-ALPHA  END-IF                           
218200     IF W-IDLEVNR-ALPHA = 'BUPDA'                                         
218300        MOVE '13635' TO W-IDLEVNR-ALPHA  END-IF                           
218400     IF W-IDLEVNR-ALPHA = 'BQ9PA'                                         
218500        MOVE '14599' TO W-IDLEVNR-ALPHA  END-IF                           
218600     IF W-IDLEVNR-ALPHA = 'P9J4A'                                         
218700        MOVE '14924' TO W-IDLEVNR-ALPHA  END-IF                           
218800     IF W-IDLEVNR-ALPHA = 'D11JA'                                         
218900        MOVE '15442' TO W-IDLEVNR-ALPHA  END-IF                           
219000     IF W-IDLEVNR-ALPHA = 'Q42PA'                                         
219100        MOVE '16062' TO W-IDLEVNR-ALPHA  END-IF                           
219200     IF W-IDLEVNR-ALPHA = 'B4X4A'                                         
219300        MOVE '16071' TO W-IDLEVNR-ALPHA  END-IF                           
219400     IF W-IDLEVNR-ALPHA = 'BPGQA'                                         
219500        MOVE '171  ' TO W-IDLEVNR-ALPHA  END-IF                           
219600     IF W-IDLEVNR-ALPHA = 'DJPSA'                                         
219700        MOVE '17255' TO W-IDLEVNR-ALPHA  END-IF                           
219800     IF W-IDLEVNR-ALPHA = 'AZYXA'                                         
219900        MOVE '18609' TO W-IDLEVNR-ALPHA  END-IF                           
220000     IF W-IDLEVNR-ALPHA = 'R0PRA'                                         
220100        MOVE '19733' TO W-IDLEVNR-ALPHA  END-IF                           
220200     IF W-IDLEVNR-ALPHA = 'BPGQD'                                         
220300        MOVE '2036 ' TO W-IDLEVNR-ALPHA  END-IF                           
220400     IF W-IDLEVNR-ALPHA = 'N2D2E'                                         
220500        MOVE '21756' TO W-IDLEVNR-ALPHA  END-IF                           
220600     IF W-IDLEVNR-ALPHA = 'CTYHA'                                         
220700        MOVE '22396' TO W-IDLEVNR-ALPHA  END-IF                           
220800     IF W-IDLEVNR-ALPHA = 'N81QA'                                         
220900        MOVE '230  ' TO W-IDLEVNR-ALPHA  END-IF                           
221000     IF W-IDLEVNR-ALPHA = 'BJHYA'                                         
221100        MOVE '23245' TO W-IDLEVNR-ALPHA  END-IF                           
221200     IF W-IDLEVNR-ALPHA = 'BQEKA'                                         
221300        MOVE '2356 ' TO W-IDLEVNR-ALPHA  END-IF                           
221400     IF W-IDLEVNR-ALPHA = 'BRTUA'                                         
221500        MOVE '23705' TO W-IDLEVNR-ALPHA  END-IF                           
221600     IF W-IDLEVNR-ALPHA = 'CLDQA'                                         
221700        MOVE '23799' TO W-IDLEVNR-ALPHA  END-IF                           
221800     IF W-IDLEVNR-ALPHA = 'BUPDA'                                         
221900        MOVE '24010' TO W-IDLEVNR-ALPHA  END-IF                           
222000     IF W-IDLEVNR-ALPHA = 'CLMPA'                                         
222100        MOVE '25669' TO W-IDLEVNR-ALPHA  END-IF                           
222200     IF W-IDLEVNR-ALPHA = 'BQ9PB'                                         
222300        MOVE '25691' TO W-IDLEVNR-ALPHA  END-IF                           
222400     IF W-IDLEVNR-ALPHA = 'DA5NA'                                         
222500        MOVE '25764' TO W-IDLEVNR-ALPHA  END-IF                           
222600     IF W-IDLEVNR-ALPHA = 'AQHKA'                                         
222700        MOVE '25874' TO W-IDLEVNR-ALPHA  END-IF                           
222800     IF W-IDLEVNR-ALPHA = 'L8K50'                                         
222900        MOVE '25890' TO W-IDLEVNR-ALPHA  END-IF                           
223000     IF W-IDLEVNR-ALPHA = 'L8K5Z'                                         
223100        MOVE '25891' TO W-IDLEVNR-ALPHA  END-IF                           
223200     IF W-IDLEVNR-ALPHA = 'L8K5Y'                                         
223300        MOVE '25892' TO W-IDLEVNR-ALPHA  END-IF                           
223400     IF W-IDLEVNR-ALPHA = 'L8K5X'                                         
223500        MOVE '25893' TO W-IDLEVNR-ALPHA  END-IF                           
223600     IF W-IDLEVNR-ALPHA = 'DNH1A'                                         
223700        MOVE '25930' TO W-IDLEVNR-ALPHA  END-IF                           
223800     IF W-IDLEVNR-ALPHA = 'CQPRA'                                         
223900        MOVE '2662 ' TO W-IDLEVNR-ALPHA  END-IF                           
224000     IF W-IDLEVNR-ALPHA = 'D0N8A'                                         
224100        MOVE '3350 ' TO W-IDLEVNR-ALPHA  END-IF                           
224200     IF W-IDLEVNR-ALPHA = 'S1XMB'                                         
224300        MOVE '3656 ' TO W-IDLEVNR-ALPHA  END-IF                           
224400     IF W-IDLEVNR-ALPHA = 'S1XMA'                                         
224500        MOVE '3964 ' TO W-IDLEVNR-ALPHA  END-IF                           
224600     IF W-IDLEVNR-ALPHA = 'BPGQB'                                         
224700        MOVE '402  ' TO W-IDLEVNR-ALPHA  END-IF                           
224800     IF W-IDLEVNR-ALPHA = 'S41GF'                                         
224900        MOVE '4234 ' TO W-IDLEVNR-ALPHA  END-IF                           
225000     IF W-IDLEVNR-ALPHA = 'C62JA'                                         
225100        MOVE '5191 ' TO W-IDLEVNR-ALPHA  END-IF                           
225200     IF W-IDLEVNR-ALPHA = 'C9S2A'                                         
225300        MOVE '5670 ' TO W-IDLEVNR-ALPHA  END-IF                           
225400     IF W-IDLEVNR-ALPHA = 'B050B'                                         
225500        MOVE '5674 ' TO W-IDLEVNR-ALPHA  END-IF                           
225600     IF W-IDLEVNR-ALPHA = 'AZYXB'                                         
225700        MOVE '6229 ' TO W-IDLEVNR-ALPHA  END-IF                           
225800     IF W-IDLEVNR-ALPHA = 'CFUDB'                                         
225900        MOVE '6268 ' TO W-IDLEVNR-ALPHA  END-IF                           
226000     IF W-IDLEVNR-ALPHA = 'D3P7B'                                         
226100        MOVE '6282 ' TO W-IDLEVNR-ALPHA  END-IF                           
226200     IF W-IDLEVNR-ALPHA = 'C5408'                                         
226300        MOVE '63058' TO W-IDLEVNR-ALPHA  END-IF                           
226400     IF W-IDLEVNR-ALPHA = 'D1K4A'                                         
226500        MOVE '6598 ' TO W-IDLEVNR-ALPHA  END-IF                           
226600     IF W-IDLEVNR-ALPHA = 'R151A'                                         
226700        MOVE '6666 ' TO W-IDLEVNR-ALPHA  END-IF                           
226800     IF W-IDLEVNR-ALPHA = 'S4MZA'                                         
226900        MOVE '6678 ' TO W-IDLEVNR-ALPHA  END-IF                           
227000     IF W-IDLEVNR-ALPHA = 'ENB6A'                                         
227100        MOVE '6809 ' TO W-IDLEVNR-ALPHA  END-IF                           
227200     IF W-IDLEVNR-ALPHA = 'ABYHA'                                         
227300        MOVE '6950 ' TO W-IDLEVNR-ALPHA  END-IF                           
227400     IF W-IDLEVNR-ALPHA = 'C86HA'                                         
227500        MOVE '6972 ' TO W-IDLEVNR-ALPHA  END-IF                           
227600     IF W-IDLEVNR-ALPHA = 'N2D2A'                                         
227700        MOVE '7847 ' TO W-IDLEVNR-ALPHA  END-IF                           
227800     IF W-IDLEVNR-ALPHA = 'D0GWA'                                         
227900        MOVE '7915 ' TO W-IDLEVNR-ALPHA  END-IF                           
228000     IF W-IDLEVNR-ALPHA = 'BP3JB'                                         
228100        MOVE '843  ' TO W-IDLEVNR-ALPHA  END-IF                           
228200     IF W-IDLEVNR-ALPHA = 'C9P1B'                                         
228300        MOVE '10490' TO W-IDLEVNR-ALPHA  END-IF                           
228400     IF W-IDLEVNR-ALPHA = 'CTX0A'                                         
228500        MOVE '11062' TO W-IDLEVNR-ALPHA  END-IF                           
228600     IF W-IDLEVNR-ALPHA = 'BP8AA'                                         
228700        MOVE '11086' TO W-IDLEVNR-ALPHA  END-IF                           
228800     IF W-IDLEVNR-ALPHA = 'BUWLA'                                         
228900        MOVE '11403' TO W-IDLEVNR-ALPHA  END-IF                           
229000     IF W-IDLEVNR-ALPHA = 'BQH8A'                                         
229100        MOVE '1148 ' TO W-IDLEVNR-ALPHA  END-IF                           
229200     IF W-IDLEVNR-ALPHA = 'CN5NA'                                         
229300        MOVE '11708' TO W-IDLEVNR-ALPHA  END-IF                           
229400     IF W-IDLEVNR-ALPHA = 'BJQNA'                                         
229500        MOVE '12055' TO W-IDLEVNR-ALPHA  END-IF                           
229600     IF W-IDLEVNR-ALPHA = 'Q98ZA'                                         
229700        MOVE '13241' TO W-IDLEVNR-ALPHA  END-IF                           
229800     IF W-IDLEVNR-ALPHA = 'CFUBA'                                         
229900        MOVE '13517' TO W-IDLEVNR-ALPHA  END-IF                           
230000     IF W-IDLEVNR-ALPHA = 'BJPMA'                                         
230100        MOVE '13527' TO W-IDLEVNR-ALPHA  END-IF                           
230200     IF W-IDLEVNR-ALPHA = 'BJPMB'                                         
230300        MOVE '13603' TO W-IDLEVNR-ALPHA  END-IF                           
230400     IF W-IDLEVNR-ALPHA = 'BC3EA'                                         
230500        MOVE '14668' TO W-IDLEVNR-ALPHA  END-IF                           
230600     IF W-IDLEVNR-ALPHA = 'BQ2ZA'                                         
230700        MOVE '1480 ' TO W-IDLEVNR-ALPHA  END-IF                           
230800     IF W-IDLEVNR-ALPHA = 'BQ8DA'                                         
230900        MOVE '1528 ' TO W-IDLEVNR-ALPHA  END-IF                           
231000     IF W-IDLEVNR-ALPHA = 'R9Q4A'                                         
231100        MOVE '16081' TO W-IDLEVNR-ALPHA  END-IF                           
231200     IF W-IDLEVNR-ALPHA = 'J17KA'                                         
231300        MOVE '16358' TO W-IDLEVNR-ALPHA  END-IF                           
231400     IF W-IDLEVNR-ALPHA = 'R3U8A'                                         
231500        MOVE '16371' TO W-IDLEVNR-ALPHA  END-IF                           
231600     IF W-IDLEVNR-ALPHA = 'D0N0E'                                         
231700        MOVE '16735' TO W-IDLEVNR-ALPHA  END-IF                           
231800     IF W-IDLEVNR-ALPHA = 'BQYKB'                                         
231900        MOVE '1689 ' TO W-IDLEVNR-ALPHA  END-IF                           
232000     IF W-IDLEVNR-ALPHA = 'BJPMD'                                         
232100        MOVE '21327' TO W-IDLEVNR-ALPHA  END-IF                           
232200     IF W-IDLEVNR-ALPHA = 'N7WWA'                                         
232300        MOVE '214  ' TO W-IDLEVNR-ALPHA  END-IF                           
232400     IF W-IDLEVNR-ALPHA = 'BPF3A'                                         
232500        MOVE '2213 ' TO W-IDLEVNR-ALPHA  END-IF                           
232600     IF W-IDLEVNR-ALPHA = 'BQ5QA'                                         
232700        MOVE '2222 ' TO W-IDLEVNR-ALPHA  END-IF                           
232800     IF W-IDLEVNR-ALPHA = 'BLNMA'                                         
232900        MOVE '2227 ' TO W-IDLEVNR-ALPHA  END-IF                           
233000     IF W-IDLEVNR-ALPHA = 'H7G6A'                                         
233100        MOVE '2312 ' TO W-IDLEVNR-ALPHA  END-IF                           
233200     IF W-IDLEVNR-ALPHA = 'AXQ1A'                                         
233300        MOVE '24660' TO W-IDLEVNR-ALPHA  END-IF                           
233400     IF W-IDLEVNR-ALPHA = 'AQYSA'                                         
233500        MOVE '25759' TO W-IDLEVNR-ALPHA  END-IF                           
233600     IF W-IDLEVNR-ALPHA = 'BQ6LA'                                         
233700        MOVE '25775' TO W-IDLEVNR-ALPHA  END-IF                           
233800     IF W-IDLEVNR-ALPHA = 'DDCLA'                                         
233900        MOVE '25815' TO W-IDLEVNR-ALPHA  END-IF                           
234000     IF W-IDLEVNR-ALPHA = 'CW0XA'                                         
234100        MOVE '26031' TO W-IDLEVNR-ALPHA  END-IF                           
234200     IF W-IDLEVNR-ALPHA = 'BQ6FA'                                         
234300        MOVE '2634 ' TO W-IDLEVNR-ALPHA  END-IF                           
234400     IF W-IDLEVNR-ALPHA = 'BQYLA'                                         
234500        MOVE '266  ' TO W-IDLEVNR-ALPHA  END-IF                           
234600     IF W-IDLEVNR-ALPHA = 'S9B0B'                                         
234700        MOVE '2670 ' TO W-IDLEVNR-ALPHA  END-IF                           
234800     IF W-IDLEVNR-ALPHA = 'BQ6LA'                                         
234900        MOVE '3043 ' TO W-IDLEVNR-ALPHA  END-IF                           
235000     IF W-IDLEVNR-ALPHA = 'CL3YA'                                         
235100        MOVE '3054 ' TO W-IDLEVNR-ALPHA  END-IF                           
235200     IF W-IDLEVNR-ALPHA = 'BKNKA'                                         
235300        MOVE '3096 ' TO W-IDLEVNR-ALPHA  END-IF                           
235400     IF W-IDLEVNR-ALPHA = 'B34SG'                                         
235500        MOVE '3653 ' TO W-IDLEVNR-ALPHA  END-IF                           
235600     IF W-IDLEVNR-ALPHA = 'AKUMA'                                         
235700        MOVE '3666 ' TO W-IDLEVNR-ALPHA  END-IF                           
235800     IF W-IDLEVNR-ALPHA = 'BQ6TA'                                         
235900        MOVE '3832 ' TO W-IDLEVNR-ALPHA  END-IF                           
236000     IF W-IDLEVNR-ALPHA = 'BP3CA'                                         
236100        MOVE '393  ' TO W-IDLEVNR-ALPHA  END-IF                           
236200     IF W-IDLEVNR-ALPHA = 'BQ6UA'                                         
236300        MOVE '3934 ' TO W-IDLEVNR-ALPHA  END-IF                           
236400     IF W-IDLEVNR-ALPHA = 'Q3MXA'                                         
236500        MOVE '4235 ' TO W-IDLEVNR-ALPHA  END-IF                           
236600     IF W-IDLEVNR-ALPHA = 'A7G7A'                                         
236700        MOVE '4253 ' TO W-IDLEVNR-ALPHA  END-IF                           
236800     IF W-IDLEVNR-ALPHA = 'S9B0A'                                         
236900        MOVE '4597 ' TO W-IDLEVNR-ALPHA  END-IF                           
237000     IF W-IDLEVNR-ALPHA = 'E7R9D'                                         
237100        MOVE '4985 ' TO W-IDLEVNR-ALPHA  END-IF                           
237200     IF W-IDLEVNR-ALPHA = 'BJ6BA'                                         
237300        MOVE '507  ' TO W-IDLEVNR-ALPHA  END-IF                           
237400     IF W-IDLEVNR-ALPHA = 'CN4SA'                                         
237500        MOVE '510  ' TO W-IDLEVNR-ALPHA  END-IF                           
237600     IF W-IDLEVNR-ALPHA = 'E9VKB'                                         
237700        MOVE '5177 ' TO W-IDLEVNR-ALPHA  END-IF                           
237800     IF W-IDLEVNR-ALPHA = 'BLHTA'                                         
237900        MOVE '521  ' TO W-IDLEVNR-ALPHA  END-IF                           
238000     IF W-IDLEVNR-ALPHA = 'F610B'                                         
238100        MOVE '5269 ' TO W-IDLEVNR-ALPHA  END-IF                           
238200     IF W-IDLEVNR-ALPHA = 'C8D2A'                                         
238300        MOVE '6057 ' TO W-IDLEVNR-ALPHA  END-IF                           
238400     IF W-IDLEVNR-ALPHA = 'LBEKA'                                         
238500        MOVE '6154 ' TO W-IDLEVNR-ALPHA  END-IF                           
238600     IF W-IDLEVNR-ALPHA = 'V022A'                                         
238700        MOVE '6175 ' TO W-IDLEVNR-ALPHA  END-IF                           
238800     IF W-IDLEVNR-ALPHA = 'ADSMA'                                         
238900        MOVE '6199 ' TO W-IDLEVNR-ALPHA  END-IF                           
239000     IF W-IDLEVNR-ALPHA = 'D04BB'                                         
239100        MOVE '6244 ' TO W-IDLEVNR-ALPHA  END-IF                           
239200     IF W-IDLEVNR-ALPHA = 'S2Z6B'                                         
239300        MOVE '6330 ' TO W-IDLEVNR-ALPHA  END-IF                           
239400     IF W-IDLEVNR-ALPHA = 'B40XA'                                         
239500        MOVE '6474 ' TO W-IDLEVNR-ALPHA  END-IF                           
239600     IF W-IDLEVNR-ALPHA = 'BQ7QA'                                         
239700        MOVE '6649 ' TO W-IDLEVNR-ALPHA  END-IF                           
239800     IF W-IDLEVNR-ALPHA = 'D0N0A'                                         
239900        MOVE '6849 ' TO W-IDLEVNR-ALPHA  END-IF                           
240000     IF W-IDLEVNR-ALPHA = 'B34SA'                                         
240100        MOVE '7785 ' TO W-IDLEVNR-ALPHA  END-IF                           
240200     IF W-IDLEVNR-ALPHA = 'BUTZA'                                         
240300        MOVE '7958 ' TO W-IDLEVNR-ALPHA  END-IF                           
240400     IF W-IDLEVNR-ALPHA = 'BQYKA'                                         
240500        MOVE '88   ' TO W-IDLEVNR-ALPHA  END-IF                           
240600     IF W-IDLEVNR-ALPHA = 'BKNQA'                                         
240700        MOVE '8888 ' TO W-IDLEVNR-ALPHA  END-IF                           
240800     IF W-IDLEVNR-ALPHA = 'BQ0GA'                                         
240900        MOVE '996  ' TO W-IDLEVNR-ALPHA  END-IF                           
241000*************************************************                         
241100     IF W-IDLEVNR-ALPHA = 'CFNFA'                                         
241200        MOVE '848  ' TO W-IDLEVNR-ALPHA  END-IF                           
241300     IF W-IDLEVNR-ALPHA = 'BX3PA'                                         
241400        MOVE '883  ' TO W-IDLEVNR-ALPHA  END-IF                           
241500     IF W-IDLEVNR-ALPHA = 'BJQRA'                                         
241600        MOVE '1203 ' TO W-IDLEVNR-ALPHA  END-IF                           
241700     IF W-IDLEVNR-ALPHA = 'BQ5SA'                                         
241800        MOVE '2346 ' TO W-IDLEVNR-ALPHA  END-IF                           
241900     IF W-IDLEVNR-ALPHA = 'BQEBA'                                         
242000        MOVE '2363 ' TO W-IDLEVNR-ALPHA  END-IF                           
242100     IF W-IDLEVNR-ALPHA = 'BQ5TA'                                         
242200        MOVE '2368 ' TO W-IDLEVNR-ALPHA  END-IF                           
242300     IF W-IDLEVNR-ALPHA = 'BP3EB'                                         
242400        MOVE '2419 ' TO W-IDLEVNR-ALPHA  END-IF                           
242500     IF W-IDLEVNR-ALPHA = 'AY2QA'                                         
242600        MOVE '3036 ' TO W-IDLEVNR-ALPHA  END-IF                           
242700     IF W-IDLEVNR-ALPHA = 'K3D7A'                                         
242800        MOVE '3712 ' TO W-IDLEVNR-ALPHA  END-IF                           
242900     IF W-IDLEVNR-ALPHA = 'X448A'                                         
243000        MOVE '5249 ' TO W-IDLEVNR-ALPHA  END-IF                           
243100     IF W-IDLEVNR-ALPHA = 'D22FA'                                         
243200        MOVE '6040 ' TO W-IDLEVNR-ALPHA  END-IF                           
243300     IF W-IDLEVNR-ALPHA = 'KKM2A'                                         
243400        MOVE '6174 ' TO W-IDLEVNR-ALPHA  END-IF                           
243500     IF W-IDLEVNR-ALPHA = 'BCK8A'                                         
243600        MOVE '6385 ' TO W-IDLEVNR-ALPHA  END-IF                           
243700     IF W-IDLEVNR-ALPHA = 'T9UZA'                                         
243800        MOVE '6485 ' TO W-IDLEVNR-ALPHA  END-IF                           
243900     IF W-IDLEVNR-ALPHA = 'BQ7NA'                                         
244000        MOVE '6513 ' TO W-IDLEVNR-ALPHA  END-IF                           
244100     IF W-IDLEVNR-ALPHA = 'M279A'                                         
244200        MOVE '6600 ' TO W-IDLEVNR-ALPHA  END-IF                           
244300     IF W-IDLEVNR-ALPHA = 'KTY7A'                                         
244400        MOVE '6614 ' TO W-IDLEVNR-ALPHA  END-IF                           
244500     IF W-IDLEVNR-ALPHA = 'D21UA'                                         
244600        MOVE '6701 ' TO W-IDLEVNR-ALPHA  END-IF                           
244700     IF W-IDLEVNR-ALPHA = 'CFTZA'                                         
244800        MOVE '6952 ' TO W-IDLEVNR-ALPHA  END-IF                           
244900     IF W-IDLEVNR-ALPHA = 'D3T6A'                                         
245000        MOVE '7042 ' TO W-IDLEVNR-ALPHA  END-IF                           
245100     IF W-IDLEVNR-ALPHA = 'BQ7TA'                                         
245200        MOVE '7052 ' TO W-IDLEVNR-ALPHA  END-IF                           
245300     IF W-IDLEVNR-ALPHA = 'R742A'                                         
245400        MOVE '7179 ' TO W-IDLEVNR-ALPHA  END-IF                           
245500     IF W-IDLEVNR-ALPHA = 'CXA0A'                                         
245600        MOVE '7189 ' TO W-IDLEVNR-ALPHA  END-IF                           
245700     IF W-IDLEVNR-ALPHA = 'CXA1A'                                         
245800        MOVE '7190 ' TO W-IDLEVNR-ALPHA  END-IF                           
245900     IF W-IDLEVNR-ALPHA = 'Q725B'                                         
246000        MOVE '7246 ' TO W-IDLEVNR-ALPHA  END-IF                           
246100     IF W-IDLEVNR-ALPHA = 'S35LA'                                         
246200        MOVE '7253 ' TO W-IDLEVNR-ALPHA  END-IF                           
246300     IF W-IDLEVNR-ALPHA = 'G1UHS'                                         
246400        MOVE '7255 ' TO W-IDLEVNR-ALPHA  END-IF                           
246500     IF W-IDLEVNR-ALPHA = 'BP3KA'                                         
246600        MOVE '7470 ' TO W-IDLEVNR-ALPHA  END-IF                           
246700     IF W-IDLEVNR-ALPHA = 'CEFPA'                                         
246800        MOVE '7787 ' TO W-IDLEVNR-ALPHA  END-IF                           
246900     IF W-IDLEVNR-ALPHA = 'CDHVA'                                         
247000        MOVE '10024' TO W-IDLEVNR-ALPHA  END-IF                           
247100     IF W-IDLEVNR-ALPHA = 'BP3EA'                                         
247200        MOVE '10121' TO W-IDLEVNR-ALPHA  END-IF                           
247300     IF W-IDLEVNR-ALPHA = 'CXMTA'                                         
247400        MOVE '10290' TO W-IDLEVNR-ALPHA  END-IF                           
247500     IF W-IDLEVNR-ALPHA = 'BJQRD'                                         
247600        MOVE '10324' TO W-IDLEVNR-ALPHA  END-IF                           
247700     IF W-IDLEVNR-ALPHA = 'C7C5A'                                         
247800        MOVE '10510' TO W-IDLEVNR-ALPHA  END-IF                           
247900     IF W-IDLEVNR-ALPHA = 'BJQRB'                                         
248000        MOVE '10657' TO W-IDLEVNR-ALPHA  END-IF                           
248100     IF W-IDLEVNR-ALPHA = 'V4B1B'                                         
248200        MOVE '11100' TO W-IDLEVNR-ALPHA  END-IF                           
248300     IF W-IDLEVNR-ALPHA = 'G8VTA'                                         
248400        MOVE '11101' TO W-IDLEVNR-ALPHA  END-IF                           
248500     IF W-IDLEVNR-ALPHA = 'BCK8B'                                         
248600        MOVE '11137' TO W-IDLEVNR-ALPHA  END-IF                           
248700     IF W-IDLEVNR-ALPHA = 'BJQRC'                                         
248800        MOVE '11139' TO W-IDLEVNR-ALPHA  END-IF                           
248900     IF W-IDLEVNR-ALPHA = 'BQEBB'                                         
249000        MOVE '11155' TO W-IDLEVNR-ALPHA  END-IF                           
249100     IF W-IDLEVNR-ALPHA = 'E521H'                                         
249200        MOVE '11388' TO W-IDLEVNR-ALPHA  END-IF                           
249300     IF W-IDLEVNR-ALPHA = 'B491E'                                         
249400        MOVE '12637' TO W-IDLEVNR-ALPHA  END-IF                           
249500     IF W-IDLEVNR-ALPHA = 'T3WQA'                                         
249600        MOVE '13537' TO W-IDLEVNR-ALPHA  END-IF                           
249700     IF W-IDLEVNR-ALPHA = 'BPTRA'                                         
249800        MOVE '13550' TO W-IDLEVNR-ALPHA  END-IF                           
249900     IF W-IDLEVNR-ALPHA = 'BPK4A'                                         
250000        MOVE '13777' TO W-IDLEVNR-ALPHA  END-IF                           
250100     IF W-IDLEVNR-ALPHA = 'C79MA'                                         
250200        MOVE '14926' TO W-IDLEVNR-ALPHA  END-IF                           
250300     IF W-IDLEVNR-ALPHA = 'D5E1C'                                         
250400        MOVE '14927' TO W-IDLEVNR-ALPHA  END-IF                           
250500     IF W-IDLEVNR-ALPHA = 'C79ME'                                         
250600        MOVE '14941' TO W-IDLEVNR-ALPHA  END-IF                           
250700     IF W-IDLEVNR-ALPHA = 'T9UZB'                                         
250800        MOVE '16021' TO W-IDLEVNR-ALPHA  END-IF                           
250900     IF W-IDLEVNR-ALPHA = 'B40YA'                                         
251000        MOVE '16084' TO W-IDLEVNR-ALPHA  END-IF                           
251100     IF W-IDLEVNR-ALPHA = 'D38CA'                                         
251200        MOVE '16385' TO W-IDLEVNR-ALPHA  END-IF                           
251300     IF W-IDLEVNR-ALPHA = 'MKB2A'                                         
251400        MOVE '16386' TO W-IDLEVNR-ALPHA  END-IF                           
251500     IF W-IDLEVNR-ALPHA = 'G0MMA'                                         
251600        MOVE '17777' TO W-IDLEVNR-ALPHA  END-IF                           
251700     IF W-IDLEVNR-ALPHA = 'T43NA'                                         
251800        MOVE '17801' TO W-IDLEVNR-ALPHA  END-IF                           
251900     IF W-IDLEVNR-ALPHA = 'S5VSA'                                         
252000        MOVE '18038' TO W-IDLEVNR-ALPHA  END-IF                           
252100     IF W-IDLEVNR-ALPHA = 'CBBNA'                                         
252200        MOVE '18059' TO W-IDLEVNR-ALPHA  END-IF                           
252300     IF W-IDLEVNR-ALPHA = 'B45VD'                                         
252400        MOVE '19725' TO W-IDLEVNR-ALPHA  END-IF                           
252500     IF W-IDLEVNR-ALPHA = 'MKB2B'                                         
252600        MOVE '19995' TO W-IDLEVNR-ALPHA  END-IF                           
252700     IF W-IDLEVNR-ALPHA = 'Q9K3B'                                         
252800        MOVE '23511' TO W-IDLEVNR-ALPHA  END-IF                           
252900     IF W-IDLEVNR-ALPHA = 'S35LB'                                         
253000        MOVE '24248' TO W-IDLEVNR-ALPHA  END-IF                           
253100     IF W-IDLEVNR-ALPHA = 'R742B'                                         
253200        MOVE '24332' TO W-IDLEVNR-ALPHA  END-IF                           
253300     IF W-IDLEVNR-ALPHA = 'ND04W'                                         
253400        MOVE '24967' TO W-IDLEVNR-ALPHA  END-IF                           
253500     IF W-IDLEVNR-ALPHA = 'N8U4A'                                         
253600        MOVE '25784' TO W-IDLEVNR-ALPHA  END-IF                           
253700     IF W-IDLEVNR-ALPHA = 'BPTQC'                                         
253800        MOVE '25809' TO W-IDLEVNR-ALPHA  END-IF                           
253900     IF W-IDLEVNR-ALPHA = 'D08GJ'                                         
254000        MOVE '25889' TO W-IDLEVNR-ALPHA  END-IF                           
254100     IF W-IDLEVNR-ALPHA = 'S044X'                                         
254200        MOVE '25895' TO W-IDLEVNR-ALPHA  END-IF                           
254300     IF W-IDLEVNR-ALPHA = 'B46ZA'                                         
254400        MOVE '25919' TO W-IDLEVNR-ALPHA  END-IF                           
254500     IF W-IDLEVNR-ALPHA = 'KTY7D'                                         
254600        MOVE '25949' TO W-IDLEVNR-ALPHA  END-IF                           
254700*************************************************                         
254800     IF W-IDLEVNR-ALPHA = 'DRDGA'                                         
254900        MOVE '15   ' TO W-IDLEVNR-ALPHA  END-IF                           
255000     IF W-IDLEVNR-ALPHA = 'S69YA'                                         
255100        MOVE '19   ' TO W-IDLEVNR-ALPHA  END-IF                           
255200     IF W-IDLEVNR-ALPHA = 'BLJ6A'                                         
255300        MOVE '177  ' TO W-IDLEVNR-ALPHA  END-IF                           
255400     IF W-IDLEVNR-ALPHA = 'BQ6HA'                                         
255500        MOVE '527  ' TO W-IDLEVNR-ALPHA  END-IF                           
255600     IF W-IDLEVNR-ALPHA = 'AY0MA'                                         
255700        MOVE '745  ' TO W-IDLEVNR-ALPHA  END-IF                           
255800     IF W-IDLEVNR-ALPHA = 'BLZWA'                                         
255900        MOVE '870  ' TO W-IDLEVNR-ALPHA  END-IF                           
256000     IF W-IDLEVNR-ALPHA = 'BQ2BA'                                         
256100        MOVE '1049 ' TO W-IDLEVNR-ALPHA  END-IF                           
256200     IF W-IDLEVNR-ALPHA = 'CFNJA'                                         
256300        MOVE '1456 ' TO W-IDLEVNR-ALPHA  END-IF                           
256400     IF W-IDLEVNR-ALPHA = 'CN4VA'                                         
256500        MOVE '1787 ' TO W-IDLEVNR-ALPHA  END-IF                           
256600     IF W-IDLEVNR-ALPHA = 'BQ3XA'                                         
256700        MOVE '1865 ' TO W-IDLEVNR-ALPHA  END-IF                           
256800     IF W-IDLEVNR-ALPHA = 'CFTNA'                                         
256900        MOVE '2054 ' TO W-IDLEVNR-ALPHA  END-IF                           
257000     IF W-IDLEVNR-ALPHA = 'BX9LA'                                         
257100        MOVE '2103 ' TO W-IDLEVNR-ALPHA  END-IF                           
257200     IF W-IDLEVNR-ALPHA = 'S6R1A'                                         
257300        MOVE '2261 ' TO W-IDLEVNR-ALPHA  END-IF                           
257400     IF W-IDLEVNR-ALPHA = 'BJ6HA'                                         
257500        MOVE '2406 ' TO W-IDLEVNR-ALPHA  END-IF                           
257600     IF W-IDLEVNR-ALPHA = 'BQ5WA'                                         
257700        MOVE '2417 ' TO W-IDLEVNR-ALPHA  END-IF                           
257800     IF W-IDLEVNR-ALPHA = 'BQ5XA'                                         
257900        MOVE '2423 ' TO W-IDLEVNR-ALPHA  END-IF                           
258000     IF W-IDLEVNR-ALPHA = 'CFNUA'                                         
258100        MOVE '2457 ' TO W-IDLEVNR-ALPHA  END-IF                           
258200     IF W-IDLEVNR-ALPHA = 'BYL8A'                                         
258300        MOVE '2490 ' TO W-IDLEVNR-ALPHA  END-IF                           
258400     IF W-IDLEVNR-ALPHA = 'DL1WA'                                         
258500        MOVE '3404 ' TO W-IDLEVNR-ALPHA  END-IF                           
258600     IF W-IDLEVNR-ALPHA = 'K4STA'                                         
258700        MOVE '3572 ' TO W-IDLEVNR-ALPHA  END-IF                           
258800     IF W-IDLEVNR-ALPHA = 'A0VWA'                                         
258900        MOVE '3747 ' TO W-IDLEVNR-ALPHA  END-IF                           
259000     IF W-IDLEVNR-ALPHA = 'R39QA'                                         
259100        MOVE '3815 ' TO W-IDLEVNR-ALPHA  END-IF                           
259200     IF W-IDLEVNR-ALPHA = 'DLJLA'                                         
259300        MOVE '3898 ' TO W-IDLEVNR-ALPHA  END-IF                           
259400     IF W-IDLEVNR-ALPHA = 'BWMAA'                                         
259500        MOVE '3902 ' TO W-IDLEVNR-ALPHA  END-IF                           
259600     IF W-IDLEVNR-ALPHA = 'CQPSA'                                         
259700        MOVE '3918 ' TO W-IDLEVNR-ALPHA  END-IF                           
259800     IF W-IDLEVNR-ALPHA = 'K3D7B'                                         
259900        MOVE '3965 ' TO W-IDLEVNR-ALPHA  END-IF                           
260000     IF W-IDLEVNR-ALPHA = 'G273T'                                         
260100        MOVE '4164 ' TO W-IDLEVNR-ALPHA  END-IF                           
260200     IF W-IDLEVNR-ALPHA = 'G255C'                                         
260300        MOVE '4239 ' TO W-IDLEVNR-ALPHA  END-IF                           
260400     IF W-IDLEVNR-ALPHA = 'C212A'                                         
260500        MOVE '4274 ' TO W-IDLEVNR-ALPHA  END-IF                           
260600     IF W-IDLEVNR-ALPHA = 'EQ17A'                                         
260700        MOVE '4344 ' TO W-IDLEVNR-ALPHA  END-IF                           
260800     IF W-IDLEVNR-ALPHA = 'CN5BA'                                         
260900        MOVE '4528 ' TO W-IDLEVNR-ALPHA  END-IF                           
261000     IF W-IDLEVNR-ALPHA = 'C8Q0A'                                         
261100        MOVE '4955 ' TO W-IDLEVNR-ALPHA  END-IF                           
261200     IF W-IDLEVNR-ALPHA = 'K817J'                                         
261300        MOVE '4994 ' TO W-IDLEVNR-ALPHA  END-IF                           
261400     IF W-IDLEVNR-ALPHA = 'B40QG'                                         
261500        MOVE '5019 ' TO W-IDLEVNR-ALPHA  END-IF                           
261600     IF W-IDLEVNR-ALPHA = 'C7G4A'                                         
261700        MOVE '5051 ' TO W-IDLEVNR-ALPHA  END-IF                           
261800     IF W-IDLEVNR-ALPHA = 'C9A2A'                                         
261900        MOVE '5135 ' TO W-IDLEVNR-ALPHA  END-IF                           
262000     IF W-IDLEVNR-ALPHA = 'CFN4A'                                         
262100        MOVE '5287 ' TO W-IDLEVNR-ALPHA  END-IF                           
262200     IF W-IDLEVNR-ALPHA = 'P8D3A'                                         
262300        MOVE '5595 ' TO W-IDLEVNR-ALPHA  END-IF                           
262400     IF W-IDLEVNR-ALPHA = 'F962A'                                         
262500        MOVE '6026 ' TO W-IDLEVNR-ALPHA  END-IF                           
262600     IF W-IDLEVNR-ALPHA = 'DL6KA'                                         
262700        MOVE '6163 ' TO W-IDLEVNR-ALPHA  END-IF                           
262800     IF W-IDLEVNR-ALPHA = 'D0SAA'                                         
262900        MOVE '6235 ' TO W-IDLEVNR-ALPHA  END-IF                           
263000     IF W-IDLEVNR-ALPHA = 'DLLEA'                                         
263100        MOVE '6296 ' TO W-IDLEVNR-ALPHA  END-IF                           
263200     IF W-IDLEVNR-ALPHA = 'BX6BC'                                         
263300        MOVE '6335 ' TO W-IDLEVNR-ALPHA  END-IF                           
263400     IF W-IDLEVNR-ALPHA = 'CUQLA'                                         
263500        MOVE '6584 ' TO W-IDLEVNR-ALPHA  END-IF                           
263600     IF W-IDLEVNR-ALPHA = 'T1X5A'                                         
263700        MOVE '6838 ' TO W-IDLEVNR-ALPHA  END-IF                           
263800     IF W-IDLEVNR-ALPHA = 'D3R7A'                                         
263900        MOVE '6908 ' TO W-IDLEVNR-ALPHA  END-IF                           
264000     IF W-IDLEVNR-ALPHA = 'CFT1A'                                         
264100        MOVE '7203 ' TO W-IDLEVNR-ALPHA  END-IF                           
264200     IF W-IDLEVNR-ALPHA = 'K8XJA'                                         
264300        MOVE '7231 ' TO W-IDLEVNR-ALPHA  END-IF                           
264400     IF W-IDLEVNR-ALPHA = 'BP3JA'                                         
264500        MOVE '7367 ' TO W-IDLEVNR-ALPHA  END-IF                           
264600     IF W-IDLEVNR-ALPHA = 'DT9EA'                                         
264700        MOVE '7662 ' TO W-IDLEVNR-ALPHA  END-IF                           
264800     IF W-IDLEVNR-ALPHA = 'BH6NA'                                         
264900        MOVE '7724 ' TO W-IDLEVNR-ALPHA  END-IF                           
265000     IF W-IDLEVNR-ALPHA = 'CKSMA'                                         
265100        MOVE '7767 ' TO W-IDLEVNR-ALPHA  END-IF                           
265200     IF W-IDLEVNR-ALPHA = 'BQ8GA'                                         
265300        MOVE '7786 ' TO W-IDLEVNR-ALPHA  END-IF                           
265400     IF W-IDLEVNR-ALPHA = 'DLLSA'                                         
265500        MOVE '7821 ' TO W-IDLEVNR-ALPHA  END-IF                           
265600     IF W-IDLEVNR-ALPHA = 'DBF3A'                                         
265700        MOVE '7956 ' TO W-IDLEVNR-ALPHA  END-IF                           
265800     IF W-IDLEVNR-ALPHA = 'H222A'                                         
265900        MOVE '7961 ' TO W-IDLEVNR-ALPHA  END-IF                           
266000     IF W-IDLEVNR-ALPHA = 'P1NQA'                                         
266100        MOVE '8204 ' TO W-IDLEVNR-ALPHA  END-IF                           
266200     IF W-IDLEVNR-ALPHA = 'CFNNA'                                         
266300        MOVE '10105' TO W-IDLEVNR-ALPHA  END-IF                           
266400     IF W-IDLEVNR-ALPHA = 'CN5LA'                                         
266500        MOVE '10141' TO W-IDLEVNR-ALPHA  END-IF                           
266600     IF W-IDLEVNR-ALPHA = 'L8K5R'                                         
266700        MOVE '10178' TO W-IDLEVNR-ALPHA  END-IF                           
266800     IF W-IDLEVNR-ALPHA = 'BRV3A'                                         
266900        MOVE '10339' TO W-IDLEVNR-ALPHA  END-IF                           
267000     IF W-IDLEVNR-ALPHA = 'L8K5G'                                         
267100        MOVE '10342' TO W-IDLEVNR-ALPHA  END-IF                           
267200     IF W-IDLEVNR-ALPHA = 'U7PMA'                                         
267300        MOVE '10986' TO W-IDLEVNR-ALPHA  END-IF                           
267400     IF W-IDLEVNR-ALPHA = 'E1P5A'                                         
267500        MOVE '11578' TO W-IDLEVNR-ALPHA  END-IF                           
267600     IF W-IDLEVNR-ALPHA = 'BQ9EA'                                         
267700        MOVE '12569' TO W-IDLEVNR-ALPHA  END-IF                           
267800     IF W-IDLEVNR-ALPHA = 'T655C'                                         
267900        MOVE '13011' TO W-IDLEVNR-ALPHA  END-IF                           
268000     IF W-IDLEVNR-ALPHA = 'D3U6A'                                         
268100        MOVE '13386' TO W-IDLEVNR-ALPHA  END-IF                           
268200     IF W-IDLEVNR-ALPHA = 'S4LBA'                                         
268300        MOVE '13391' TO W-IDLEVNR-ALPHA  END-IF                           
268400     IF W-IDLEVNR-ALPHA = 'DPVGA'                                         
268500        MOVE '13486' TO W-IDLEVNR-ALPHA  END-IF                           
268600     IF W-IDLEVNR-ALPHA = 'BQ9HB'                                         
268700        MOVE '13516' TO W-IDLEVNR-ALPHA  END-IF                           
268800     IF W-IDLEVNR-ALPHA = 'M9TPB'                                         
268900        MOVE '13556' TO W-IDLEVNR-ALPHA  END-IF                           
269000     IF W-IDLEVNR-ALPHA = 'BN7SA'                                         
269100        MOVE '13598' TO W-IDLEVNR-ALPHA  END-IF                           
269200     IF W-IDLEVNR-ALPHA = 'BQ9LA'                                         
269300        MOVE '13600' TO W-IDLEVNR-ALPHA  END-IF                           
269400     IF W-IDLEVNR-ALPHA = 'BQ9HC'                                         
269500        MOVE '13661' TO W-IDLEVNR-ALPHA  END-IF                           
269600     IF W-IDLEVNR-ALPHA = 'V4FWA'                                         
269700        MOVE '14313' TO W-IDLEVNR-ALPHA  END-IF                           
269800     IF W-IDLEVNR-ALPHA = 'D45NA'                                         
269900        MOVE '14602' TO W-IDLEVNR-ALPHA  END-IF                           
270000     IF W-IDLEVNR-ALPHA = 'L8K5A'                                         
270100        MOVE '14605' TO W-IDLEVNR-ALPHA  END-IF                           
270200     IF W-IDLEVNR-ALPHA = 'D26YB'                                         
270300        MOVE '15580' TO W-IDLEVNR-ALPHA  END-IF                           
270400     IF W-IDLEVNR-ALPHA = 'BTKBA'                                         
270500        MOVE '16048' TO W-IDLEVNR-ALPHA  END-IF                           
270600     IF W-IDLEVNR-ALPHA = 'S13SA'                                         
270700        MOVE '16058' TO W-IDLEVNR-ALPHA  END-IF                           
270800     IF W-IDLEVNR-ALPHA = 'E510F'                                         
270900        MOVE '16132' TO W-IDLEVNR-ALPHA  END-IF                           
271000     IF W-IDLEVNR-ALPHA = 'R7B3A'                                         
271100        MOVE '16158' TO W-IDLEVNR-ALPHA  END-IF                           
271200     IF W-IDLEVNR-ALPHA = 'CJ0GA'                                         
271300        MOVE '16251' TO W-IDLEVNR-ALPHA  END-IF                           
271400     IF W-IDLEVNR-ALPHA = 'S1YHA'                                         
271500        MOVE '16267' TO W-IDLEVNR-ALPHA  END-IF                           
271600     IF W-IDLEVNR-ALPHA = 'BQ9UA'                                         
271700        MOVE '16325' TO W-IDLEVNR-ALPHA  END-IF                           
271800     IF W-IDLEVNR-ALPHA = 'T446A'                                         
271900        MOVE '16470' TO W-IDLEVNR-ALPHA  END-IF                           
272000     IF W-IDLEVNR-ALPHA = 'B44XE'                                         
272100        MOVE '16490' TO W-IDLEVNR-ALPHA  END-IF                           
272200     IF W-IDLEVNR-ALPHA = 'BRV3B'                                         
272300        MOVE '17712' TO W-IDLEVNR-ALPHA  END-IF                           
272400     IF W-IDLEVNR-ALPHA = 'CN5QA'                                         
272500        MOVE '17713' TO W-IDLEVNR-ALPHA  END-IF                           
272600     IF W-IDLEVNR-ALPHA = 'CN5SA'                                         
272700        MOVE '17819' TO W-IDLEVNR-ALPHA  END-IF                           
272800     IF W-IDLEVNR-ALPHA = 'CW5YA'                                         
272900        MOVE '17819' TO W-IDLEVNR-ALPHA  END-IF                           
273000     IF W-IDLEVNR-ALPHA = 'DSHRA'                                         
273100        MOVE '18971' TO W-IDLEVNR-ALPHA  END-IF                           
273200     IF W-IDLEVNR-ALPHA = 'C9T2C'                                         
273300        MOVE '18997' TO W-IDLEVNR-ALPHA  END-IF                           
273400     IF W-IDLEVNR-ALPHA = 'AA8SA'                                         
273500        MOVE '19892' TO W-IDLEVNR-ALPHA  END-IF                           
273600     IF W-IDLEVNR-ALPHA = 'F641A'                                         
273700        MOVE '23093' TO W-IDLEVNR-ALPHA  END-IF                           
273800     IF W-IDLEVNR-ALPHA = 'DLNVA'                                         
273900        MOVE '23685' TO W-IDLEVNR-ALPHA  END-IF                           
274000     IF W-IDLEVNR-ALPHA = 'P108C'                                         
274100        MOVE '24263' TO W-IDLEVNR-ALPHA  END-IF                           
274200     IF W-IDLEVNR-ALPHA = 'CT3JA'                                         
274300        MOVE '24521' TO W-IDLEVNR-ALPHA  END-IF                           
274400     IF W-IDLEVNR-ALPHA = 'CW0HA'                                         
274500        MOVE '24653' TO W-IDLEVNR-ALPHA  END-IF                           
274600     IF W-IDLEVNR-ALPHA = 'CVVEA'                                         
274700        MOVE '24789' TO W-IDLEVNR-ALPHA  END-IF                           
274800     IF W-IDLEVNR-ALPHA = 'S5U0B'                                         
274900        MOVE '24896' TO W-IDLEVNR-ALPHA  END-IF                           
275000     IF W-IDLEVNR-ALPHA = 'CYSZB'                                         
275100        MOVE '24921' TO W-IDLEVNR-ALPHA  END-IF                           
275200     IF W-IDLEVNR-ALPHA = 'C72GA'                                         
275300        MOVE '24999' TO W-IDLEVNR-ALPHA  END-IF                           
275400     IF W-IDLEVNR-ALPHA = 'CYFWA'                                         
275500        MOVE '25286' TO W-IDLEVNR-ALPHA  END-IF                           
275600     IF W-IDLEVNR-ALPHA = 'BKL3A'                                         
275700        MOVE '25402' TO W-IDLEVNR-ALPHA  END-IF                           
275800     IF W-IDLEVNR-ALPHA = 'CZSEB'                                         
275900        MOVE '25620' TO W-IDLEVNR-ALPHA  END-IF                           
276000     IF W-IDLEVNR-ALPHA = 'DBBTA'                                         
276100        MOVE '25755' TO W-IDLEVNR-ALPHA  END-IF                           
276200     IF W-IDLEVNR-ALPHA = 'DA9EA'                                         
276300        MOVE '25765' TO W-IDLEVNR-ALPHA  END-IF                           
276400     IF W-IDLEVNR-ALPHA = 'JWMJA'                                         
276500        MOVE '25770' TO W-IDLEVNR-ALPHA  END-IF                           
276600     IF W-IDLEVNR-ALPHA = 'CYDAB'                                         
276700        MOVE '25797' TO W-IDLEVNR-ALPHA  END-IF                           
276800     IF W-IDLEVNR-ALPHA = 'EQ62A'                                         
276900        MOVE '25808' TO W-IDLEVNR-ALPHA  END-IF                           
277000     IF W-IDLEVNR-ALPHA = 'DDA4A'                                         
277100        MOVE '25810' TO W-IDLEVNR-ALPHA  END-IF                           
277200     IF W-IDLEVNR-ALPHA = 'DDETA'                                         
277300        MOVE '25818' TO W-IDLEVNR-ALPHA  END-IF                           
277400     IF W-IDLEVNR-ALPHA = 'V2NYA'                                         
277500        MOVE '25847' TO W-IDLEVNR-ALPHA  END-IF                           
277600     IF W-IDLEVNR-ALPHA = 'T655D'                                         
277700        MOVE '25859' TO W-IDLEVNR-ALPHA  END-IF                           
277800     IF W-IDLEVNR-ALPHA = 'G261S'                                         
277900        MOVE '25869' TO W-IDLEVNR-ALPHA  END-IF                           
278000     IF W-IDLEVNR-ALPHA = 'DH0RA'                                         
278100        MOVE '25870' TO W-IDLEVNR-ALPHA  END-IF                           
278200     IF W-IDLEVNR-ALPHA = 'BNWSE'                                         
278300        MOVE '25972' TO W-IDLEVNR-ALPHA  END-IF                           
278400     IF W-IDLEVNR-ALPHA = 'DRTHA'                                         
278500        MOVE '25996' TO W-IDLEVNR-ALPHA  END-IF                           
278600     IF W-IDLEVNR-ALPHA = 'BP8HA'                                         
278700        MOVE '34621' TO W-IDLEVNR-ALPHA  END-IF                           
278800     IF W-IDLEVNR-ALPHA = 'U9JXA'                                         
278900        MOVE '51065' TO W-IDLEVNR-ALPHA  END-IF                           
279000     IF W-IDLEVNR-ALPHA = 'A405A'                                         
279100        MOVE '51103' TO W-IDLEVNR-ALPHA  END-IF                           
279200     IF W-IDLEVNR-ALPHA = 'T733E'                                         
279300        MOVE '51568' TO W-IDLEVNR-ALPHA  END-IF                           
279400     IF W-IDLEVNR-ALPHA = 'F903H'                                         
279500        MOVE '62513' TO W-IDLEVNR-ALPHA  END-IF                           
279600     IF W-IDLEVNR-ALPHA = 'DCYQA'                                         
279700        MOVE '63300' TO W-IDLEVNR-ALPHA  END-IF                           
279800*************************************************                         
279900     IF W-IDLEVNR-ALPHA = 'C61MA'                                         
280000        MOVE '82   ' TO W-IDLEVNR-ALPHA  END-IF                           
280100     IF W-IDLEVNR-ALPHA = 'W064Z'                                         
280200        MOVE '4001 ' TO W-IDLEVNR-ALPHA  END-IF                           
280300     IF W-IDLEVNR-ALPHA = 'F260B'                                         
280400        MOVE '4038 ' TO W-IDLEVNR-ALPHA  END-IF                           
280500     IF W-IDLEVNR-ALPHA = 'BPQUA'                                         
280600        MOVE '4210 ' TO W-IDLEVNR-ALPHA  END-IF                           
280700     IF W-IDLEVNR-ALPHA = 'L217Q'                                         
280800        MOVE '4230 ' TO W-IDLEVNR-ALPHA  END-IF                           
280900     IF W-IDLEVNR-ALPHA = 'BPQUA'                                         
281000        MOVE '4280 ' TO W-IDLEVNR-ALPHA  END-IF                           
281100     IF W-IDLEVNR-ALPHA = 'BPGRA'                                         
281200        MOVE '4536 ' TO W-IDLEVNR-ALPHA  END-IF                           
281300     IF W-IDLEVNR-ALPHA = 'KFBGA'                                         
281400        MOVE '4667 ' TO W-IDLEVNR-ALPHA  END-IF                           
281500     IF W-IDLEVNR-ALPHA = 'D00MD'                                         
281600        MOVE '4921 ' TO W-IDLEVNR-ALPHA  END-IF                           
281700     IF W-IDLEVNR-ALPHA = 'D00MB'                                         
281800        MOVE '4930 ' TO W-IDLEVNR-ALPHA  END-IF                           
281900     IF W-IDLEVNR-ALPHA = 'D2T1E'                                         
282000        MOVE '6434 ' TO W-IDLEVNR-ALPHA  END-IF                           
282100     IF W-IDLEVNR-ALPHA = 'V1W4A'                                         
282200        MOVE '6969 ' TO W-IDLEVNR-ALPHA  END-IF                           
282300     IF W-IDLEVNR-ALPHA = 'J7TDB'                                         
282400        MOVE '7254 ' TO W-IDLEVNR-ALPHA  END-IF                           
282500     IF W-IDLEVNR-ALPHA = 'P981A'                                         
282600        MOVE '7994 ' TO W-IDLEVNR-ALPHA  END-IF                           
282700     IF W-IDLEVNR-ALPHA = 'TC12A'                                         
282800        MOVE '12101' TO W-IDLEVNR-ALPHA  END-IF                           
282900     IF W-IDLEVNR-ALPHA = 'BNWSB'                                         
283000        MOVE '13673' TO W-IDLEVNR-ALPHA  END-IF                           
283100     IF W-IDLEVNR-ALPHA = 'D00ME'                                         
283200        MOVE '14345' TO W-IDLEVNR-ALPHA  END-IF                           
283300     IF W-IDLEVNR-ALPHA = 'BQ6XB'                                         
283400        MOVE '14642' TO W-IDLEVNR-ALPHA  END-IF                           
283500     IF W-IDLEVNR-ALPHA = 'B468X'                                         
283600        MOVE '17064' TO W-IDLEVNR-ALPHA  END-IF                           
283700     IF W-IDLEVNR-ALPHA = 'B535B'                                         
283800        MOVE '18992' TO W-IDLEVNR-ALPHA  END-IF                           
283900     IF W-IDLEVNR-ALPHA = 'CQPPA'                                         
284000        MOVE '19018' TO W-IDLEVNR-ALPHA  END-IF                           
284100     IF W-IDLEVNR-ALPHA = 'T6A3A'                                         
284200        MOVE '23559' TO W-IDLEVNR-ALPHA  END-IF                           
284300     IF W-IDLEVNR-ALPHA = 'D00MF'                                         
284400        MOVE '23560' TO W-IDLEVNR-ALPHA  END-IF                           
284500     IF W-IDLEVNR-ALPHA = 'B40CC'                                         
284600        MOVE '23758' TO W-IDLEVNR-ALPHA  END-IF                           
284700     IF W-IDLEVNR-ALPHA = 'MLE6B'                                         
284800        MOVE '25832' TO W-IDLEVNR-ALPHA  END-IF                           
284900     IF W-IDLEVNR-ALPHA = 'D00MG'                                         
285000        MOVE '25925' TO W-IDLEVNR-ALPHA  END-IF                           
285100     IF W-IDLEVNR-ALPHA = 'D00MH'                                         
285200        MOVE '34345' TO W-IDLEVNR-ALPHA  END-IF                           
285300     IF W-IDLEVNR-ALPHA = 'R235B'                                         
285400        MOVE '50091' TO W-IDLEVNR-ALPHA  END-IF                           
285500     IF W-IDLEVNR-ALPHA = 'M617T'                                         
285600        MOVE '62385' TO W-IDLEVNR-ALPHA  END-IF                           
285700     IF W-IDLEVNR-ALPHA = 'R235G'                                         
285800        MOVE '63544' TO W-IDLEVNR-ALPHA  END-IF                           
285900*************************************************                         
286000     IF W-IDLEVNR-ALPHA = 'CQF3A'                                         
286100        MOVE '124  ' TO W-IDLEVNR-ALPHA  END-IF                           
286200     IF W-IDLEVNR-ALPHA = 'BJRAA'                                         
286300        MOVE '175  ' TO W-IDLEVNR-ALPHA  END-IF                           
286400     IF W-IDLEVNR-ALPHA = 'DFVDA'                                         
286500        MOVE '281  ' TO W-IDLEVNR-ALPHA  END-IF                           
286600     IF W-IDLEVNR-ALPHA = 'BJVLA'                                         
286700        MOVE '312  ' TO W-IDLEVNR-ALPHA  END-IF                           
286800     IF W-IDLEVNR-ALPHA = 'BQ1PA'                                         
286900        MOVE '966  ' TO W-IDLEVNR-ALPHA  END-IF                           
287000     IF W-IDLEVNR-ALPHA = 'S3DHA'                                         
287100        MOVE '1125 ' TO W-IDLEVNR-ALPHA  END-IF                           
287200     IF W-IDLEVNR-ALPHA = 'CFNGA'                                         
287300        MOVE '1235 ' TO W-IDLEVNR-ALPHA  END-IF                           
287400     IF W-IDLEVNR-ALPHA = 'CFNHA'                                         
287500        MOVE '1271 ' TO W-IDLEVNR-ALPHA  END-IF                           
287600     IF W-IDLEVNR-ALPHA = 'BKTVA'                                         
287700        MOVE '1495 ' TO W-IDLEVNR-ALPHA  END-IF                           
287800     IF W-IDLEVNR-ALPHA = 'CFJEA'                                         
287900        MOVE '1757 ' TO W-IDLEVNR-ALPHA  END-IF                           
288000     IF W-IDLEVNR-ALPHA = 'BYL2A'                                         
288100        MOVE '2001 ' TO W-IDLEVNR-ALPHA  END-IF                           
288200     IF W-IDLEVNR-ALPHA = 'CFTPA'                                         
288300        MOVE '2250 ' TO W-IDLEVNR-ALPHA  END-IF                           
288400     IF W-IDLEVNR-ALPHA = 'S3DHC'                                         
288500        MOVE '2429 ' TO W-IDLEVNR-ALPHA  END-IF                           
288600     IF W-IDLEVNR-ALPHA = 'BPUFA'                                         
288700        MOVE '2503 ' TO W-IDLEVNR-ALPHA  END-IF                           
288800     IF W-IDLEVNR-ALPHA = 'BKXQA'                                         
288900        MOVE '2650 ' TO W-IDLEVNR-ALPHA  END-IF                           
289000     IF W-IDLEVNR-ALPHA = 'CFFXA'                                         
289100        MOVE '3143 ' TO W-IDLEVNR-ALPHA  END-IF                           
289200     IF W-IDLEVNR-ALPHA = 'D1V4A'                                         
289300        MOVE '3312 ' TO W-IDLEVNR-ALPHA  END-IF                           
289400     IF W-IDLEVNR-ALPHA = 'L8K5V'                                         
289500        MOVE '3341 ' TO W-IDLEVNR-ALPHA  END-IF                           
289600     IF W-IDLEVNR-ALPHA = 'S552A'                                         
289700        MOVE '6074 ' TO W-IDLEVNR-ALPHA  END-IF                           
289800     IF W-IDLEVNR-ALPHA = 'T226F'                                         
289900        MOVE '6089 ' TO W-IDLEVNR-ALPHA  END-IF                           
290000     IF W-IDLEVNR-ALPHA = 'C8P5A'                                         
290100        MOVE '6670 ' TO W-IDLEVNR-ALPHA  END-IF                           
290200     IF W-IDLEVNR-ALPHA = 'JBA1A'                                         
290300        MOVE '6745 ' TO W-IDLEVNR-ALPHA  END-IF                           
290400     IF W-IDLEVNR-ALPHA = 'BP7YA'                                         
290500        MOVE '7500 ' TO W-IDLEVNR-ALPHA  END-IF                           
290600     IF W-IDLEVNR-ALPHA = 'CY7ZA'                                         
290700        MOVE '7923 ' TO W-IDLEVNR-ALPHA  END-IF                           
290800     IF W-IDLEVNR-ALPHA = 'BJVHA'                                         
290900        MOVE '10057' TO W-IDLEVNR-ALPHA  END-IF                           
291000     IF W-IDLEVNR-ALPHA = 'D36ZA'                                         
291100        MOVE '10947' TO W-IDLEVNR-ALPHA  END-IF                           
291200     IF W-IDLEVNR-ALPHA = 'J3BDA'                                         
291300        MOVE '14922' TO W-IDLEVNR-ALPHA  END-IF                           
291400     IF W-IDLEVNR-ALPHA = 'J6R3A'                                         
291500        MOVE '14990' TO W-IDLEVNR-ALPHA  END-IF                           
291600     IF W-IDLEVNR-ALPHA = 'C0VAH'                                         
291700        MOVE '15284' TO W-IDLEVNR-ALPHA  END-IF                           
291800     IF W-IDLEVNR-ALPHA = 'C8P5C'                                         
291900        MOVE '16037' TO W-IDLEVNR-ALPHA  END-IF                           
292000     IF W-IDLEVNR-ALPHA = 'C8P5D'                                         
292100        MOVE '16179' TO W-IDLEVNR-ALPHA  END-IF                           
292200     IF W-IDLEVNR-ALPHA = 'KTY7B'                                         
292300        MOVE '16336' TO W-IDLEVNR-ALPHA  END-IF                           
292400     IF W-IDLEVNR-ALPHA = 'C8P5J'                                         
292500        MOVE '16372' TO W-IDLEVNR-ALPHA  END-IF                           
292600     IF W-IDLEVNR-ALPHA = 'DR7TA'                                         
292700        MOVE '19206' TO W-IDLEVNR-ALPHA  END-IF                           
292800     IF W-IDLEVNR-ALPHA = 'C9H7A'                                         
292900        MOVE '19611' TO W-IDLEVNR-ALPHA  END-IF                           
293000     IF W-IDLEVNR-ALPHA = 'BXMZA'                                         
293100        MOVE '19914' TO W-IDLEVNR-ALPHA  END-IF                           
293200     IF W-IDLEVNR-ALPHA = 'C72DC'                                         
293300        MOVE '22292' TO W-IDLEVNR-ALPHA  END-IF                           
293400     IF W-IDLEVNR-ALPHA = 'A0VWC'                                         
293500        MOVE '23934' TO W-IDLEVNR-ALPHA  END-IF                           
293600     IF W-IDLEVNR-ALPHA = 'BQ9VB'                                         
293700        MOVE '24081' TO W-IDLEVNR-ALPHA  END-IF                           
293800     IF W-IDLEVNR-ALPHA = 'AJFWA'                                         
293900        MOVE '25072' TO W-IDLEVNR-ALPHA  END-IF                           
294000     IF W-IDLEVNR-ALPHA = 'AJFXA'                                         
294100        MOVE '25073' TO W-IDLEVNR-ALPHA  END-IF                           
294200     IF W-IDLEVNR-ALPHA = 'D2L4A'                                         
294300        MOVE '25082' TO W-IDLEVNR-ALPHA  END-IF                           
294400     IF W-IDLEVNR-ALPHA = 'K261A'                                         
294500        MOVE '25682' TO W-IDLEVNR-ALPHA  END-IF                           
294600     IF W-IDLEVNR-ALPHA = 'A76VE'                                         
294700        MOVE '25817' TO W-IDLEVNR-ALPHA  END-IF                           
294800     IF W-IDLEVNR-ALPHA = 'D36ZC'                                         
294900        MOVE '25826' TO W-IDLEVNR-ALPHA  END-IF                           
295000     IF W-IDLEVNR-ALPHA = 'DSDLA'                                         
295100        MOVE '25871' TO W-IDLEVNR-ALPHA  END-IF                           
295200     IF W-IDLEVNR-ALPHA = 'Q9KNA'                                         
295300        MOVE '25967' TO W-IDLEVNR-ALPHA  END-IF                           
295400     IF W-IDLEVNR-ALPHA = 'L2HWG'                                         
295500        MOVE '63609' TO W-IDLEVNR-ALPHA  END-IF                           
295600*************************************************                         
295700*************************************************                         
295800     IF W-IDLEVNR-ALPHA = 'BPGTB'                                         
295900        MOVE '211  ' TO W-IDLEVNR-ALPHA  END-IF                           
296000     IF W-IDLEVNR-ALPHA = 'BPGTA'                                         
296100        MOVE '1826 ' TO W-IDLEVNR-ALPHA  END-IF                           
296200     IF W-IDLEVNR-ALPHA = 'AD1YA'                                         
296300        MOVE '3469 ' TO W-IDLEVNR-ALPHA  END-IF                           
296400     IF W-IDLEVNR-ALPHA = 'L8K5U'                                         
296500        MOVE '3471 ' TO W-IDLEVNR-ALPHA  END-IF                           
296600     IF W-IDLEVNR-ALPHA = 'G8UTB'                                         
296700        MOVE '3494 ' TO W-IDLEVNR-ALPHA  END-IF                           
296800     IF W-IDLEVNR-ALPHA = 'MCSQA'                                         
296900        MOVE '3767 ' TO W-IDLEVNR-ALPHA  END-IF                           
297000     IF W-IDLEVNR-ALPHA = 'LNAPA'                                         
297100        MOVE '4540 ' TO W-IDLEVNR-ALPHA  END-IF                           
297200     IF W-IDLEVNR-ALPHA = 'LJVZA'                                         
297300        MOVE '4730 ' TO W-IDLEVNR-ALPHA  END-IF                           
297400     IF W-IDLEVNR-ALPHA = 'DL5LA'                                         
297500        MOVE '4762 ' TO W-IDLEVNR-ALPHA  END-IF                           
297600     IF W-IDLEVNR-ALPHA = 'DDE2A'                                         
297700        MOVE '4790 ' TO W-IDLEVNR-ALPHA  END-IF                           
297800     IF W-IDLEVNR-ALPHA = 'F636B'                                         
297900        MOVE '5154 ' TO W-IDLEVNR-ALPHA  END-IF                           
298000     IF W-IDLEVNR-ALPHA = 'Q33YA'                                         
298100        MOVE '5220 ' TO W-IDLEVNR-ALPHA  END-IF                           
298200     IF W-IDLEVNR-ALPHA = 'DGDKA'                                         
298300        MOVE '5239 ' TO W-IDLEVNR-ALPHA  END-IF                           
298400     IF W-IDLEVNR-ALPHA = 'B517X'                                         
298500        MOVE '5419 ' TO W-IDLEVNR-ALPHA  END-IF                           
298600     IF W-IDLEVNR-ALPHA = 'BRGHA'                                         
298700        MOVE '5614 ' TO W-IDLEVNR-ALPHA  END-IF                           
298800     IF W-IDLEVNR-ALPHA = 'C89GA'                                         
298900        MOVE '5664 ' TO W-IDLEVNR-ALPHA  END-IF                           
299000     IF W-IDLEVNR-ALPHA = 'CKBZA'                                         
299100        MOVE '6133 ' TO W-IDLEVNR-ALPHA  END-IF                           
299200     IF W-IDLEVNR-ALPHA = 'DL6LA'                                         
299300        MOVE '6292 ' TO W-IDLEVNR-ALPHA  END-IF                           
299400     IF W-IDLEVNR-ALPHA = 'D17MA'                                         
299500        MOVE '6363 ' TO W-IDLEVNR-ALPHA  END-IF                           
299600     IF W-IDLEVNR-ALPHA = 'E355B'                                         
299700        MOVE '6369 ' TO W-IDLEVNR-ALPHA  END-IF                           
299800     IF W-IDLEVNR-ALPHA = 'R1N9A'                                         
299900        MOVE '6552 ' TO W-IDLEVNR-ALPHA  END-IF                           
300000     IF W-IDLEVNR-ALPHA = 'M738A'                                         
300100        MOVE '6599 ' TO W-IDLEVNR-ALPHA  END-IF                           
300200     IF W-IDLEVNR-ALPHA = 'DLLMA'                                         
300300        MOVE '6677 ' TO W-IDLEVNR-ALPHA  END-IF                           
300400     IF W-IDLEVNR-ALPHA = 'B45GA'                                         
300500        MOVE '6748 ' TO W-IDLEVNR-ALPHA  END-IF                           
300600     IF W-IDLEVNR-ALPHA = 'D0FVB'                                         
300700        MOVE '7752 ' TO W-IDLEVNR-ALPHA  END-IF                           
300800     IF W-IDLEVNR-ALPHA = 'D0HHD'                                         
300900        MOVE '7800 ' TO W-IDLEVNR-ALPHA  END-IF                           
301000     IF W-IDLEVNR-ALPHA = 'BPTQB'                                         
301100        MOVE '10157' TO W-IDLEVNR-ALPHA  END-IF                           
301200     IF W-IDLEVNR-ALPHA = 'BPTQA'                                         
301300        MOVE '10158' TO W-IDLEVNR-ALPHA  END-IF                           
301400     IF W-IDLEVNR-ALPHA = 'DL7EA'                                         
301500        MOVE '10356' TO W-IDLEVNR-ALPHA  END-IF                           
301600     IF W-IDLEVNR-ALPHA = 'CT3KA'                                         
301700        MOVE '10934' TO W-IDLEVNR-ALPHA  END-IF                           
301800     IF W-IDLEVNR-ALPHA = 'DL7KA'                                         
301900        MOVE '13349' TO W-IDLEVNR-ALPHA  END-IF                           
302000     IF W-IDLEVNR-ALPHA = 'DL7LA'                                         
302100        MOVE '13388' TO W-IDLEVNR-ALPHA  END-IF                           
302200     IF W-IDLEVNR-ALPHA = 'DLMNA'                                         
302300        MOVE '13396' TO W-IDLEVNR-ALPHA  END-IF                           
302400     IF W-IDLEVNR-ALPHA = 'DBQZA'                                         
302500        MOVE '13571' TO W-IDLEVNR-ALPHA  END-IF                           
302600     IF W-IDLEVNR-ALPHA = 'CRKZA'                                         
302700        MOVE '13572' TO W-IDLEVNR-ALPHA  END-IF                           
302800     IF W-IDLEVNR-ALPHA = 'DL7WA'                                         
302900        MOVE '13583' TO W-IDLEVNR-ALPHA  END-IF                           
303000     IF W-IDLEVNR-ALPHA = 'BQAEB'                                         
303100        MOVE '13717' TO W-IDLEVNR-ALPHA  END-IF                           
303200     IF W-IDLEVNR-ALPHA = 'Q5EGA'                                         
303300        MOVE '14947' TO W-IDLEVNR-ALPHA  END-IF                           
303400     IF W-IDLEVNR-ALPHA = 'Q89FA'                                         
303500        MOVE '16030' TO W-IDLEVNR-ALPHA  END-IF                           
303600     IF W-IDLEVNR-ALPHA = 'AQ2MB'                                         
303700        MOVE '16112' TO W-IDLEVNR-ALPHA  END-IF                           
303800     IF W-IDLEVNR-ALPHA = 'CNT5A'                                         
303900        MOVE '20522' TO W-IDLEVNR-ALPHA  END-IF                           
304000     IF W-IDLEVNR-ALPHA = 'BMZJA'                                         
304100        MOVE '24489' TO W-IDLEVNR-ALPHA  END-IF                           
304200     IF W-IDLEVNR-ALPHA = 'ATC2A'                                         
304300        MOVE '25089' TO W-IDLEVNR-ALPHA  END-IF                           
304400     IF W-IDLEVNR-ALPHA = 'MAXWA'                                         
304500        MOVE '25090' TO W-IDLEVNR-ALPHA  END-IF                           
304600     IF W-IDLEVNR-ALPHA = 'BL2CA'                                         
304700        MOVE '25412' TO W-IDLEVNR-ALPHA  END-IF                           
304800     IF W-IDLEVNR-ALPHA = 'M799G'                                         
304900        MOVE '25446' TO W-IDLEVNR-ALPHA  END-IF                           
305000     IF W-IDLEVNR-ALPHA = 'N2KUA'                                         
305100        MOVE '25828' TO W-IDLEVNR-ALPHA  END-IF                           
305200     IF W-IDLEVNR-ALPHA = 'D38ME'                                         
305300        MOVE '25878' TO W-IDLEVNR-ALPHA  END-IF                           
305400     IF W-IDLEVNR-ALPHA = 'D33BA'                                         
305500        MOVE '25910' TO W-IDLEVNR-ALPHA  END-IF                           
305600     IF W-IDLEVNR-ALPHA = 'BPTQD'                                         
305700        MOVE '25924' TO W-IDLEVNR-ALPHA  END-IF                           
305800     IF W-IDLEVNR-ALPHA = 'DPEWA'                                         
305900        MOVE '25962' TO W-IDLEVNR-ALPHA  END-IF                           
306000     IF W-IDLEVNR-ALPHA = 'DP5RA'                                         
306100        MOVE '25973' TO W-IDLEVNR-ALPHA  END-IF                           
306200     IF W-IDLEVNR-ALPHA = 'DSBYA'                                         
306300        MOVE '26009' TO W-IDLEVNR-ALPHA  END-IF                           
306400     IF W-IDLEVNR-ALPHA = 'CRX1D'                                         
306500        MOVE '26012' TO W-IDLEVNR-ALPHA  END-IF                           
306600     IF W-IDLEVNR-ALPHA = 'BKDDB'                                         
306700        MOVE '31014' TO W-IDLEVNR-ALPHA  END-IF                           
306800     IF W-IDLEVNR-ALPHA = 'BKDDC'                                         
306900        MOVE '41014' TO W-IDLEVNR-ALPHA  END-IF                           
307000*************************************************                         
307100**********************************************                            
307200     PERFORM S21-IDLEVNR-ALPHA-TO-NUM                                     
307300     MOVE W-IDLEVNR-NUM       TO UTVCOM1-IDLEVNR-NUM                      
307400     MOVE IN1-PRARTSJK        TO UTVCOM1-PRARTSJK                         
307500     MOVE IN1-PRARTSTD        TO UTVCOM1-PRARTSTD                         
307600     MOVE IN1-FLIART          TO UTVCOM1-FLIART                           
307700     MOVE IN1-IDPROJ          TO UTVCOM1-IDPROJ                           
307800     MOVE IN1-IDAO(1)         TO UTVCOM1-IDAO(1)                          
307900     MOVE IN1-IDAO(2)         TO UTVCOM1-IDAO(2)                          
308000     MOVE IN1-TIFINLEV        TO UTVCOM1-TIFINLEV                         
308100     MOVE IN1-IDANSK          TO UTVCOM1-IDANSK                           
308200     MOVE IN1-KVPB            TO UTVCOM1-KVPB                             
308300     MOVE IN1-KDVVKL          TO UTVCOM1-KDVVKL                           
308400     MOVE IN1-BELEVART        TO UTVCOM1-BELEVART                         
308500     MOVE IN1-KDTIPPR         TO UTVCOM1-KDTIPPR                          
308600     IF IN1-IDINK(1:1) NUMERIC                                            
308700       IF IN1-IDINK(1:3) NUMERIC                                          
308800         MOVE IN1-IDINK(1:3)      TO UTVCOM1-IDINK-OLD                    
308900       ELSE                                                               
309000         IF IN1-IDINK(1:2) NUMERIC                                        
309100           MOVE IN1-IDINK(1:2)    TO UTVCOM1-IDINK-OLD                    
309200         ELSE                                                             
309300           MOVE ZERO              TO UTVCOM1-IDINK-OLD                    
309400         END-IF                                                           
309500       END-IF                                                             
309600     ELSE                                                                 
309700       IF IN1-IDINK(2:3) NUMERIC                                          
309800         MOVE IN1-IDINK(2:3)      TO UTVCOM1-IDINK-OLD                    
309900       ELSE                                                               
310000         IF IN1-IDINK(2:2) NUMERIC                                        
310100           MOVE IN1-IDINK(2:2)    TO UTVCOM1-IDINK-OLD                    
310200         ELSE                                                             
310300           MOVE ZERO              TO UTVCOM1-IDINK-OLD                    
310400         END-IF                                                           
310500       END-IF                                                             
310600     END-IF                                                               
310700                                                                          
310800     MOVE IN1-IDINK           TO UTVCOM1-IDINK                            
310900     MOVE IN1-TIREGDAT        TO UTVCOM1-TIREGDAT                         
311000     MOVE IN1-KDUART          TO UTVCOM1-KDUART                           
311100     MOVE IN1-IDARTNR-MOTSV   TO UTVCOM1-IDARTNR-MOTSV                    
311200     MOVE IN1-PRINK           TO UTVCOM1-PRINK                            
311300     MOVE IN1-IDRITN          TO UTVCOM1-IDRITN                           
311400     MOVE IN1-PRHANTK         TO UTVCOM1-PRHANTK                          
311500     MOVE IN1-KDAGE           TO UTVCOM1-KDAGE                            
311600     MOVE IN1-KDPSLLOC        TO UTVCOM1-KDPSLLOC                         
311700     MOVE IN1-SLAG-IDLEVNR    TO UTVCOM1-SLAG-IDLEVNR                     
311800                                 W-IDLEVNR-ALPHA                          
311900     MOVE IN1-IDKAT(1)        TO UTVCOM1-IDKAT(1)                         
312000     MOVE IN1-IDKAT(2)        TO UTVCOM1-IDKAT(2)                         
312100     MOVE IN1-IDKAT(3)        TO UTVCOM1-IDKAT(3)                         
312200     MOVE IN1-KDRAB           TO UTVCOM1-KDRAB                            
312300     MOVE IN1-PRARTBEL        TO UTVCOM1-PRARTBEL                         
312400     MOVE IN1-FLLSRDEL        TO UTVCOM1-FLLSRDEL                         
312500     MOVE IN1-IDPROJUP        TO UTVCOM1-IDPROJUP                         
312600     MOVE IN1-FLGEMFMC        TO UTVCOM1-FLGEMFMC                         
312610     MOVE IN1-TIURPROD        TO UTVCOM1-TIURPROD                         
312700****FIX FÖR ALFA LEVNR**************************                          
312800     IF W-IDLEVNR-ALPHA = 'BQ2QA'                                         
312900        MOVE '01385' TO W-IDLEVNR-ALPHA                                   
313000     END-IF                                                               
313100     IF W-IDLEVNR-ALPHA = 'D12YA'                                         
313200        MOVE '06414' TO W-IDLEVNR-ALPHA                                   
313300     END-IF                                                               
313400     IF W-IDLEVNR-ALPHA = 'D0KLA'                                         
313500        MOVE '06916' TO W-IDLEVNR-ALPHA                                   
313600     END-IF                                                               
313700**********************************************                            
313800*************************************************                         
313900     IF W-IDLEVNR-ALPHA = 'BJ7TB'                                         
314000        MOVE '10132' TO W-IDLEVNR-ALPHA  END-IF                           
314100     IF W-IDLEVNR-ALPHA = 'D6M4A'                                         
314200        MOVE '19064' TO W-IDLEVNR-ALPHA  END-IF                           
314300     IF W-IDLEVNR-ALPHA = 'BQCTA'                                         
314400        MOVE '19535' TO W-IDLEVNR-ALPHA  END-IF                           
314500     IF W-IDLEVNR-ALPHA = 'R14MA'                                         
314600        MOVE '19610' TO W-IDLEVNR-ALPHA  END-IF                           
314700     IF W-IDLEVNR-ALPHA = 'BQYGA'                                         
314800        MOVE '22   ' TO W-IDLEVNR-ALPHA  END-IF                           
314900     IF W-IDLEVNR-ALPHA = 'BLTLA'                                         
315000        MOVE '2591 ' TO W-IDLEVNR-ALPHA  END-IF                           
315100     IF W-IDLEVNR-ALPHA = 'F4P6B'                                         
315200        MOVE '4445 ' TO W-IDLEVNR-ALPHA  END-IF                           
315300     IF W-IDLEVNR-ALPHA = 'CN5CA'                                         
315400        MOVE '4893 ' TO W-IDLEVNR-ALPHA  END-IF                           
315500     IF W-IDLEVNR-ALPHA = 'BWMZA'                                         
315600        MOVE '7630 ' TO W-IDLEVNR-ALPHA  END-IF                           
315700     IF W-IDLEVNR-ALPHA = 'CFT2B'                                         
315800        MOVE '7949 ' TO W-IDLEVNR-ALPHA  END-IF                           
315900**********************************************                            
316000     IF W-IDLEVNR-ALPHA = 'BWS5A'                                         
316100        MOVE '132  ' TO W-IDLEVNR-ALPHA  END-IF                           
316200     IF W-IDLEVNR-ALPHA = 'BMBQA'                                         
316300        MOVE '226  ' TO W-IDLEVNR-ALPHA  END-IF                           
316400     IF W-IDLEVNR-ALPHA = 'BKFVA'                                         
316500        MOVE '346  ' TO W-IDLEVNR-ALPHA  END-IF                           
316600     IF W-IDLEVNR-ALPHA = 'BWS9A'                                         
316700        MOVE '350  ' TO W-IDLEVNR-ALPHA  END-IF                           
316800     IF W-IDLEVNR-ALPHA = 'R9KSA'                                         
316900        MOVE '500  ' TO W-IDLEVNR-ALPHA  END-IF                           
317000     IF W-IDLEVNR-ALPHA = 'BQ2MA'                                         
317100        MOVE '1362 ' TO W-IDLEVNR-ALPHA  END-IF                           
317200     IF W-IDLEVNR-ALPHA = 'BQ2RA'                                         
317300        MOVE '1389 ' TO W-IDLEVNR-ALPHA  END-IF                           
317400     IF W-IDLEVNR-ALPHA = 'BPW9A'                                         
317500        MOVE '1425 ' TO W-IDLEVNR-ALPHA  END-IF                           
317600     IF W-IDLEVNR-ALPHA = 'DDDPA'                                         
317700        MOVE '1594 ' TO W-IDLEVNR-ALPHA  END-IF                           
317800     IF W-IDLEVNR-ALPHA = 'BQ3BA'                                         
317900        MOVE '1603 ' TO W-IDLEVNR-ALPHA  END-IF                           
318000     IF W-IDLEVNR-ALPHA = 'CFNKA'                                         
318100        MOVE '1675 ' TO W-IDLEVNR-ALPHA  END-IF                           
318200     IF W-IDLEVNR-ALPHA = 'MNDLA'                                         
318300        MOVE '2299 ' TO W-IDLEVNR-ALPHA  END-IF                           
318400     IF W-IDLEVNR-ALPHA = 'BQ6AA'                                         
318500        MOVE '2507 ' TO W-IDLEVNR-ALPHA  END-IF                           
318600     IF W-IDLEVNR-ALPHA = 'CFN0A'                                         
318700        MOVE '3664 ' TO W-IDLEVNR-ALPHA  END-IF                           
318800     IF W-IDLEVNR-ALPHA = 'CFN9A'                                         
318900        MOVE '3718 ' TO W-IDLEVNR-ALPHA  END-IF                           
319000     IF W-IDLEVNR-ALPHA = 'C96AA'                                         
319100        MOVE '5183 ' TO W-IDLEVNR-ALPHA  END-IF                           
319200     IF W-IDLEVNR-ALPHA = 'D0RED'                                         
319300        MOVE '6323 ' TO W-IDLEVNR-ALPHA  END-IF                           
319400     IF W-IDLEVNR-ALPHA = 'D0REB'                                         
319500        MOVE '6704 ' TO W-IDLEVNR-ALPHA  END-IF                           
319600     IF W-IDLEVNR-ALPHA = 'D0REA'                                         
319700        MOVE '6770 ' TO W-IDLEVNR-ALPHA  END-IF                           
319800     IF W-IDLEVNR-ALPHA = 'BUA7A'                                         
319900        MOVE '10108' TO W-IDLEVNR-ALPHA  END-IF                           
320000     IF W-IDLEVNR-ALPHA = 'AGPBA'                                         
320100        MOVE '14829' TO W-IDLEVNR-ALPHA  END-IF                           
320200*************************************************                         
320300     IF W-IDLEVNR-ALPHA = 'BYLRA'                                         
320400        MOVE '839  ' TO W-IDLEVNR-ALPHA  END-IF                           
320500     IF W-IDLEVNR-ALPHA = 'BQ2DA'                                         
320600        MOVE '1100 ' TO W-IDLEVNR-ALPHA  END-IF                           
320700     IF W-IDLEVNR-ALPHA = 'BKWRA'                                         
320800        MOVE '1205 ' TO W-IDLEVNR-ALPHA  END-IF                           
320900     IF W-IDLEVNR-ALPHA = 'BQ2HA'                                         
321000        MOVE '1285 ' TO W-IDLEVNR-ALPHA  END-IF                           
321100     IF W-IDLEVNR-ALPHA = 'N81NA'                                         
321200        MOVE '1336 ' TO W-IDLEVNR-ALPHA  END-IF                           
321300     IF W-IDLEVNR-ALPHA = 'S51YA'                                         
321400        MOVE '1605 ' TO W-IDLEVNR-ALPHA  END-IF                           
321500     IF W-IDLEVNR-ALPHA = 'AHTXA'                                         
321600        MOVE '3948 ' TO W-IDLEVNR-ALPHA  END-IF                           
321700     IF W-IDLEVNR-ALPHA = 'R500F'                                         
321800        MOVE '4034 ' TO W-IDLEVNR-ALPHA  END-IF                           
321900     IF W-IDLEVNR-ALPHA = 'K4UKA'                                         
322000        MOVE '4937 ' TO W-IDLEVNR-ALPHA  END-IF                           
322100     IF W-IDLEVNR-ALPHA = 'S3ULA'                                         
322200        MOVE '4979 ' TO W-IDLEVNR-ALPHA  END-IF                           
322300     IF W-IDLEVNR-ALPHA = 'C9F8A'                                         
322400        MOVE '5012 ' TO W-IDLEVNR-ALPHA  END-IF                           
322500     IF W-IDLEVNR-ALPHA = 'CFN1A'                                         
322600        MOVE '5145 ' TO W-IDLEVNR-ALPHA  END-IF                           
322700     IF W-IDLEVNR-ALPHA = 'BVNUA'                                         
322800        MOVE '5197 ' TO W-IDLEVNR-ALPHA  END-IF                           
322900     IF W-IDLEVNR-ALPHA = 'BQ7BA'                                         
323000        MOVE '5356 ' TO W-IDLEVNR-ALPHA  END-IF                           
323100     IF W-IDLEVNR-ALPHA = 'T727Z'                                         
323200        MOVE '5645 ' TO W-IDLEVNR-ALPHA  END-IF                           
323300     IF W-IDLEVNR-ALPHA = 'U494Q'                                         
323400        MOVE '5868 ' TO W-IDLEVNR-ALPHA  END-IF                           
323500     IF W-IDLEVNR-ALPHA = 'D35VA'                                         
323600        MOVE '6543 ' TO W-IDLEVNR-ALPHA  END-IF                           
323700     IF W-IDLEVNR-ALPHA = 'FNT8A'                                         
323800        MOVE '6985 ' TO W-IDLEVNR-ALPHA  END-IF                           
323900     IF W-IDLEVNR-ALPHA = 'CFT0A'                                         
324000        MOVE '7139 ' TO W-IDLEVNR-ALPHA  END-IF                           
324100     IF W-IDLEVNR-ALPHA = 'CFT8A'                                         
324200        MOVE '8187 ' TO W-IDLEVNR-ALPHA  END-IF                           
324300     IF W-IDLEVNR-ALPHA = 'K4UKB'                                         
324400        MOVE '11577' TO W-IDLEVNR-ALPHA  END-IF                           
324500     IF W-IDLEVNR-ALPHA = 'H387D'                                         
324600        MOVE '14985' TO W-IDLEVNR-ALPHA  END-IF                           
324700     IF W-IDLEVNR-ALPHA = 'S12HA'                                         
324800        MOVE '16076' TO W-IDLEVNR-ALPHA  END-IF                           
324900     IF W-IDLEVNR-ALPHA = 'BPXJA'                                         
325000        MOVE '16265' TO W-IDLEVNR-ALPHA  END-IF                           
325100     IF W-IDLEVNR-ALPHA = 'CFT8B'                                         
325200        MOVE '22410' TO W-IDLEVNR-ALPHA  END-IF                           
325300     IF W-IDLEVNR-ALPHA = 'C9G6A'                                         
325400        MOVE '24030' TO W-IDLEVNR-ALPHA  END-IF                           
325500     IF W-IDLEVNR-ALPHA = 'S12HE'                                         
325600        MOVE '24331' TO W-IDLEVNR-ALPHA  END-IF                           
325700*************************************************                         
325800*************************************************                         
325900     IF W-IDLEVNR-ALPHA = 'BQ1FA'                                         
326000        MOVE '640  ' TO W-IDLEVNR-ALPHA  END-IF                           
326100     IF W-IDLEVNR-ALPHA = 'BK3DA'                                         
326200        MOVE '813  ' TO W-IDLEVNR-ALPHA  END-IF                           
326300     IF W-IDLEVNR-ALPHA = 'N81FA'                                         
326400        MOVE '836  ' TO W-IDLEVNR-ALPHA  END-IF                           
326500     IF W-IDLEVNR-ALPHA = 'BQ1JA'                                         
326600        MOVE '845  ' TO W-IDLEVNR-ALPHA  END-IF                           
326700     IF W-IDLEVNR-ALPHA = 'BQ1KA'                                         
326800        MOVE '850  ' TO W-IDLEVNR-ALPHA  END-IF                           
326900     IF W-IDLEVNR-ALPHA = 'BQ1LA'                                         
327000        MOVE '861  ' TO W-IDLEVNR-ALPHA  END-IF                           
327100     IF W-IDLEVNR-ALPHA = 'BHFCA'                                         
327200        MOVE '927  ' TO W-IDLEVNR-ALPHA  END-IF                           
327300     IF W-IDLEVNR-ALPHA = 'BKCKA'                                         
327400        MOVE '930  ' TO W-IDLEVNR-ALPHA  END-IF                           
327500     IF W-IDLEVNR-ALPHA = 'N81JA'                                         
327600        MOVE '933  ' TO W-IDLEVNR-ALPHA  END-IF                           
327700     IF W-IDLEVNR-ALPHA = 'BQ2XA'                                         
327800        MOVE '1326 ' TO W-IDLEVNR-ALPHA  END-IF                           
327900     IF W-IDLEVNR-ALPHA = 'BLU5A'                                         
328000        MOVE '1659 ' TO W-IDLEVNR-ALPHA  END-IF                           
328100     IF W-IDLEVNR-ALPHA = 'BQ2YA'                                         
328200        MOVE '1977 ' TO W-IDLEVNR-ALPHA  END-IF                           
328300     IF W-IDLEVNR-ALPHA = 'CN5GA'                                         
328400        MOVE '7186 ' TO W-IDLEVNR-ALPHA  END-IF                           
328500     IF W-IDLEVNR-ALPHA = 'D0FTA'                                         
328600        MOVE '7208 ' TO W-IDLEVNR-ALPHA  END-IF                           
328700     IF W-IDLEVNR-ALPHA = 'FLZ6B'                                         
328800        MOVE '16154' TO W-IDLEVNR-ALPHA  END-IF                           
328900     IF W-IDLEVNR-ALPHA = 'S9GAA'                                         
329000        MOVE '18086' TO W-IDLEVNR-ALPHA  END-IF                           
329100*************************************************                         
329200     IF W-IDLEVNR-ALPHA = 'BWTDA'                                         
329300        MOVE '548  ' TO W-IDLEVNR-ALPHA  END-IF                           
329400     IF W-IDLEVNR-ALPHA = 'BWTFA'                                         
329500        MOVE '598  ' TO W-IDLEVNR-ALPHA  END-IF                           
329600     IF W-IDLEVNR-ALPHA = 'BQ1CA'                                         
329700        MOVE '605  ' TO W-IDLEVNR-ALPHA  END-IF                           
329800     IF W-IDLEVNR-ALPHA = 'DBHJA'                                         
329900        MOVE '667  ' TO W-IDLEVNR-ALPHA  END-IF                           
330000     IF W-IDLEVNR-ALPHA = 'BQ1MA'                                         
330100        MOVE '890  ' TO W-IDLEVNR-ALPHA  END-IF                           
330200     IF W-IDLEVNR-ALPHA = 'CD2JA'                                         
330300        MOVE '897  ' TO W-IDLEVNR-ALPHA  END-IF                           
330400     IF W-IDLEVNR-ALPHA = 'BLRQA'                                         
330500        MOVE '929  ' TO W-IDLEVNR-ALPHA  END-IF                           
330600     IF W-IDLEVNR-ALPHA = 'BQ2PA'                                         
330700        MOVE '1380 ' TO W-IDLEVNR-ALPHA  END-IF                           
330800     IF W-IDLEVNR-ALPHA = 'BQ6QB'                                         
330900        MOVE '1386 ' TO W-IDLEVNR-ALPHA  END-IF                           
331000     IF W-IDLEVNR-ALPHA = 'BSBZA'                                         
331100        MOVE '1560 ' TO W-IDLEVNR-ALPHA  END-IF                           
331200     IF W-IDLEVNR-ALPHA = 'BQ3EA'                                         
331300        MOVE '1728 ' TO W-IDLEVNR-ALPHA  END-IF                           
331400     IF W-IDLEVNR-ALPHA = 'BQ3NA'                                         
331500        MOVE '1736 ' TO W-IDLEVNR-ALPHA  END-IF                           
331600     IF W-IDLEVNR-ALPHA = 'BQ8AA'                                         
331700        MOVE '1783 ' TO W-IDLEVNR-ALPHA  END-IF                           
331800     IF W-IDLEVNR-ALPHA = 'BK6ZA'                                         
331900        MOVE '1809 ' TO W-IDLEVNR-ALPHA  END-IF                           
332000     IF W-IDLEVNR-ALPHA = 'BQ5MA'                                         
332100        MOVE '1978 ' TO W-IDLEVNR-ALPHA  END-IF                           
332200     IF W-IDLEVNR-ALPHA = 'BQ5NA'                                         
332300        MOVE '1982 ' TO W-IDLEVNR-ALPHA  END-IF                           
332400     IF W-IDLEVNR-ALPHA = 'S52HA'                                         
332500        MOVE '2087 ' TO W-IDLEVNR-ALPHA  END-IF                           
332600     IF W-IDLEVNR-ALPHA = 'CFNYA'                                         
332700        MOVE '3380 ' TO W-IDLEVNR-ALPHA  END-IF                           
332800     IF W-IDLEVNR-ALPHA = 'E622D'                                         
332900        MOVE '3654 ' TO W-IDLEVNR-ALPHA  END-IF                           
333000     IF W-IDLEVNR-ALPHA = 'D3D4A'                                         
333100        MOVE '6881 ' TO W-IDLEVNR-ALPHA  END-IF                           
333200     IF W-IDLEVNR-ALPHA = 'BQ7YC'                                         
333300        MOVE '7314 ' TO W-IDLEVNR-ALPHA  END-IF                           
333400     IF W-IDLEVNR-ALPHA = 'BQ7YB'                                         
333500        MOVE '7369 ' TO W-IDLEVNR-ALPHA  END-IF                           
333600     IF W-IDLEVNR-ALPHA = 'BQ7YA'                                         
333700        MOVE '7420 ' TO W-IDLEVNR-ALPHA  END-IF                           
333800     IF W-IDLEVNR-ALPHA = 'BPVBC'                                         
333900        MOVE '7881 ' TO W-IDLEVNR-ALPHA  END-IF                           
334000     IF W-IDLEVNR-ALPHA = 'BPVBB'                                         
334100        MOVE '10096' TO W-IDLEVNR-ALPHA  END-IF                           
334200     IF W-IDLEVNR-ALPHA = 'CFNZB'                                         
334300        MOVE '13691' TO W-IDLEVNR-ALPHA  END-IF                           
334400     IF W-IDLEVNR-ALPHA = 'U7SAD'                                         
334500        MOVE '14996' TO W-IDLEVNR-ALPHA  END-IF                           
334600     IF W-IDLEVNR-ALPHA = 'AN3AB'                                         
334700        MOVE '16226' TO W-IDLEVNR-ALPHA  END-IF                           
334800     IF W-IDLEVNR-ALPHA = 'U910A'                                         
334900        MOVE '24719' TO W-IDLEVNR-ALPHA  END-IF                           
335000     IF W-IDLEVNR-ALPHA = 'J5C2B'                                         
335100        MOVE '25016' TO W-IDLEVNR-ALPHA  END-IF                           
335200     IF W-IDLEVNR-ALPHA = 'LMJGA'                                         
335300        MOVE '25676' TO W-IDLEVNR-ALPHA  END-IF                           
335400     IF W-IDLEVNR-ALPHA = 'BPVBA'                                         
335500        MOVE '13445' TO W-IDLEVNR-ALPHA  END-IF                           
335600*************************************************                         
335700*************************************************                         
335800     IF W-IDLEVNR-ALPHA = 'BQ8ZD'                                         
335900        MOVE '93   ' TO W-IDLEVNR-ALPHA  END-IF                           
336000     IF W-IDLEVNR-ALPHA = 'BQ9CB'                                         
336100        MOVE '234  ' TO W-IDLEVNR-ALPHA  END-IF                           
336200     IF W-IDLEVNR-ALPHA = 'J2A6B'                                         
336300        MOVE '386  ' TO W-IDLEVNR-ALPHA  END-IF                           
336400     IF W-IDLEVNR-ALPHA = 'BQ8ZB'                                         
336500        MOVE '607  ' TO W-IDLEVNR-ALPHA  END-IF                           
336600     IF W-IDLEVNR-ALPHA = 'CXC6A'                                         
336700        MOVE '780  ' TO W-IDLEVNR-ALPHA  END-IF                           
336800     IF W-IDLEVNR-ALPHA = 'C7Q2D'                                         
336900        MOVE '835  ' TO W-IDLEVNR-ALPHA  END-IF                           
337000     IF W-IDLEVNR-ALPHA = 'BQ9AA'                                         
337100        MOVE '1187 ' TO W-IDLEVNR-ALPHA  END-IF                           
337200     IF W-IDLEVNR-ALPHA = 'C7Q2B'                                         
337300        MOVE '1228 ' TO W-IDLEVNR-ALPHA  END-IF                           
337400     IF W-IDLEVNR-ALPHA = 'BQ9CC'                                         
337500        MOVE '1393 ' TO W-IDLEVNR-ALPHA  END-IF                           
337600     IF W-IDLEVNR-ALPHA = 'BPU0A'                                         
337700        MOVE '2065 ' TO W-IDLEVNR-ALPHA  END-IF                           
337800     IF W-IDLEVNR-ALPHA = 'BQ5PA'                                         
337900        MOVE '2108 ' TO W-IDLEVNR-ALPHA  END-IF                           
338000     IF W-IDLEVNR-ALPHA = 'BPU0B'                                         
338100        MOVE '2157 ' TO W-IDLEVNR-ALPHA  END-IF                           
338200     IF W-IDLEVNR-ALPHA = 'BKVVA'                                         
338300        MOVE '2220 ' TO W-IDLEVNR-ALPHA  END-IF                           
338400     IF W-IDLEVNR-ALPHA = 'J2A6A'                                         
338500        MOVE '2288 ' TO W-IDLEVNR-ALPHA  END-IF                           
338600     IF W-IDLEVNR-ALPHA = 'BX0ZA'                                         
338700        MOVE '2289 ' TO W-IDLEVNR-ALPHA  END-IF                           
338800     IF W-IDLEVNR-ALPHA = 'BQ5RA'                                         
338900        MOVE '2315 ' TO W-IDLEVNR-ALPHA  END-IF                           
339000     IF W-IDLEVNR-ALPHA = 'D3F1A'                                         
339100        MOVE '2379 ' TO W-IDLEVNR-ALPHA  END-IF                           
339200     IF W-IDLEVNR-ALPHA = 'CXC6B'                                         
339300        MOVE '2545 ' TO W-IDLEVNR-ALPHA  END-IF                           
339400     IF W-IDLEVNR-ALPHA = 'D3F1B'                                         
339500        MOVE '2560 ' TO W-IDLEVNR-ALPHA  END-IF                           
339600     IF W-IDLEVNR-ALPHA = 'S053A'                                         
339700        MOVE '5033 ' TO W-IDLEVNR-ALPHA  END-IF                           
339800     IF W-IDLEVNR-ALPHA = 'P90CA'                                         
339900        MOVE '6664 ' TO W-IDLEVNR-ALPHA  END-IF                           
340000     IF W-IDLEVNR-ALPHA = 'BQ9CD'                                         
340100        MOVE '8294 ' TO W-IDLEVNR-ALPHA  END-IF                           
340200     IF W-IDLEVNR-ALPHA = 'BQ1QB'                                         
340300        MOVE '10220' TO W-IDLEVNR-ALPHA  END-IF                           
340400     IF W-IDLEVNR-ALPHA = 'BQ8UA'                                         
340500        MOVE '10374' TO W-IDLEVNR-ALPHA  END-IF                           
340600     IF W-IDLEVNR-ALPHA = 'N5MXB'                                         
340700        MOVE '14032' TO W-IDLEVNR-ALPHA  END-IF                           
340800     IF W-IDLEVNR-ALPHA = 'BQ9CE'                                         
340900        MOVE '19623' TO W-IDLEVNR-ALPHA  END-IF                           
341000     IF W-IDLEVNR-ALPHA = 'D3F1C'                                         
341100        MOVE '25979' TO W-IDLEVNR-ALPHA  END-IF                           
341200     IF W-IDLEVNR-ALPHA = 'D3F1D'                                         
341300        MOVE '25980' TO W-IDLEVNR-ALPHA  END-IF                           
341400     IF W-IDLEVNR-ALPHA = 'DMS1A'                                         
341500        MOVE '25923' TO W-IDLEVNR-ALPHA  END-IF                           
341600*************************************************                         
341700*************************************************                         
341800     IF W-IDLEVNR-ALPHA = 'BHFAA'                                         
341900        MOVE '4    ' TO W-IDLEVNR-ALPHA  END-IF                           
342000     IF W-IDLEVNR-ALPHA = 'CFM6A'                                         
342100        MOVE '32   ' TO W-IDLEVNR-ALPHA  END-IF                           
342200     IF W-IDLEVNR-ALPHA = 'BKR3A'                                         
342300        MOVE '64   ' TO W-IDLEVNR-ALPHA  END-IF                           
342400     IF W-IDLEVNR-ALPHA = 'CL3WA'                                         
342500        MOVE '75   ' TO W-IDLEVNR-ALPHA  END-IF                           
342600     IF W-IDLEVNR-ALPHA = 'BJPKA'                                         
342700        MOVE '81   ' TO W-IDLEVNR-ALPHA  END-IF                           
342800     IF W-IDLEVNR-ALPHA = 'CL3ZA'                                         
342900        MOVE '682  ' TO W-IDLEVNR-ALPHA  END-IF                           
343000     IF W-IDLEVNR-ALPHA = 'CXN9A'                                         
343100        MOVE '777  ' TO W-IDLEVNR-ALPHA  END-IF                           
343200     IF W-IDLEVNR-ALPHA = 'BLRUA'                                         
343300        MOVE '1952 ' TO W-IDLEVNR-ALPHA  END-IF                           
343400     IF W-IDLEVNR-ALPHA = 'BQ5UA'                                         
343500        MOVE '2387 ' TO W-IDLEVNR-ALPHA  END-IF                           
343600     IF W-IDLEVNR-ALPHA = 'BQ5YA'                                         
343700        MOVE '2437 ' TO W-IDLEVNR-ALPHA  END-IF                           
343800     IF W-IDLEVNR-ALPHA = 'BJR1A'                                         
343900        MOVE '2483 ' TO W-IDLEVNR-ALPHA  END-IF                           
344000     IF W-IDLEVNR-ALPHA = 'BQ5ZA'                                         
344100        MOVE '2495 ' TO W-IDLEVNR-ALPHA  END-IF                           
344200     IF W-IDLEVNR-ALPHA = 'BQ6BA'                                         
344300        MOVE '2539 ' TO W-IDLEVNR-ALPHA  END-IF                           
344400     IF W-IDLEVNR-ALPHA = 'CZ1SA'                                         
344500        MOVE '3730 ' TO W-IDLEVNR-ALPHA  END-IF                           
344600     IF W-IDLEVNR-ALPHA = 'F842A'                                         
344700        MOVE '4961 ' TO W-IDLEVNR-ALPHA  END-IF                           
344800     IF W-IDLEVNR-ALPHA = 'P7ZFA'                                         
344900        MOVE '4969 ' TO W-IDLEVNR-ALPHA  END-IF                           
345000     IF W-IDLEVNR-ALPHA = 'C9B7B'                                         
345100        MOVE '6757 ' TO W-IDLEVNR-ALPHA  END-IF                           
345200     IF W-IDLEVNR-ALPHA = 'BQ5VA'                                         
345300        MOVE '10370' TO W-IDLEVNR-ALPHA  END-IF                           
345400     IF W-IDLEVNR-ALPHA = 'R2FLG'                                         
345500        MOVE '11096' TO W-IDLEVNR-ALPHA  END-IF                           
345600     IF W-IDLEVNR-ALPHA = 'BPUKA'                                         
345700        MOVE '13360' TO W-IDLEVNR-ALPHA  END-IF                           
345800     IF W-IDLEVNR-ALPHA = 'BPUMA'                                         
345900        MOVE '13538' TO W-IDLEVNR-ALPHA  END-IF                           
346000     IF W-IDLEVNR-ALPHA = 'E019A'                                         
346100        MOVE '50535' TO W-IDLEVNR-ALPHA  END-IF                           
346200*************************************************                         
346300*************************************************                         
346400     IF W-IDLEVNR-ALPHA = 'LCWLA'                                         
346500        MOVE '139  ' TO W-IDLEVNR-ALPHA  END-IF                           
346600     IF W-IDLEVNR-ALPHA = 'CN4TA'                                         
346700        MOVE '793  ' TO W-IDLEVNR-ALPHA  END-IF                           
346800     IF W-IDLEVNR-ALPHA = 'DL2QA'                                         
346900        MOVE '3911 ' TO W-IDLEVNR-ALPHA  END-IF                           
347000     IF W-IDLEVNR-ALPHA = 'CBVLA'                                         
347100        MOVE '808  ' TO W-IDLEVNR-ALPHA  END-IF                           
347200     IF W-IDLEVNR-ALPHA = 'BMBMA'                                         
347300        MOVE '855  ' TO W-IDLEVNR-ALPHA  END-IF                           
347400     IF W-IDLEVNR-ALPHA = 'BQ3AA'                                         
347500        MOVE '1566 ' TO W-IDLEVNR-ALPHA  END-IF                           
347600     IF W-IDLEVNR-ALPHA = 'CFNMA'                                         
347700        MOVE '1859 ' TO W-IDLEVNR-ALPHA  END-IF                           
347800     IF W-IDLEVNR-ALPHA = 'CBSDA'                                         
347900        MOVE '1863 ' TO W-IDLEVNR-ALPHA  END-IF                           
348000     IF W-IDLEVNR-ALPHA = 'BUF5A'                                         
348100        MOVE '1902 ' TO W-IDLEVNR-ALPHA  END-IF                           
348200     IF W-IDLEVNR-ALPHA = 'BQ6EA'                                         
348300        MOVE '2609 ' TO W-IDLEVNR-ALPHA  END-IF                           
348400     IF W-IDLEVNR-ALPHA = 'BLUDA'                                         
348500        MOVE '3034 ' TO W-IDLEVNR-ALPHA  END-IF                           
348600     IF W-IDLEVNR-ALPHA = 'BKJDA'                                         
348700        MOVE '3152 ' TO W-IDLEVNR-ALPHA  END-IF                           
348800     IF W-IDLEVNR-ALPHA = 'BL1UA'                                         
348900        MOVE '3167 ' TO W-IDLEVNR-ALPHA  END-IF                           
349000     IF W-IDLEVNR-ALPHA = 'BQ6RA'                                         
349100        MOVE '3370 ' TO W-IDLEVNR-ALPHA  END-IF                           
349200     IF W-IDLEVNR-ALPHA = 'D3U3A'                                         
349300        MOVE '3538 ' TO W-IDLEVNR-ALPHA  END-IF                           
349400     IF W-IDLEVNR-ALPHA = 'S0H5D'                                         
349500        MOVE '3669 ' TO W-IDLEVNR-ALPHA  END-IF                           
349600     IF W-IDLEVNR-ALPHA = 'G5FPC'                                         
349700        MOVE '3722 ' TO W-IDLEVNR-ALPHA  END-IF                           
349800     IF W-IDLEVNR-ALPHA = 'V136C'                                         
349900        MOVE '3770 ' TO W-IDLEVNR-ALPHA  END-IF                           
350000     IF W-IDLEVNR-ALPHA = 'P38NA'                                         
350100        MOVE '3988 ' TO W-IDLEVNR-ALPHA  END-IF                           
350200     IF W-IDLEVNR-ALPHA = 'G5FPD'                                         
350300        MOVE '3994 ' TO W-IDLEVNR-ALPHA  END-IF                           
350400     IF W-IDLEVNR-ALPHA = 'E623F'                                         
350500        MOVE '4880 ' TO W-IDLEVNR-ALPHA  END-IF                           
350600     IF W-IDLEVNR-ALPHA = 'D3U2A'                                         
350700        MOVE '4942 ' TO W-IDLEVNR-ALPHA  END-IF                           
350800     IF W-IDLEVNR-ALPHA = 'D3K6A'                                         
350900        MOVE '5314 ' TO W-IDLEVNR-ALPHA  END-IF                           
351000     IF W-IDLEVNR-ALPHA = 'D3L3A'                                         
351100        MOVE '5362 ' TO W-IDLEVNR-ALPHA  END-IF                           
351200     IF W-IDLEVNR-ALPHA = 'BQYEA'                                         
351300        MOVE '25982' TO W-IDLEVNR-ALPHA  END-IF                           
351400     IF W-IDLEVNR-ALPHA = 'V2NYA'                                         
351500        MOVE '25847' TO W-IDLEVNR-ALPHA  END-IF                           
351600     IF W-IDLEVNR-ALPHA = 'DRTHA'                                         
351700        MOVE '25996' TO W-IDLEVNR-ALPHA  END-IF                           
351800     IF W-IDLEVNR-ALPHA = 'D3D0A'                                         
351900        MOVE '6014 ' TO W-IDLEVNR-ALPHA  END-IF                           
352000     IF W-IDLEVNR-ALPHA = 'CRGJA'                                         
352100        MOVE '6101 ' TO W-IDLEVNR-ALPHA  END-IF                           
352200     IF W-IDLEVNR-ALPHA = 'C6S4A'                                         
352300        MOVE '6200 ' TO W-IDLEVNR-ALPHA  END-IF                           
352400     IF W-IDLEVNR-ALPHA = 'Q6QLA'                                         
352500        MOVE '6346 ' TO W-IDLEVNR-ALPHA  END-IF                           
352600     IF W-IDLEVNR-ALPHA = 'D25KA'                                         
352700        MOVE '6810 ' TO W-IDLEVNR-ALPHA  END-IF                           
352800     IF W-IDLEVNR-ALPHA = 'E623B'                                         
352900        MOVE '6842 ' TO W-IDLEVNR-ALPHA  END-IF                           
353000     IF W-IDLEVNR-ALPHA = 'T7WFA'                                         
353100        MOVE '6996 ' TO W-IDLEVNR-ALPHA  END-IF                           
353200     IF W-IDLEVNR-ALPHA = 'BP8JA'                                         
353300        MOVE '10135' TO W-IDLEVNR-ALPHA  END-IF                           
353400     IF W-IDLEVNR-ALPHA = 'BP8JE'                                         
353500        MOVE '10483' TO W-IDLEVNR-ALPHA  END-IF                           
353600     IF W-IDLEVNR-ALPHA = 'R5YYA'                                         
353700        MOVE '10488' TO W-IDLEVNR-ALPHA  END-IF                           
353800     IF W-IDLEVNR-ALPHA = 'BP8JC'                                         
353900        MOVE '10493' TO W-IDLEVNR-ALPHA  END-IF                           
354000     IF W-IDLEVNR-ALPHA = 'Q5EGB'                                         
354100        MOVE '10494' TO W-IDLEVNR-ALPHA  END-IF                           
354200     IF W-IDLEVNR-ALPHA = 'Q5EGB'                                         
354300        MOVE '10494' TO W-IDLEVNR-ALPHA  END-IF                           
354400     IF W-IDLEVNR-ALPHA = 'BQ6RB'                                         
354500        MOVE '13389' TO W-IDLEVNR-ALPHA  END-IF                           
354600     IF W-IDLEVNR-ALPHA = 'M9TMB'                                         
354700        MOVE '13578' TO W-IDLEVNR-ALPHA  END-IF                           
354800     IF W-IDLEVNR-ALPHA = 'F432J'                                         
354900        MOVE '13594' TO W-IDLEVNR-ALPHA  END-IF                           
355000     IF W-IDLEVNR-ALPHA = 'D07LG'                                         
355100        MOVE '13612' TO W-IDLEVNR-ALPHA  END-IF                           
355200     IF W-IDLEVNR-ALPHA = 'D07LB'                                         
355300        MOVE '13614' TO W-IDLEVNR-ALPHA  END-IF                           
355400     IF W-IDLEVNR-ALPHA = 'CR9GA'                                         
355500        MOVE '13741' TO W-IDLEVNR-ALPHA  END-IF                           
355600     IF W-IDLEVNR-ALPHA = 'AN1JE'                                         
355700        MOVE '13793' TO W-IDLEVNR-ALPHA  END-IF                           
355800     IF W-IDLEVNR-ALPHA = 'BQJFA'                                         
355900        MOVE '15451' TO W-IDLEVNR-ALPHA  END-IF                           
356000     IF W-IDLEVNR-ALPHA = 'D25KD'                                         
356100        MOVE '16004' TO W-IDLEVNR-ALPHA  END-IF                           
356200     IF W-IDLEVNR-ALPHA = 'D25KE'                                         
356300        MOVE '16005' TO W-IDLEVNR-ALPHA  END-IF                           
356400     IF W-IDLEVNR-ALPHA = 'E623C'                                         
356500        MOVE '16039' TO W-IDLEVNR-ALPHA  END-IF                           
356600     IF W-IDLEVNR-ALPHA = 'B41YA'                                         
356700        MOVE '16080' TO W-IDLEVNR-ALPHA  END-IF                           
356800     IF W-IDLEVNR-ALPHA = 'E623E'                                         
356900        MOVE '16268' TO W-IDLEVNR-ALPHA  END-IF                           
357000     IF W-IDLEVNR-ALPHA = 'EKM4A'                                         
357100        MOVE '16403' TO W-IDLEVNR-ALPHA  END-IF                           
357200     IF W-IDLEVNR-ALPHA = 'BP8JD'                                         
357300        MOVE '17715' TO W-IDLEVNR-ALPHA  END-IF                           
357400     IF W-IDLEVNR-ALPHA = 'BKPTA'                                         
357500        MOVE '18203' TO W-IDLEVNR-ALPHA  END-IF                           
357600     IF W-IDLEVNR-ALPHA = 'ANEBA'                                         
357700        MOVE '19907' TO W-IDLEVNR-ALPHA  END-IF                           
357800     IF W-IDLEVNR-ALPHA = 'C72DC'                                         
357900        MOVE '22292' TO W-IDLEVNR-ALPHA  END-IF                           
358000     IF W-IDLEVNR-ALPHA = 'DDC3A'                                         
358100        MOVE '25729' TO W-IDLEVNR-ALPHA  END-IF                           
358200     IF W-IDLEVNR-ALPHA = 'AUJ7B'                                         
358300        MOVE '25749' TO W-IDLEVNR-ALPHA  END-IF                           
358400     IF W-IDLEVNR-ALPHA = 'AUJ7C'                                         
358500        MOVE '25846' TO W-IDLEVNR-ALPHA  END-IF                           
358600     IF W-IDLEVNR-ALPHA = 'DNK9A'                                         
358700        MOVE '25931' TO W-IDLEVNR-ALPHA  END-IF                           
358800*************************************************                         
358900**********************************************                            
359000     IF W-IDLEVNR-ALPHA = 'BQEFA'                                         
359100        MOVE '29   ' TO W-IDLEVNR-ALPHA  END-IF                           
359200     IF W-IDLEVNR-ALPHA = 'BQZ7A'                                         
359300        MOVE '280  ' TO W-IDLEVNR-ALPHA  END-IF                           
359400     IF W-IDLEVNR-ALPHA = 'BQZ9A'                                         
359500        MOVE '320  ' TO W-IDLEVNR-ALPHA  END-IF                           
359600     IF W-IDLEVNR-ALPHA = 'BQ0DA'                                         
359700        MOVE '372  ' TO W-IDLEVNR-ALPHA  END-IF                           
359800     IF W-IDLEVNR-ALPHA = 'BQ0EA'                                         
359900        MOVE '495  ' TO W-IDLEVNR-ALPHA  END-IF                           
360000     IF W-IDLEVNR-ALPHA = 'BQ6K1'                                         
360100        MOVE '579  ' TO W-IDLEVNR-ALPHA  END-IF                           
360200     IF W-IDLEVNR-ALPHA = 'CFNEA'                                         
360300        MOVE '710  ' TO W-IDLEVNR-ALPHA  END-IF                           
360400     IF W-IDLEVNR-ALPHA = 'BQ5LA'                                         
360500        MOVE '1912 ' TO W-IDLEVNR-ALPHA  END-IF                           
360600     IF W-IDLEVNR-ALPHA = 'CN4WA'                                         
360700        MOVE '1944 ' TO W-IDLEVNR-ALPHA  END-IF                           
360800     IF W-IDLEVNR-ALPHA = 'CW6BA'                                         
360900        MOVE '2020 ' TO W-IDLEVNR-ALPHA  END-IF                           
361000     IF W-IDLEVNR-ALPHA = 'C6T3A'                                         
361100        MOVE '2351 ' TO W-IDLEVNR-ALPHA  END-IF                           
361200     IF W-IDLEVNR-ALPHA = 'CFNSA'                                         
361300        MOVE '2410 ' TO W-IDLEVNR-ALPHA  END-IF                           
361400     IF W-IDLEVNR-ALPHA = 'P112B'                                         
361500        MOVE '3559 ' TO W-IDLEVNR-ALPHA  END-IF                           
361600     IF W-IDLEVNR-ALPHA = 'C75RA'                                         
361700        MOVE '3606 ' TO W-IDLEVNR-ALPHA  END-IF                           
361800     IF W-IDLEVNR-ALPHA = 'P112L'                                         
361900        MOVE '3705 ' TO W-IDLEVNR-ALPHA  END-IF                           
362000     IF W-IDLEVNR-ALPHA = 'R7NAB'                                         
362100        MOVE '3865 ' TO W-IDLEVNR-ALPHA  END-IF                           
362200     IF W-IDLEVNR-ALPHA = 'B2N4A'                                         
362300        MOVE '4964 ' TO W-IDLEVNR-ALPHA  END-IF                           
362400     IF W-IDLEVNR-ALPHA = 'C8Z7A'                                         
362500        MOVE '6605 ' TO W-IDLEVNR-ALPHA  END-IF                           
362600     IF W-IDLEVNR-ALPHA = 'D0U2A'                                         
362700        MOVE '6765 ' TO W-IDLEVNR-ALPHA  END-IF                           
362800     IF W-IDLEVNR-ALPHA = 'C97GA'                                         
362900        MOVE '6947 ' TO W-IDLEVNR-ALPHA  END-IF                           
363000     IF W-IDLEVNR-ALPHA = 'AZJLA'                                         
363100        MOVE '11148' TO W-IDLEVNR-ALPHA  END-IF                           
363200     IF W-IDLEVNR-ALPHA = 'K0R6F'                                         
363300        MOVE '11326' TO W-IDLEVNR-ALPHA  END-IF                           
363400     IF W-IDLEVNR-ALPHA = 'Q18RA'                                         
363500        MOVE '13382' TO W-IDLEVNR-ALPHA  END-IF                           
363600     IF W-IDLEVNR-ALPHA = 'P112D'                                         
363700        MOVE '13540' TO W-IDLEVNR-ALPHA  END-IF                           
363800     IF W-IDLEVNR-ALPHA = 'P112M'                                         
363900        MOVE '13541' TO W-IDLEVNR-ALPHA  END-IF                           
364000     IF W-IDLEVNR-ALPHA = 'T0CJA'                                         
364100        MOVE '13548' TO W-IDLEVNR-ALPHA  END-IF                           
364200     IF W-IDLEVNR-ALPHA = 'C75RB'                                         
364300        MOVE '13579' TO W-IDLEVNR-ALPHA  END-IF                           
364400     IF W-IDLEVNR-ALPHA = 'K0R6G'                                         
364500        MOVE '13587' TO W-IDLEVNR-ALPHA  END-IF                           
364600     IF W-IDLEVNR-ALPHA = 'BP8HB'                                         
364700        MOVE '14621' TO W-IDLEVNR-ALPHA  END-IF                           
364800     IF W-IDLEVNR-ALPHA = 'D0U2B'                                         
364900        MOVE '16172' TO W-IDLEVNR-ALPHA  END-IF                           
365000     IF W-IDLEVNR-ALPHA = 'D17KA'                                         
365100        MOVE '19255' TO W-IDLEVNR-ALPHA  END-IF                           
365200     IF W-IDLEVNR-ALPHA = 'R76JA'                                         
365300        MOVE '25936' TO W-IDLEVNR-ALPHA  END-IF                           
365400     IF W-IDLEVNR-ALPHA = 'D26QC'                                         
365500        MOVE '25937' TO W-IDLEVNR-ALPHA  END-IF                           
365600     IF W-IDLEVNR-ALPHA = 'BP8HA'                                         
365700        MOVE '34621' TO W-IDLEVNR-ALPHA  END-IF                           
365800*************************************************                         
365900**********************************************                            
366000     IF W-IDLEVNR-ALPHA = 'BJWQA'                                         
366100        MOVE '354  ' TO W-IDLEVNR-ALPHA  END-IF                           
366200     IF W-IDLEVNR-ALPHA = 'BQ3DA'                                         
366300        MOVE '1612 ' TO W-IDLEVNR-ALPHA  END-IF                           
366400     IF W-IDLEVNR-ALPHA = 'CFNLA'                                         
366500        MOVE '1755 ' TO W-IDLEVNR-ALPHA  END-IF                           
366600     IF W-IDLEVNR-ALPHA = 'C96UD'                                         
366700        MOVE '1800 ' TO W-IDLEVNR-ALPHA  END-IF                           
366800     IF W-IDLEVNR-ALPHA = 'BK9KA'                                         
366900        MOVE '2248 ' TO W-IDLEVNR-ALPHA  END-IF                           
367000     IF W-IDLEVNR-ALPHA = 'GPR5A'                                         
367100        MOVE '5425 ' TO W-IDLEVNR-ALPHA  END-IF                           
367200     IF W-IDLEVNR-ALPHA = 'Q0ERA'                                         
367300        MOVE '5489 ' TO W-IDLEVNR-ALPHA  END-IF                           
367400     IF W-IDLEVNR-ALPHA = 'AHHSA'                                         
367500        MOVE '5679 ' TO W-IDLEVNR-ALPHA  END-IF                           
367600     IF W-IDLEVNR-ALPHA = 'S2ZLA'                                         
367700        MOVE '6756 ' TO W-IDLEVNR-ALPHA  END-IF                           
367800     IF W-IDLEVNR-ALPHA = 'C7B1A'                                         
367900        MOVE '6775 ' TO W-IDLEVNR-ALPHA  END-IF                           
368000     IF W-IDLEVNR-ALPHA = 'D2S2A'                                         
368100        MOVE '6795 ' TO W-IDLEVNR-ALPHA  END-IF                           
368200     IF W-IDLEVNR-ALPHA = 'D23YA'                                         
368300        MOVE '6807 ' TO W-IDLEVNR-ALPHA  END-IF                           
368400     IF W-IDLEVNR-ALPHA = 'BQ7PA'                                         
368500        MOVE '7218 ' TO W-IDLEVNR-ALPHA  END-IF                           
368600     IF W-IDLEVNR-ALPHA = 'CDHSA'                                         
368700        MOVE '7757 ' TO W-IDLEVNR-ALPHA  END-IF                           
368800     IF W-IDLEVNR-ALPHA = 'D23YB'                                         
368900        MOVE '7922 ' TO W-IDLEVNR-ALPHA  END-IF                           
369000     IF W-IDLEVNR-ALPHA = 'P8C8A'                                         
369100        MOVE '14615' TO W-IDLEVNR-ALPHA  END-IF                           
369200     IF W-IDLEVNR-ALPHA = 'N2M5A'                                         
369300        MOVE '16088' TO W-IDLEVNR-ALPHA  END-IF                           
369400     IF W-IDLEVNR-ALPHA = 'DAFTA'                                         
369500        MOVE '16171' TO W-IDLEVNR-ALPHA  END-IF                           
369600     IF W-IDLEVNR-ALPHA = 'BQ9VA'                                         
369700        MOVE '16388' TO W-IDLEVNR-ALPHA  END-IF                           
369800     IF W-IDLEVNR-ALPHA = 'BHZ3A'                                         
369900        MOVE '18688' TO W-IDLEVNR-ALPHA  END-IF                           
370000     IF W-IDLEVNR-ALPHA = 'V4FVA'                                         
370100        MOVE '19454' TO W-IDLEVNR-ALPHA  END-IF                           
370200     IF W-IDLEVNR-ALPHA = 'S3JJA'                                         
370300        MOVE '19455' TO W-IDLEVNR-ALPHA  END-IF                           
370400     IF W-IDLEVNR-ALPHA = 'BQ7PB'                                         
370500        MOVE '24065' TO W-IDLEVNR-ALPHA  END-IF                           
370600     IF W-IDLEVNR-ALPHA = 'BQ7RB'                                         
370700        MOVE '24066' TO W-IDLEVNR-ALPHA  END-IF                           
370800     IF W-IDLEVNR-ALPHA = 'G13FC'                                         
370900        MOVE '24078' TO W-IDLEVNR-ALPHA  END-IF                           
371000     IF W-IDLEVNR-ALPHA = 'BQ7RC'                                         
371100        MOVE '25752' TO W-IDLEVNR-ALPHA  END-IF                           
371200*************************************************                         
371300     IF W-IDLEVNR-ALPHA = 'BMLWA'                                         
371400        MOVE '143  ' TO W-IDLEVNR-ALPHA  END-IF                           
371500     IF W-IDLEVNR-ALPHA = 'BQ1NA'                                         
371600        MOVE '934  ' TO W-IDLEVNR-ALPHA  END-IF                           
371700     IF W-IDLEVNR-ALPHA = 'BPUYA'                                         
371800        MOVE '1008 ' TO W-IDLEVNR-ALPHA  END-IF                           
371900     IF W-IDLEVNR-ALPHA = 'BKRZA'                                         
372000        MOVE '1062 ' TO W-IDLEVNR-ALPHA  END-IF                           
372100     IF W-IDLEVNR-ALPHA = 'S34XF'                                         
372200        MOVE '1134 ' TO W-IDLEVNR-ALPHA  END-IF                           
372300     IF W-IDLEVNR-ALPHA = 'BKDQA'                                         
372400        MOVE '1196 ' TO W-IDLEVNR-ALPHA  END-IF                           
372500     IF W-IDLEVNR-ALPHA = 'U7ABB'                                         
372600        MOVE '1244 ' TO W-IDLEVNR-ALPHA  END-IF                           
372700     IF W-IDLEVNR-ALPHA = 'U7ABC'                                         
372800        MOVE '1269 ' TO W-IDLEVNR-ALPHA  END-IF                           
372900     IF W-IDLEVNR-ALPHA = 'BSKYA'                                         
373000        MOVE '1449 ' TO W-IDLEVNR-ALPHA  END-IF                           
373100     IF W-IDLEVNR-ALPHA = 'BKMMA'                                         
373200        MOVE '1662 ' TO W-IDLEVNR-ALPHA  END-IF                           
373300     IF W-IDLEVNR-ALPHA = 'U7ABA'                                         
373400        MOVE '1847 ' TO W-IDLEVNR-ALPHA  END-IF                           
373500     IF W-IDLEVNR-ALPHA = 'S34XD'                                         
373600        MOVE '2500 ' TO W-IDLEVNR-ALPHA  END-IF                           
373700     IF W-IDLEVNR-ALPHA = 'BQAJA'                                         
373800        MOVE '2552 ' TO W-IDLEVNR-ALPHA  END-IF                           
373900     IF W-IDLEVNR-ALPHA = 'BWKGA'                                         
374000        MOVE '2553 ' TO W-IDLEVNR-ALPHA  END-IF                           
374100     IF W-IDLEVNR-ALPHA = 'BWTCA'                                         
374200        MOVE '2558 ' TO W-IDLEVNR-ALPHA  END-IF                           
374300     IF W-IDLEVNR-ALPHA = 'CFNVA'                                         
374400        MOVE '2633 ' TO W-IDLEVNR-ALPHA  END-IF                           
374500     IF W-IDLEVNR-ALPHA = 'BQAJB'                                         
374600        MOVE '2684 ' TO W-IDLEVNR-ALPHA  END-IF                           
374700     IF W-IDLEVNR-ALPHA = 'BWKSA'                                         
374800        MOVE '3054 ' TO W-IDLEVNR-ALPHA  END-IF                           
374900     IF W-IDLEVNR-ALPHA = 'CN4YA'                                         
375000        MOVE '3165 ' TO W-IDLEVNR-ALPHA  END-IF                           
375100     IF W-IDLEVNR-ALPHA = 'CFNWA'                                         
375200        MOVE '3197 ' TO W-IDLEVNR-ALPHA  END-IF                           
375300     IF W-IDLEVNR-ALPHA = 'D3C5A'                                         
375400        MOVE '3468 ' TO W-IDLEVNR-ALPHA  END-IF                           
375500     IF W-IDLEVNR-ALPHA = 'BP9ZA'                                         
375600        MOVE '3558 ' TO W-IDLEVNR-ALPHA  END-IF                           
375700     IF W-IDLEVNR-ALPHA = 'Q6TFA'                                         
375800        MOVE '3662 ' TO W-IDLEVNR-ALPHA  END-IF                           
375900     IF W-IDLEVNR-ALPHA = 'D5H4A'                                         
376000        MOVE '3807 ' TO W-IDLEVNR-ALPHA  END-IF                           
376100     IF W-IDLEVNR-ALPHA = 'BQ6VA'                                         
376200        MOVE '3982 ' TO W-IDLEVNR-ALPHA  END-IF                           
376300     IF W-IDLEVNR-ALPHA = 'AHMPA'                                         
376400        MOVE '4172 ' TO W-IDLEVNR-ALPHA  END-IF                           
376500     IF W-IDLEVNR-ALPHA = 'BPFNA'                                         
376600        MOVE '4721 ' TO W-IDLEVNR-ALPHA  END-IF                           
376700     IF W-IDLEVNR-ALPHA = 'ATNNA'                                         
376800        MOVE '4724 ' TO W-IDLEVNR-ALPHA  END-IF                           
376900     IF W-IDLEVNR-ALPHA = 'BCJSA'                                         
377000        MOVE '4988 ' TO W-IDLEVNR-ALPHA  END-IF                           
377100     IF W-IDLEVNR-ALPHA = 'C7L2A'                                         
377200        MOVE '5281 ' TO W-IDLEVNR-ALPHA  END-IF                           
377300     IF W-IDLEVNR-ALPHA = 'E3B2B'                                         
377400        MOVE '6305 ' TO W-IDLEVNR-ALPHA  END-IF                           
377500     IF W-IDLEVNR-ALPHA = 'C68JA'                                         
377600        MOVE '6350 ' TO W-IDLEVNR-ALPHA  END-IF                           
377700     IF W-IDLEVNR-ALPHA = 'D0MGA'                                         
377800        MOVE '6554 ' TO W-IDLEVNR-ALPHA  END-IF                           
377900     IF W-IDLEVNR-ALPHA = 'BPX3B'                                         
378000        MOVE '6555 ' TO W-IDLEVNR-ALPHA  END-IF                           
378100     IF W-IDLEVNR-ALPHA = 'C8T1A'                                         
378200        MOVE '6556 ' TO W-IDLEVNR-ALPHA  END-IF                           
378300     IF W-IDLEVNR-ALPHA = 'C68JC'                                         
378400        MOVE '6597 ' TO W-IDLEVNR-ALPHA  END-IF                           
378500     IF W-IDLEVNR-ALPHA = 'G944E'                                         
378600        MOVE '6911 ' TO W-IDLEVNR-ALPHA  END-IF                           
378700     IF W-IDLEVNR-ALPHA = 'CDNXA'                                         
378800        MOVE '6958 ' TO W-IDLEVNR-ALPHA  END-IF                           
378900     IF W-IDLEVNR-ALPHA = 'D38KA'                                         
379000        MOVE '6961 ' TO W-IDLEVNR-ALPHA  END-IF                           
379100     IF W-IDLEVNR-ALPHA = 'D38KD'                                         
379200        MOVE '6962 ' TO W-IDLEVNR-ALPHA  END-IF                           
379300     IF W-IDLEVNR-ALPHA = 'MTSFA'                                         
379400        MOVE '7462 ' TO W-IDLEVNR-ALPHA  END-IF                           
379500     IF W-IDLEVNR-ALPHA = 'BP9ZC'                                         
379600        MOVE '13672' TO W-IDLEVNR-ALPHA  END-IF                           
379700     IF W-IDLEVNR-ALPHA = 'BP9ZB'                                         
379800        MOVE '13709' TO W-IDLEVNR-ALPHA  END-IF                           
379900     IF W-IDLEVNR-ALPHA = 'D38KF'                                         
380000        MOVE '16044' TO W-IDLEVNR-ALPHA  END-IF                           
380100     IF W-IDLEVNR-ALPHA = 'R7GNA'                                         
380200        MOVE '16092' TO W-IDLEVNR-ALPHA  END-IF                           
380300     IF W-IDLEVNR-ALPHA = 'T2GCC'                                         
380400        MOVE '16141' TO W-IDLEVNR-ALPHA  END-IF                           
380500     IF W-IDLEVNR-ALPHA = 'D0W5D'                                         
380600        MOVE '16142' TO W-IDLEVNR-ALPHA  END-IF                           
380700     IF W-IDLEVNR-ALPHA = 'BPX3C'                                         
380800        MOVE '16365' TO W-IDLEVNR-ALPHA  END-IF                           
380900     IF W-IDLEVNR-ALPHA = 'T2GCA'                                         
381000        MOVE '16406' TO W-IDLEVNR-ALPHA  END-IF                           
381100     IF W-IDLEVNR-ALPHA = 'AD3XA'                                         
381200        MOVE '21256' TO W-IDLEVNR-ALPHA  END-IF                           
381300     IF W-IDLEVNR-ALPHA = 'D38KG'                                         
381400        MOVE '25618' TO W-IDLEVNR-ALPHA  END-IF                           
381500     IF W-IDLEVNR-ALPHA = 'BVPDA'                                         
381600        MOVE '25920' TO W-IDLEVNR-ALPHA  END-IF                           
381700     IF W-IDLEVNR-ALPHA = 'C68JF'                                         
381800        MOVE '26350' TO W-IDLEVNR-ALPHA  END-IF                           
381900*************************************************                         
382000     IF W-IDLEVNR-ALPHA = 'BP8JB'                                         
382100        MOVE '912  ' TO W-IDLEVNR-ALPHA  END-IF                           
382200     IF W-IDLEVNR-ALPHA = 'BMJGA'                                         
382300        MOVE '1005 ' TO W-IDLEVNR-ALPHA  END-IF                           
382400     IF W-IDLEVNR-ALPHA = 'AY0CA'                                         
382500        MOVE '1720 ' TO W-IDLEVNR-ALPHA  END-IF                           
382600     IF W-IDLEVNR-ALPHA = 'BPUNA'                                         
382700        MOVE '2442 ' TO W-IDLEVNR-ALPHA  END-IF                           
382800     IF W-IDLEVNR-ALPHA = 'BKTXA'                                         
382900        MOVE '3074 ' TO W-IDLEVNR-ALPHA  END-IF                           
383000     IF W-IDLEVNR-ALPHA = 'BPA1A'                                         
383100        MOVE '3163 ' TO W-IDLEVNR-ALPHA  END-IF                           
383200     IF W-IDLEVNR-ALPHA = 'C62FA'                                         
383300        MOVE '3575 ' TO W-IDLEVNR-ALPHA  END-IF                           
383400     IF W-IDLEVNR-ALPHA = 'J3CQA'                                         
383500        MOVE '4319 ' TO W-IDLEVNR-ALPHA  END-IF                           
383600     IF W-IDLEVNR-ALPHA = 'D026P'                                         
383700        MOVE '4382 ' TO W-IDLEVNR-ALPHA  END-IF                           
383800     IF W-IDLEVNR-ALPHA = 'MLMZA'                                         
383900        MOVE '4448 ' TO W-IDLEVNR-ALPHA  END-IF                           
384000     IF W-IDLEVNR-ALPHA = 'BQ6WA'                                         
384100        MOVE '4585 ' TO W-IDLEVNR-ALPHA  END-IF                           
384200     IF W-IDLEVNR-ALPHA = 'BQ6WB'                                         
384300        MOVE '4628 ' TO W-IDLEVNR-ALPHA  END-IF                           
384400     IF W-IDLEVNR-ALPHA = 'A426K'                                         
384500        MOVE '4894 ' TO W-IDLEVNR-ALPHA  END-IF                           
384600     IF W-IDLEVNR-ALPHA = 'C0VAG'                                         
384700        MOVE '4965 ' TO W-IDLEVNR-ALPHA  END-IF                           
384800     IF W-IDLEVNR-ALPHA = 'AVG9A'                                         
384900        MOVE '5065 ' TO W-IDLEVNR-ALPHA  END-IF                           
385000     IF W-IDLEVNR-ALPHA = 'D24DA'                                         
385100        MOVE '5647 ' TO W-IDLEVNR-ALPHA  END-IF                           
385200     IF W-IDLEVNR-ALPHA = 'D0RYA'                                         
385300        MOVE '6030 ' TO W-IDLEVNR-ALPHA  END-IF                           
385400     IF W-IDLEVNR-ALPHA = 'BT7WA'                                         
385500        MOVE '6279 ' TO W-IDLEVNR-ALPHA  END-IF                           
385600     IF W-IDLEVNR-ALPHA = 'A426G'                                         
385700        MOVE '6840 ' TO W-IDLEVNR-ALPHA  END-IF                           
385800     IF W-IDLEVNR-ALPHA = 'F488A'                                         
385900        MOVE '6992 ' TO W-IDLEVNR-ALPHA  END-IF                           
386000     IF W-IDLEVNR-ALPHA = 'BP3HA'                                         
386100        MOVE '7349 ' TO W-IDLEVNR-ALPHA  END-IF                           
386200     IF W-IDLEVNR-ALPHA = 'CL3VA'                                         
386300        MOVE '10453' TO W-IDLEVNR-ALPHA  END-IF                           
386400     IF W-IDLEVNR-ALPHA = 'A426S'                                         
386500        MOVE '10814' TO W-IDLEVNR-ALPHA  END-IF                           
386600     IF W-IDLEVNR-ALPHA = 'CFT4B'                                         
386700        MOVE '12543' TO W-IDLEVNR-ALPHA  END-IF                           
386800     IF W-IDLEVNR-ALPHA = 'M09EA'                                         
386900        MOVE '14616' TO W-IDLEVNR-ALPHA  END-IF                           
387000     IF W-IDLEVNR-ALPHA = 'BQ6WC'                                         
387100        MOVE '14643' TO W-IDLEVNR-ALPHA  END-IF                           
387200     IF W-IDLEVNR-ALPHA = 'A426R'                                         
387300        MOVE '14647' TO W-IDLEVNR-ALPHA  END-IF                           
387400     IF W-IDLEVNR-ALPHA = 'N718D'                                         
387500        MOVE '14963' TO W-IDLEVNR-ALPHA  END-IF                           
387600     IF W-IDLEVNR-ALPHA = 'N718B'                                         
387700        MOVE '14988' TO W-IDLEVNR-ALPHA  END-IF                           
387800     IF W-IDLEVNR-ALPHA = 'B492C'                                         
387900        MOVE '16134' TO W-IDLEVNR-ALPHA  END-IF                           
388000     IF W-IDLEVNR-ALPHA = 'F488X'                                         
388100        MOVE '16137' TO W-IDLEVNR-ALPHA  END-IF                           
388200     IF W-IDLEVNR-ALPHA = 'AYZ4A'                                         
388300        MOVE '16210' TO W-IDLEVNR-ALPHA  END-IF                           
388400     IF W-IDLEVNR-ALPHA = 'C685Y'                                         
388500        MOVE '16211' TO W-IDLEVNR-ALPHA  END-IF                           
388600     IF W-IDLEVNR-ALPHA = 'C685C'                                         
388700        MOVE '16222' TO W-IDLEVNR-ALPHA  END-IF                           
388800     IF W-IDLEVNR-ALPHA = 'A426C'                                         
388900        MOVE '16237' TO W-IDLEVNR-ALPHA  END-IF                           
389000     IF W-IDLEVNR-ALPHA = 'A426M'                                         
389100        MOVE '16279' TO W-IDLEVNR-ALPHA  END-IF                           
389200     IF W-IDLEVNR-ALPHA = 'F488Z'                                         
389300        MOVE '16283' TO W-IDLEVNR-ALPHA  END-IF                           
389400     IF W-IDLEVNR-ALPHA = 'BA8YA'                                         
389500        MOVE '20094' TO W-IDLEVNR-ALPHA  END-IF                           
389600     IF W-IDLEVNR-ALPHA = 'AUE4A'                                         
389700        MOVE '21873' TO W-IDLEVNR-ALPHA  END-IF                           
389800     IF W-IDLEVNR-ALPHA = 'M09EB'                                         
389900        MOVE '25851' TO W-IDLEVNR-ALPHA  END-IF                           
390000     IF W-IDLEVNR-ALPHA = 'BARJA'                                         
390100        MOVE '25907' TO W-IDLEVNR-ALPHA  END-IF                           
390200     IF W-IDLEVNR-ALPHA = 'BARJB'                                         
390300        MOVE '25908' TO W-IDLEVNR-ALPHA  END-IF                           
390400     IF W-IDLEVNR-ALPHA = 'CUTBA'                                         
390500        MOVE '25909' TO W-IDLEVNR-ALPHA  END-IF                           
390600     IF W-IDLEVNR-ALPHA = 'A426U'                                         
390700        MOVE '26840' TO W-IDLEVNR-ALPHA  END-IF                           
390800     IF W-IDLEVNR-ALPHA = 'BQ7RB'                                         
390900        MOVE '24066' TO W-IDLEVNR-ALPHA  END-IF                           
391000     IF W-IDLEVNR-ALPHA = 'BQ7RC'                                         
391100        MOVE '25752' TO W-IDLEVNR-ALPHA  END-IF                           
391200*************************************************                         
391300     IF W-IDLEVNR-ALPHA = 'BMP1A'                                         
391400        MOVE '856  ' TO W-IDLEVNR-ALPHA  END-IF                           
391500     IF W-IDLEVNR-ALPHA = 'D07FA'                                         
391600        MOVE '3330 ' TO W-IDLEVNR-ALPHA  END-IF                           
391700     IF W-IDLEVNR-ALPHA = 'T2VGA'                                         
391800        MOVE '5453 ' TO W-IDLEVNR-ALPHA  END-IF                           
391900     IF W-IDLEVNR-ALPHA = 'D07FD'                                         
392000        MOVE '6013 ' TO W-IDLEVNR-ALPHA  END-IF                           
392100     IF W-IDLEVNR-ALPHA = 'E510D'                                         
392200        MOVE '6160 ' TO W-IDLEVNR-ALPHA  END-IF                           
392300     IF W-IDLEVNR-ALPHA = 'D07FF'                                         
392400        MOVE '6193 ' TO W-IDLEVNR-ALPHA  END-IF                           
392500     IF W-IDLEVNR-ALPHA = 'BQ7MA'                                         
392600        MOVE '6285 ' TO W-IDLEVNR-ALPHA  END-IF                           
392700     IF W-IDLEVNR-ALPHA = 'CGSWA'                                         
392800        MOVE '7622 ' TO W-IDLEVNR-ALPHA  END-IF                           
392900     IF W-IDLEVNR-ALPHA = 'BZMDA'                                         
393000        MOVE '10965' TO W-IDLEVNR-ALPHA  END-IF                           
393100     IF W-IDLEVNR-ALPHA = 'BEF1A'                                         
393200        MOVE '20303' TO W-IDLEVNR-ALPHA  END-IF                           
393300*************************************************                         
393400     IF W-IDLEVNR-ALPHA = 'BWTAA'                                         
393500        MOVE '520  ' TO W-IDLEVNR-ALPHA  END-IF                           
393600     IF W-IDLEVNR-ALPHA = 'BQ00A'                                         
393700        MOVE '546  ' TO W-IDLEVNR-ALPHA  END-IF                           
393800     IF W-IDLEVNR-ALPHA = 'AHFGA'                                         
393900        MOVE '547  ' TO W-IDLEVNR-ALPHA  END-IF                           
394000     IF W-IDLEVNR-ALPHA = 'BQ1BA'                                         
394100        MOVE '550  ' TO W-IDLEVNR-ALPHA  END-IF                           
394200     IF W-IDLEVNR-ALPHA = 'BQ3YA'                                         
394300        MOVE '1883 ' TO W-IDLEVNR-ALPHA  END-IF                           
394400     IF W-IDLEVNR-ALPHA = 'S3HXA'                                         
394500        MOVE '4610 ' TO W-IDLEVNR-ALPHA  END-IF                           
394600     IF W-IDLEVNR-ALPHA = 'AKTAD'                                         
394700        MOVE '6110 ' TO W-IDLEVNR-ALPHA  END-IF                           
394800     IF W-IDLEVNR-ALPHA = 'BQAWA'                                         
394900        MOVE '10087' TO W-IDLEVNR-ALPHA  END-IF                           
395000     IF W-IDLEVNR-ALPHA = 'BAM8D'                                         
395100        MOVE '11328' TO W-IDLEVNR-ALPHA  END-IF                           
395200     IF W-IDLEVNR-ALPHA = 'BAM8A'                                         
395300        MOVE '19956' TO W-IDLEVNR-ALPHA  END-IF                           
395400     IF W-IDLEVNR-ALPHA = 'CEPHA'                                         
395500        MOVE '19986' TO W-IDLEVNR-ALPHA  END-IF                           
395600*************************************************                         
395700     IF W-IDLEVNR-ALPHA = 'BQAHA'                                         
395800        MOVE '114  ' TO W-IDLEVNR-ALPHA  END-IF                           
395900     IF W-IDLEVNR-ALPHA = 'S3SQA'                                         
396000        MOVE '642  ' TO W-IDLEVNR-ALPHA  END-IF                           
396100     IF W-IDLEVNR-ALPHA = 'S3SQB'                                         
396200        MOVE '1297 ' TO W-IDLEVNR-ALPHA  END-IF                           
396300     IF W-IDLEVNR-ALPHA = 'BQAHD'                                         
396400        MOVE '2639 ' TO W-IDLEVNR-ALPHA  END-IF                           
396500     IF W-IDLEVNR-ALPHA = 'BT7RA'                                         
396600        MOVE '2647 ' TO W-IDLEVNR-ALPHA  END-IF                           
396700     IF W-IDLEVNR-ALPHA = 'BEFYA'                                         
396800        MOVE '3671 ' TO W-IDLEVNR-ALPHA  END-IF                           
396900     IF W-IDLEVNR-ALPHA = 'K0R6A'                                         
397000        MOVE '3787 ' TO W-IDLEVNR-ALPHA  END-IF                           
397100     IF W-IDLEVNR-ALPHA = 'C84QA'                                         
397200        MOVE '3883 ' TO W-IDLEVNR-ALPHA  END-IF                           
397300     IF W-IDLEVNR-ALPHA = 'P38NA'                                         
397400        MOVE '3988 ' TO W-IDLEVNR-ALPHA  END-IF                           
397500     IF W-IDLEVNR-ALPHA = 'MMV4A'                                         
397600        MOVE '4737 ' TO W-IDLEVNR-ALPHA  END-IF                           
397700     IF W-IDLEVNR-ALPHA = 'F842A'                                         
397800        MOVE '4961 ' TO W-IDLEVNR-ALPHA  END-IF                           
397900     IF W-IDLEVNR-ALPHA = 'E1SLB'                                         
398000        MOVE '5160 ' TO W-IDLEVNR-ALPHA  END-IF                           
398100     IF W-IDLEVNR-ALPHA = 'D2R5A'                                         
398200        MOVE '5162 ' TO W-IDLEVNR-ALPHA  END-IF                           
398300     IF W-IDLEVNR-ALPHA = 'C93SA'                                         
398400        MOVE '5171 ' TO W-IDLEVNR-ALPHA  END-IF                           
398500     IF W-IDLEVNR-ALPHA = 'Q749A'                                         
398600        MOVE '5240 ' TO W-IDLEVNR-ALPHA  END-IF                           
398700     IF W-IDLEVNR-ALPHA = 'H268X'                                         
398800        MOVE '5383 ' TO W-IDLEVNR-ALPHA  END-IF                           
398900     IF W-IDLEVNR-ALPHA = 'AXVNA'                                         
399000        MOVE '5662 ' TO W-IDLEVNR-ALPHA  END-IF                           
399100     IF W-IDLEVNR-ALPHA = 'D16AA'                                         
399200        MOVE '6083 ' TO W-IDLEVNR-ALPHA  END-IF                           
399300     IF W-IDLEVNR-ALPHA = 'E521A'                                         
399400        MOVE '6090 ' TO W-IDLEVNR-ALPHA  END-IF                           
399500     IF W-IDLEVNR-ALPHA = 'D04HA'                                         
399600        MOVE '6096 ' TO W-IDLEVNR-ALPHA  END-IF                           
399700     IF W-IDLEVNR-ALPHA = 'EGX2A'                                         
399800        MOVE '6146 ' TO W-IDLEVNR-ALPHA  END-IF                           
399900     IF W-IDLEVNR-ALPHA = 'C6S4A'                                         
400000        MOVE '6200 ' TO W-IDLEVNR-ALPHA  END-IF                           
400100     IF W-IDLEVNR-ALPHA = 'C9U3A'                                         
400200        MOVE '6226 ' TO W-IDLEVNR-ALPHA  END-IF                           
400300     IF W-IDLEVNR-ALPHA = 'D1C6A'                                         
400400        MOVE '6310 ' TO W-IDLEVNR-ALPHA  END-IF                           
400500     IF W-IDLEVNR-ALPHA = 'F6W7A'                                         
400600        MOVE '6321 ' TO W-IDLEVNR-ALPHA  END-IF                           
400700     IF W-IDLEVNR-ALPHA = 'CFTXA'                                         
400800        MOVE '6328 ' TO W-IDLEVNR-ALPHA  END-IF                           
400900     IF W-IDLEVNR-ALPHA = 'AECTA'                                         
401000        MOVE '6360 ' TO W-IDLEVNR-ALPHA  END-IF                           
401100     IF W-IDLEVNR-ALPHA = 'BPU8A'                                         
401200        MOVE '6421 ' TO W-IDLEVNR-ALPHA  END-IF                           
401300     IF W-IDLEVNR-ALPHA = 'X345A'                                         
401400        MOVE '6443 ' TO W-IDLEVNR-ALPHA  END-IF                           
401500     IF W-IDLEVNR-ALPHA = 'D16MA'                                         
401600        MOVE '6468 ' TO W-IDLEVNR-ALPHA  END-IF                           
401700     IF W-IDLEVNR-ALPHA = 'BB4SA'                                         
401800        MOVE '6488 ' TO W-IDLEVNR-ALPHA  END-IF                           
401900     IF W-IDLEVNR-ALPHA = 'BZFFA'                                         
402000        MOVE '6492 ' TO W-IDLEVNR-ALPHA  END-IF                           
402100     IF W-IDLEVNR-ALPHA = 'C9T9A'                                         
402200        MOVE '6764 ' TO W-IDLEVNR-ALPHA  END-IF                           
402300     IF W-IDLEVNR-ALPHA = 'D3L4A'                                         
402400        MOVE '6903 ' TO W-IDLEVNR-ALPHA  END-IF                           
402500     IF W-IDLEVNR-ALPHA = 'DNR5A'                                         
402600        MOVE '6968 ' TO W-IDLEVNR-ALPHA  END-IF                           
402700     IF W-IDLEVNR-ALPHA = 'D0DMA'                                         
402800        MOVE '8040 ' TO W-IDLEVNR-ALPHA  END-IF                           
402900     IF W-IDLEVNR-ALPHA = 'BPXEB'                                         
403000        MOVE '10103' TO W-IDLEVNR-ALPHA  END-IF                           
403100     IF W-IDLEVNR-ALPHA = 'R2FLG'                                         
403200        MOVE '11096' TO W-IDLEVNR-ALPHA  END-IF                           
403300     IF W-IDLEVNR-ALPHA = 'F432J'                                         
403400        MOVE '13594' TO W-IDLEVNR-ALPHA  END-IF                           
403500     IF W-IDLEVNR-ALPHA = 'F432B'                                         
403600        MOVE '13690' TO W-IDLEVNR-ALPHA  END-IF                           
403700     IF W-IDLEVNR-ALPHA = 'K0R6E'                                         
403800        MOVE '13701' TO W-IDLEVNR-ALPHA  END-IF                           
403900     IF W-IDLEVNR-ALPHA = 'E2L9A'                                         
404000        MOVE '16051' TO W-IDLEVNR-ALPHA  END-IF                           
404100     IF W-IDLEVNR-ALPHA = 'C6S4B'                                         
404200        MOVE '16115' TO W-IDLEVNR-ALPHA  END-IF                           
404300     IF W-IDLEVNR-ALPHA = 'H681K'                                         
404400        MOVE '16383' TO W-IDLEVNR-ALPHA  END-IF                           
404500     IF W-IDLEVNR-ALPHA = 'D0V6C'                                         
404600        MOVE '25047' TO W-IDLEVNR-ALPHA  END-IF                           
404700     IF W-IDLEVNR-ALPHA = 'E521J'                                         
404800        MOVE '25403' TO W-IDLEVNR-ALPHA  END-IF                           
404900     IF W-IDLEVNR-ALPHA = 'E019A'                                         
405000        MOVE '50535' TO W-IDLEVNR-ALPHA  END-IF                           
405100     IF W-IDLEVNR-ALPHA = 'CL3XA'                                         
405200        MOVE '63567' TO W-IDLEVNR-ALPHA  END-IF                           
405300*************************************************                         
405400     IF W-IDLEVNR-ALPHA = 'BSK5A'                                         
405500        MOVE '84   ' TO W-IDLEVNR-ALPHA  END-IF                           
405600     IF W-IDLEVNR-ALPHA = 'DJAYA'                                         
405700        MOVE '157  ' TO W-IDLEVNR-ALPHA  END-IF                           
405800     IF W-IDLEVNR-ALPHA = 'BV9NA'                                         
405900        MOVE '173  ' TO W-IDLEVNR-ALPHA  END-IF                           
406000     IF W-IDLEVNR-ALPHA = 'BU4GA'                                         
406100        MOVE '233  ' TO W-IDLEVNR-ALPHA  END-IF                           
406200     IF W-IDLEVNR-ALPHA = 'DJA8A'                                         
406300        MOVE '235  ' TO W-IDLEVNR-ALPHA  END-IF                           
406400     IF W-IDLEVNR-ALPHA = 'DL0KA'                                         
406500        MOVE '282  ' TO W-IDLEVNR-ALPHA  END-IF                           
406600     IF W-IDLEVNR-ALPHA = 'BXPRA'                                         
406700        MOVE '294  ' TO W-IDLEVNR-ALPHA  END-IF                           
406800     IF W-IDLEVNR-ALPHA = 'DJBBA'                                         
406900        MOVE '357  ' TO W-IDLEVNR-ALPHA  END-IF                           
407000     IF W-IDLEVNR-ALPHA = 'DL0LA'                                         
407100        MOVE '370  ' TO W-IDLEVNR-ALPHA  END-IF                           
407200     IF W-IDLEVNR-ALPHA = 'DJBCA'                                         
407300        MOVE '376  ' TO W-IDLEVNR-ALPHA  END-IF                           
407400     IF W-IDLEVNR-ALPHA = 'DL0MA'                                         
407500        MOVE '428  ' TO W-IDLEVNR-ALPHA  END-IF                           
407600     IF W-IDLEVNR-ALPHA = 'BRQFA'                                         
407700        MOVE '449  ' TO W-IDLEVNR-ALPHA  END-IF                           
407800     IF W-IDLEVNR-ALPHA = 'BSK2A'                                         
407900        MOVE '466  ' TO W-IDLEVNR-ALPHA  END-IF                           
408000     IF W-IDLEVNR-ALPHA = 'S6RPA'                                         
408100        MOVE '498  ' TO W-IDLEVNR-ALPHA  END-IF                           
408200     IF W-IDLEVNR-ALPHA = 'BLMNA'                                         
408300        MOVE '505  ' TO W-IDLEVNR-ALPHA  END-IF                           
408400     IF W-IDLEVNR-ALPHA = 'BJ9QA'                                         
408500        MOVE '523  ' TO W-IDLEVNR-ALPHA  END-IF                           
408600     IF W-IDLEVNR-ALPHA = 'DJBDA'                                         
408700        MOVE '524  ' TO W-IDLEVNR-ALPHA  END-IF                           
408800     IF W-IDLEVNR-ALPHA = 'CVESB'                                         
408900        MOVE '555  ' TO W-IDLEVNR-ALPHA  END-IF                           
409000     IF W-IDLEVNR-ALPHA = 'CFH3A'                                         
409100        MOVE '652  ' TO W-IDLEVNR-ALPHA  END-IF                           
409200     IF W-IDLEVNR-ALPHA = 'DJBEA'                                         
409300        MOVE '670  ' TO W-IDLEVNR-ALPHA  END-IF                           
409400     IF W-IDLEVNR-ALPHA = 'BMKTA'                                         
409500        MOVE '671  ' TO W-IDLEVNR-ALPHA  END-IF                           
409600     IF W-IDLEVNR-ALPHA = 'DJBFA'                                         
409700        MOVE '719  ' TO W-IDLEVNR-ALPHA  END-IF                           
409800     IF W-IDLEVNR-ALPHA = 'CDT1A'                                         
409900        MOVE '753  ' TO W-IDLEVNR-ALPHA  END-IF                           
410000     IF W-IDLEVNR-ALPHA = 'DJBGA'                                         
410100        MOVE '794  ' TO W-IDLEVNR-ALPHA  END-IF                           
410200     IF W-IDLEVNR-ALPHA = 'DJKLA'                                         
410300        MOVE '804  ' TO W-IDLEVNR-ALPHA  END-IF                           
410400     IF W-IDLEVNR-ALPHA = 'BN6ZA'                                         
410500        MOVE '944  ' TO W-IDLEVNR-ALPHA  END-IF                           
410600     IF W-IDLEVNR-ALPHA = 'DL0PA'                                         
410700        MOVE '948  ' TO W-IDLEVNR-ALPHA  END-IF                           
410800     IF W-IDLEVNR-ALPHA = 'BSKZA'                                         
410900        MOVE '971  ' TO W-IDLEVNR-ALPHA  END-IF                           
411000     IF W-IDLEVNR-ALPHA = 'DL0TA'                                         
411100        MOVE '1025 ' TO W-IDLEVNR-ALPHA  END-IF                           
411200     IF W-IDLEVNR-ALPHA = 'BZCEA'                                         
411300        MOVE '1068 ' TO W-IDLEVNR-ALPHA  END-IF                           
411400     IF W-IDLEVNR-ALPHA = 'DJKMA'                                         
411500        MOVE '1102 ' TO W-IDLEVNR-ALPHA  END-IF                           
411600     IF W-IDLEVNR-ALPHA = 'DL0UA'                                         
411700        MOVE '1160 ' TO W-IDLEVNR-ALPHA  END-IF                           
411800     IF W-IDLEVNR-ALPHA = 'BKMEA'                                         
411900        MOVE '1232 ' TO W-IDLEVNR-ALPHA  END-IF                           
412000     IF W-IDLEVNR-ALPHA = 'BJTTA'                                         
412100        MOVE '1293 ' TO W-IDLEVNR-ALPHA  END-IF                           
412200     IF W-IDLEVNR-ALPHA = 'BKMJA'                                         
412300        MOVE '1304 ' TO W-IDLEVNR-ALPHA  END-IF                           
412400     IF W-IDLEVNR-ALPHA = 'BMN7B'                                         
412500        MOVE '1331 ' TO W-IDLEVNR-ALPHA  END-IF                           
412600     IF W-IDLEVNR-ALPHA = 'BX4TA'                                         
412700        MOVE '1512 ' TO W-IDLEVNR-ALPHA  END-IF                           
412800     IF W-IDLEVNR-ALPHA = 'DJKNA'                                         
412900        MOVE '1523 ' TO W-IDLEVNR-ALPHA  END-IF                           
413000     IF W-IDLEVNR-ALPHA = 'DJKPA'                                         
413100        MOVE '1542 ' TO W-IDLEVNR-ALPHA  END-IF                           
413200     IF W-IDLEVNR-ALPHA = 'DJKQA'                                         
413300        MOVE '1606 ' TO W-IDLEVNR-ALPHA  END-IF                           
413400     IF W-IDLEVNR-ALPHA = 'BKEBA'                                         
413500        MOVE '1633 ' TO W-IDLEVNR-ALPHA  END-IF                           
413600     IF W-IDLEVNR-ALPHA = 'BJZQA'                                         
413700        MOVE '1699 ' TO W-IDLEVNR-ALPHA  END-IF                           
413800     IF W-IDLEVNR-ALPHA = 'DJKRA'                                         
413900        MOVE '1756 ' TO W-IDLEVNR-ALPHA  END-IF                           
414000     IF W-IDLEVNR-ALPHA = 'DJKSA'                                         
414100        MOVE '1764 ' TO W-IDLEVNR-ALPHA  END-IF                           
414200     IF W-IDLEVNR-ALPHA = 'BJ9ZA'                                         
414300        MOVE '1887 ' TO W-IDLEVNR-ALPHA  END-IF                           
414400     IF W-IDLEVNR-ALPHA = 'BKMUA'                                         
414500        MOVE '1916 ' TO W-IDLEVNR-ALPHA  END-IF                           
414600     IF W-IDLEVNR-ALPHA = 'DL0XA'                                         
414700        MOVE '2028 ' TO W-IDLEVNR-ALPHA  END-IF                           
414800     IF W-IDLEVNR-ALPHA = 'BLWMA'                                         
414900        MOVE '2055 ' TO W-IDLEVNR-ALPHA  END-IF                           
415000     IF W-IDLEVNR-ALPHA = 'BJQTA'                                         
415100        MOVE '2177 ' TO W-IDLEVNR-ALPHA  END-IF                           
415200     IF W-IDLEVNR-ALPHA = 'BJPDA'                                         
415300        MOVE '2229 ' TO W-IDLEVNR-ALPHA  END-IF                           
415400     IF W-IDLEVNR-ALPHA = 'CZ4BA'                                         
415500        MOVE '2243 ' TO W-IDLEVNR-ALPHA  END-IF                           
415600     IF W-IDLEVNR-ALPHA = 'DJKTA'                                         
415700        MOVE '2284 ' TO W-IDLEVNR-ALPHA  END-IF                           
415800     IF W-IDLEVNR-ALPHA = 'BK4LA'                                         
415900        MOVE '2322 ' TO W-IDLEVNR-ALPHA  END-IF                           
416000     IF W-IDLEVNR-ALPHA = 'DL0ZA'                                         
416100        MOVE '2344 ' TO W-IDLEVNR-ALPHA  END-IF                           
416200     IF W-IDLEVNR-ALPHA = 'DJKVA'                                         
416300        MOVE '2446 ' TO W-IDLEVNR-ALPHA  END-IF                           
416400     IF W-IDLEVNR-ALPHA = 'BJKNA'                                         
416500        MOVE '2451 ' TO W-IDLEVNR-ALPHA  END-IF                           
416600     IF W-IDLEVNR-ALPHA = 'CD2HA'                                         
416700        MOVE '2480 ' TO W-IDLEVNR-ALPHA  END-IF                           
416800     IF W-IDLEVNR-ALPHA = 'BJYVA'                                         
416900        MOVE '2619 ' TO W-IDLEVNR-ALPHA  END-IF                           
417000     IF W-IDLEVNR-ALPHA = 'BL3AA'                                         
417100        MOVE '2669 ' TO W-IDLEVNR-ALPHA  END-IF                           
417200     IF W-IDLEVNR-ALPHA = 'BLNLA'                                         
417300        MOVE '3105 ' TO W-IDLEVNR-ALPHA  END-IF                           
417400     IF W-IDLEVNR-ALPHA = 'DJKWA'                                         
417500        MOVE '3304 ' TO W-IDLEVNR-ALPHA  END-IF                           
417600     IF W-IDLEVNR-ALPHA = 'DL1RA'                                         
417700        MOVE '3310 ' TO W-IDLEVNR-ALPHA  END-IF                           
417800     IF W-IDLEVNR-ALPHA = 'DL1SA'                                         
417900        MOVE '3332 ' TO W-IDLEVNR-ALPHA  END-IF                           
418000     IF W-IDLEVNR-ALPHA = 'DEV4A'                                         
418100        MOVE '3342 ' TO W-IDLEVNR-ALPHA  END-IF                           
418200     IF W-IDLEVNR-ALPHA = 'DJKXA'                                         
418300        MOVE '3343 ' TO W-IDLEVNR-ALPHA  END-IF                           
418400     IF W-IDLEVNR-ALPHA = 'DJKYA'                                         
418500        MOVE '3345 ' TO W-IDLEVNR-ALPHA  END-IF                           
418600     IF W-IDLEVNR-ALPHA = 'DL1TA'                                         
418700        MOVE '3349 ' TO W-IDLEVNR-ALPHA  END-IF                           
418800     IF W-IDLEVNR-ALPHA = 'DL1UA'                                         
418900        MOVE '3354 ' TO W-IDLEVNR-ALPHA  END-IF                           
419000     IF W-IDLEVNR-ALPHA = 'DJKZA'                                         
419100        MOVE '3369 ' TO W-IDLEVNR-ALPHA  END-IF                           
419200     IF W-IDLEVNR-ALPHA = 'DJK0A'                                         
419300        MOVE '3375 ' TO W-IDLEVNR-ALPHA  END-IF                           
419400     IF W-IDLEVNR-ALPHA = 'DL1VA'                                         
419500        MOVE '3376 ' TO W-IDLEVNR-ALPHA  END-IF                           
419600     IF W-IDLEVNR-ALPHA = 'BQ6SA'                                         
419700        MOVE '3389 ' TO W-IDLEVNR-ALPHA  END-IF                           
419800     IF W-IDLEVNR-ALPHA = 'DJK9A'                                         
419900        MOVE '3401 ' TO W-IDLEVNR-ALPHA  END-IF                           
420000     IF W-IDLEVNR-ALPHA = 'CFJCA'                                         
420100        MOVE '3405 ' TO W-IDLEVNR-ALPHA  END-IF                           
420200     IF W-IDLEVNR-ALPHA = 'BK1EA'                                         
420300        MOVE '3421 ' TO W-IDLEVNR-ALPHA  END-IF                           
420400     IF W-IDLEVNR-ALPHA = 'DJLAA'                                         
420500        MOVE '3423 ' TO W-IDLEVNR-ALPHA  END-IF                           
420600     IF W-IDLEVNR-ALPHA = 'DL1XA'                                         
420700        MOVE '3424 ' TO W-IDLEVNR-ALPHA  END-IF                           
420800*************************************************                         
420900     IF W-IDLEVNR-ALPHA = 'BP8CA'                                         
421000        MOVE '1335 ' TO W-IDLEVNR-ALPHA  END-IF                           
421100     IF W-IDLEVNR-ALPHA = 'BWKFA'                                         
421200        MOVE '1649 ' TO W-IDLEVNR-ALPHA  END-IF                           
421300     IF W-IDLEVNR-ALPHA = 'G13FA'                                         
421400        MOVE '1797 ' TO W-IDLEVNR-ALPHA  END-IF                           
421500     IF W-IDLEVNR-ALPHA = 'BLMPA'                                         
421600        MOVE '2113 ' TO W-IDLEVNR-ALPHA  END-IF                           
421700     IF W-IDLEVNR-ALPHA = 'BQ7HA'                                         
421800        MOVE '2302 ' TO W-IDLEVNR-ALPHA  END-IF                           
421900     IF W-IDLEVNR-ALPHA = 'M279D'                                         
422000        MOVE '3556 ' TO W-IDLEVNR-ALPHA  END-IF                           
422100     IF W-IDLEVNR-ALPHA = 'AB7ZA'                                         
422200        MOVE '4760 ' TO W-IDLEVNR-ALPHA  END-IF                           
422300     IF W-IDLEVNR-ALPHA = 'D0UQA'                                         
422400        MOVE '6191 ' TO W-IDLEVNR-ALPHA  END-IF                           
422500     IF W-IDLEVNR-ALPHA = 'P511A'                                         
422600        MOVE '6354 ' TO W-IDLEVNR-ALPHA  END-IF                           
422700     IF W-IDLEVNR-ALPHA = 'ADBTA'                                         
422800        MOVE '6505 ' TO W-IDLEVNR-ALPHA  END-IF                           
422900     IF W-IDLEVNR-ALPHA = 'B42KA'                                         
423000        MOVE '6512 ' TO W-IDLEVNR-ALPHA  END-IF                           
423100     IF W-IDLEVNR-ALPHA = 'K1FWA'                                         
423200        MOVE '6589 ' TO W-IDLEVNR-ALPHA  END-IF                           
423300     IF W-IDLEVNR-ALPHA = 'M1F7A'                                         
423400        MOVE '6608 ' TO W-IDLEVNR-ALPHA  END-IF                           
423500     IF W-IDLEVNR-ALPHA = 'CX9XA'                                         
423600        MOVE '6650 ' TO W-IDLEVNR-ALPHA  END-IF                           
423700     IF W-IDLEVNR-ALPHA = 'P790A'                                         
423800        MOVE '6665 ' TO W-IDLEVNR-ALPHA  END-IF                           
423900     IF W-IDLEVNR-ALPHA = 'S106A'                                         
424000        MOVE '6669 ' TO W-IDLEVNR-ALPHA  END-IF                           
424100     IF W-IDLEVNR-ALPHA = 'J613A'                                         
424200        MOVE '6684 ' TO W-IDLEVNR-ALPHA  END-IF                           
424300     IF W-IDLEVNR-ALPHA = 'BZ02A'                                         
424400        MOVE '6692 ' TO W-IDLEVNR-ALPHA  END-IF                           
424500     IF W-IDLEVNR-ALPHA = 'B4W7A'                                         
424600        MOVE '6823 ' TO W-IDLEVNR-ALPHA  END-IF                           
424700     IF W-IDLEVNR-ALPHA = 'D0QWA'                                         
424800        MOVE '7277 ' TO W-IDLEVNR-ALPHA  END-IF                           
424900     IF W-IDLEVNR-ALPHA = 'BCSFA'                                         
425000        MOVE '7317 ' TO W-IDLEVNR-ALPHA  END-IF                           
425100     IF W-IDLEVNR-ALPHA = 'BPTMB'                                         
425200        MOVE '7900 ' TO W-IDLEVNR-ALPHA  END-IF                           
425300     IF W-IDLEVNR-ALPHA = 'BPTMD'                                         
425400        MOVE '10159' TO W-IDLEVNR-ALPHA  END-IF                           
425500     IF W-IDLEVNR-ALPHA = 'BPTMA'                                         
425600        MOVE '10160' TO W-IDLEVNR-ALPHA  END-IF                           
425700     IF W-IDLEVNR-ALPHA = 'BPTMC'                                         
425800        MOVE '11377' TO W-IDLEVNR-ALPHA  END-IF                           
425900     IF W-IDLEVNR-ALPHA = 'BP8DB'                                         
426000        MOVE '13585' TO W-IDLEVNR-ALPHA  END-IF                           
426100     IF W-IDLEVNR-ALPHA = 'BP8DD'                                         
426200        MOVE '13622' TO W-IDLEVNR-ALPHA  END-IF                           
426300     IF W-IDLEVNR-ALPHA = 'T8LLA'                                         
426400        MOVE '13801' TO W-IDLEVNR-ALPHA  END-IF                           
426500     IF W-IDLEVNR-ALPHA = 'D06MA'                                         
426600        MOVE '13849' TO W-IDLEVNR-ALPHA  END-IF                           
426700     IF W-IDLEVNR-ALPHA = 'B47PG'                                         
426800        MOVE '14280' TO W-IDLEVNR-ALPHA  END-IF                           
426900     IF W-IDLEVNR-ALPHA = 'CNXXA'                                         
427000        MOVE '14500' TO W-IDLEVNR-ALPHA  END-IF                           
427100     IF W-IDLEVNR-ALPHA = 'BPW0A'                                         
427200        MOVE '14519' TO W-IDLEVNR-ALPHA  END-IF                           
427300     IF W-IDLEVNR-ALPHA = 'C9D2A'                                         
427400        MOVE '14944' TO W-IDLEVNR-ALPHA  END-IF                           
427500     IF W-IDLEVNR-ALPHA = 'BPTME'                                         
427600        MOVE '16036' TO W-IDLEVNR-ALPHA  END-IF                           
427700     IF W-IDLEVNR-ALPHA = 'M279C'                                         
427800        MOVE '16144' TO W-IDLEVNR-ALPHA  END-IF                           
427900     IF W-IDLEVNR-ALPHA = 'M279E'                                         
428000        MOVE '16145' TO W-IDLEVNR-ALPHA  END-IF                           
428100     IF W-IDLEVNR-ALPHA = 'U2W5B'                                         
428200        MOVE '16274' TO W-IDLEVNR-ALPHA  END-IF                           
428300     IF W-IDLEVNR-ALPHA = 'K1FWB'                                         
428400        MOVE '16332' TO W-IDLEVNR-ALPHA  END-IF                           
428500     IF W-IDLEVNR-ALPHA = 'D059D'                                         
428600        MOVE '19564' TO W-IDLEVNR-ALPHA  END-IF                           
428700     IF W-IDLEVNR-ALPHA = 'BP8DC'                                         
428800        MOVE '19609' TO W-IDLEVNR-ALPHA  END-IF                           
428900     IF W-IDLEVNR-ALPHA = 'G1UHN'                                         
429000        MOVE '21590' TO W-IDLEVNR-ALPHA  END-IF                           
429100     IF W-IDLEVNR-ALPHA = 'D30FA'                                         
429200        MOVE '22419' TO W-IDLEVNR-ALPHA  END-IF                           
429300     IF W-IDLEVNR-ALPHA = 'D01QA'                                         
429400        MOVE '22420' TO W-IDLEVNR-ALPHA  END-IF                           
429500     IF W-IDLEVNR-ALPHA = 'D059E'                                         
429600        MOVE '23375' TO W-IDLEVNR-ALPHA  END-IF                           
429700     IF W-IDLEVNR-ALPHA = 'BQ1ZC'                                         
429800        MOVE '23919' TO W-IDLEVNR-ALPHA  END-IF                           
429900     IF W-IDLEVNR-ALPHA = 'D059F'                                         
430000        MOVE '23926' TO W-IDLEVNR-ALPHA  END-IF                           
430100     IF W-IDLEVNR-ALPHA = 'ABD3A'                                         
430200        MOVE '25944' TO W-IDLEVNR-ALPHA  END-IF                           
430300     IF W-IDLEVNR-ALPHA = 'P790M'                                         
430400        MOVE '16149' TO W-IDLEVNR-ALPHA  END-IF                           
430500*************************************************                         
430600     IF W-IDLEVNR-ALPHA = 'DL1YA'                                         
430700        MOVE '3445 ' TO W-IDLEVNR-ALPHA  END-IF                           
430800     IF W-IDLEVNR-ALPHA = 'DLH6A'                                         
430900        MOVE '3449 ' TO W-IDLEVNR-ALPHA  END-IF                           
431000     IF W-IDLEVNR-ALPHA = 'DLH7A'                                         
431100        MOVE '3463 ' TO W-IDLEVNR-ALPHA  END-IF                           
431200     IF W-IDLEVNR-ALPHA = 'DL1ZA'                                         
431300        MOVE '3470 ' TO W-IDLEVNR-ALPHA  END-IF                           
431400     IF W-IDLEVNR-ALPHA = 'DLJAA'                                         
431500        MOVE '3474 ' TO W-IDLEVNR-ALPHA  END-IF                           
431600     IF W-IDLEVNR-ALPHA = 'DLJBA'                                         
431700        MOVE '3482 ' TO W-IDLEVNR-ALPHA  END-IF                           
431800     IF W-IDLEVNR-ALPHA = 'DLJCA'                                         
431900        MOVE '3485 ' TO W-IDLEVNR-ALPHA  END-IF                           
432000     IF W-IDLEVNR-ALPHA = 'DLJDA'                                         
432100        MOVE '3487 ' TO W-IDLEVNR-ALPHA  END-IF                           
432200     IF W-IDLEVNR-ALPHA = 'DLJEA'                                         
432300        MOVE '3495 ' TO W-IDLEVNR-ALPHA  END-IF                           
432400     IF W-IDLEVNR-ALPHA = 'DL2BA'                                         
432500        MOVE '3505 ' TO W-IDLEVNR-ALPHA  END-IF                           
432600     IF W-IDLEVNR-ALPHA = 'BEFXA'                                         
432700        MOVE '3511 ' TO W-IDLEVNR-ALPHA  END-IF                           
432800     IF W-IDLEVNR-ALPHA = 'LYSDA'                                         
432900        MOVE '3537 ' TO W-IDLEVNR-ALPHA  END-IF                           
433000     IF W-IDLEVNR-ALPHA = 'DL2DA'                                         
433100        MOVE '3581 ' TO W-IDLEVNR-ALPHA  END-IF                           
433200     IF W-IDLEVNR-ALPHA = 'KYBHA'                                         
433300        MOVE '3634 ' TO W-IDLEVNR-ALPHA  END-IF                           
433400     IF W-IDLEVNR-ALPHA = 'DLJFA'                                         
433500        MOVE '3641 ' TO W-IDLEVNR-ALPHA  END-IF                           
433600     IF W-IDLEVNR-ALPHA = 'F745A'                                         
433700        MOVE '3660 ' TO W-IDLEVNR-ALPHA  END-IF                           
433800     IF W-IDLEVNR-ALPHA = 'E23VB'                                         
433900        MOVE '3663 ' TO W-IDLEVNR-ALPHA  END-IF                           
434000     IF W-IDLEVNR-ALPHA = 'DLJHA'                                         
434100        MOVE '3681 ' TO W-IDLEVNR-ALPHA  END-IF                           
434200     IF W-IDLEVNR-ALPHA = 'DL2EA'                                         
434300        MOVE '3723 ' TO W-IDLEVNR-ALPHA  END-IF                           
434400     IF W-IDLEVNR-ALPHA = 'DL2FA'                                         
434500        MOVE '3728 ' TO W-IDLEVNR-ALPHA  END-IF                           
434600     IF W-IDLEVNR-ALPHA = 'DL2GA'                                         
434700        MOVE '3732 ' TO W-IDLEVNR-ALPHA  END-IF                           
434800     IF W-IDLEVNR-ALPHA = 'CYMBD'                                         
434900        MOVE '3751 ' TO W-IDLEVNR-ALPHA  END-IF                           
435000     IF W-IDLEVNR-ALPHA = 'DL2HA'                                         
435100        MOVE '3771 ' TO W-IDLEVNR-ALPHA  END-IF                           
435200     IF W-IDLEVNR-ALPHA = 'DLJJA'                                         
435300        MOVE '3790 ' TO W-IDLEVNR-ALPHA  END-IF                           
435400     IF W-IDLEVNR-ALPHA = 'DL2JA'                                         
435500        MOVE '3793 ' TO W-IDLEVNR-ALPHA  END-IF                           
435600     IF W-IDLEVNR-ALPHA = 'DL2KA'                                         
435700        MOVE '3799 ' TO W-IDLEVNR-ALPHA  END-IF                           
435800     IF W-IDLEVNR-ALPHA = 'DL2LA'                                         
435900        MOVE '3814 ' TO W-IDLEVNR-ALPHA  END-IF                           
436000     IF W-IDLEVNR-ALPHA = 'DL2MA'                                         
436100        MOVE '3830 ' TO W-IDLEVNR-ALPHA  END-IF                           
436200     IF W-IDLEVNR-ALPHA = 'DL2NA'                                         
436300        MOVE '3833 ' TO W-IDLEVNR-ALPHA  END-IF                           
436400     IF W-IDLEVNR-ALPHA = 'DLJKA'                                         
436500        MOVE '3855 ' TO W-IDLEVNR-ALPHA  END-IF                           
436600     IF W-IDLEVNR-ALPHA = 'CP6JB'                                         
436700        MOVE '3861 ' TO W-IDLEVNR-ALPHA  END-IF                           
436800     IF W-IDLEVNR-ALPHA = 'DL2PA'                                         
436900        MOVE '3866 ' TO W-IDLEVNR-ALPHA  END-IF                           
437000     IF W-IDLEVNR-ALPHA = 'R57KA'                                         
437100        MOVE '3925 ' TO W-IDLEVNR-ALPHA  END-IF                           
437200     IF W-IDLEVNR-ALPHA = 'D0UCC'                                         
437300        MOVE '3933 ' TO W-IDLEVNR-ALPHA  END-IF                           
437400     IF W-IDLEVNR-ALPHA = 'DL4SA'                                         
437500        MOVE '3938 ' TO W-IDLEVNR-ALPHA  END-IF                           
437600     IF W-IDLEVNR-ALPHA = 'DLJMA'                                         
437700        MOVE '3941 ' TO W-IDLEVNR-ALPHA  END-IF                           
437800     IF W-IDLEVNR-ALPHA = 'DL4TA'                                         
437900        MOVE '3952 ' TO W-IDLEVNR-ALPHA  END-IF                           
438000     IF W-IDLEVNR-ALPHA = 'DLJPA'                                         
438100        MOVE '3957 ' TO W-IDLEVNR-ALPHA  END-IF                           
438200     IF W-IDLEVNR-ALPHA = 'DL5GB'                                         
438300        MOVE '3970 ' TO W-IDLEVNR-ALPHA  END-IF                           
438400     IF W-IDLEVNR-ALPHA = 'LEMWA'                                         
438500        MOVE '3977 ' TO W-IDLEVNR-ALPHA  END-IF                           
438600     IF W-IDLEVNR-ALPHA = 'S601E'                                         
438700        MOVE '4148 ' TO W-IDLEVNR-ALPHA  END-IF                           
438800     IF W-IDLEVNR-ALPHA = 'G8KDA'                                         
438900        MOVE '4256 ' TO W-IDLEVNR-ALPHA  END-IF                           
439000     IF W-IDLEVNR-ALPHA = 'LRT1A'                                         
439100        MOVE '4488 ' TO W-IDLEVNR-ALPHA  END-IF                           
439200     IF W-IDLEVNR-ALPHA = 'DL5HA'                                         
439300        MOVE '4542 ' TO W-IDLEVNR-ALPHA  END-IF                           
439400     IF W-IDLEVNR-ALPHA = 'CFJBA'                                         
439500        MOVE '4637 ' TO W-IDLEVNR-ALPHA  END-IF                           
439600     IF W-IDLEVNR-ALPHA = 'DLJRA'                                         
439700        MOVE '4700 ' TO W-IDLEVNR-ALPHA  END-IF                           
439800     IF W-IDLEVNR-ALPHA = 'JHZZA'                                         
439900        MOVE '4749 ' TO W-IDLEVNR-ALPHA  END-IF                           
440000     IF W-IDLEVNR-ALPHA = 'R6PFB'                                         
440100        MOVE '4845 ' TO W-IDLEVNR-ALPHA  END-IF                           
440200     IF W-IDLEVNR-ALPHA = 'D5Q3F'                                         
440300        MOVE '4934 ' TO W-IDLEVNR-ALPHA  END-IF                           
440400     IF W-IDLEVNR-ALPHA = 'DL6FA'                                         
440500        MOVE '4968 ' TO W-IDLEVNR-ALPHA  END-IF                           
440600     IF W-IDLEVNR-ALPHA = 'DL6JA'                                         
440700        MOVE '5122 ' TO W-IDLEVNR-ALPHA  END-IF                           
440800     IF W-IDLEVNR-ALPHA = 'LRVLA'                                         
440900        MOVE '5161 ' TO W-IDLEVNR-ALPHA  END-IF                           
441000     IF W-IDLEVNR-ALPHA = 'L3PGE'                                         
441100        MOVE '5182 ' TO W-IDLEVNR-ALPHA  END-IF                           
441200     IF W-IDLEVNR-ALPHA = 'D21XA'                                         
441300        MOVE '5233 ' TO W-IDLEVNR-ALPHA  END-IF                           
441400     IF W-IDLEVNR-ALPHA = 'C8W0A'                                         
441500        MOVE '5295 ' TO W-IDLEVNR-ALPHA  END-IF                           
441600     IF W-IDLEVNR-ALPHA = 'LHSYA'                                         
441700        MOVE '5354 ' TO W-IDLEVNR-ALPHA  END-IF                           
441800     IF W-IDLEVNR-ALPHA = 'AADLA'                                         
441900        MOVE '5436 ' TO W-IDLEVNR-ALPHA  END-IF                           
442000     IF W-IDLEVNR-ALPHA = 'DLJSA'                                         
442100        MOVE '5443 ' TO W-IDLEVNR-ALPHA  END-IF                           
442200     IF W-IDLEVNR-ALPHA = 'DLJTA'                                         
442300        MOVE '5637 ' TO W-IDLEVNR-ALPHA  END-IF                           
442400     IF W-IDLEVNR-ALPHA = 'D26YA'                                         
442500        MOVE '5671 ' TO W-IDLEVNR-ALPHA  END-IF                           
442600     IF W-IDLEVNR-ALPHA = 'MRZ4A'                                         
442700        MOVE '5678 ' TO W-IDLEVNR-ALPHA  END-IF                           
442800     IF W-IDLEVNR-ALPHA = 'DLK7A'                                         
442900        MOVE '6016 ' TO W-IDLEVNR-ALPHA  END-IF                           
443000     IF W-IDLEVNR-ALPHA = 'D1H7A'                                         
443100        MOVE '6024 ' TO W-IDLEVNR-ALPHA  END-IF                           
443200     IF W-IDLEVNR-ALPHA = 'DLLAA'                                         
443300        MOVE '6045 ' TO W-IDLEVNR-ALPHA  END-IF                           
443400     IF W-IDLEVNR-ALPHA = 'B42DA'                                         
443500        MOVE '6053 ' TO W-IDLEVNR-ALPHA  END-IF                           
443600     IF W-IDLEVNR-ALPHA = 'C8F4A'                                         
443700        MOVE '6061 ' TO W-IDLEVNR-ALPHA  END-IF                           
443800     IF W-IDLEVNR-ALPHA = 'EGX6K'                                         
443900        MOVE '6066 ' TO W-IDLEVNR-ALPHA  END-IF                           
444000     IF W-IDLEVNR-ALPHA = 'DLLBA'                                         
444100        MOVE '6069 ' TO W-IDLEVNR-ALPHA  END-IF                           
444200     IF W-IDLEVNR-ALPHA = 'B40WB'                                         
444300        MOVE '6080 ' TO W-IDLEVNR-ALPHA  END-IF                           
444400     IF W-IDLEVNR-ALPHA = 'D04DA'                                         
444500        MOVE '6098 ' TO W-IDLEVNR-ALPHA  END-IF                           
444600     IF W-IDLEVNR-ALPHA = 'D33QB'                                         
444700        MOVE '6120 ' TO W-IDLEVNR-ALPHA  END-IF                           
444800     IF W-IDLEVNR-ALPHA = 'D23JB'                                         
444900        MOVE '6151 ' TO W-IDLEVNR-ALPHA  END-IF                           
445000     IF W-IDLEVNR-ALPHA = 'DZK7A'                                         
445100        MOVE '6158 ' TO W-IDLEVNR-ALPHA  END-IF                           
445200     IF W-IDLEVNR-ALPHA = 'C8S2B'                                         
445300        MOVE '6202 ' TO W-IDLEVNR-ALPHA  END-IF                           
445400     IF W-IDLEVNR-ALPHA = 'D8NLJ'                                         
445500        MOVE '6228 ' TO W-IDLEVNR-ALPHA  END-IF                           
445600     IF W-IDLEVNR-ALPHA = 'DLLCA'                                         
445700        MOVE '6245 ' TO W-IDLEVNR-ALPHA  END-IF                           
445800     IF W-IDLEVNR-ALPHA = 'D0NNA'                                         
445900        MOVE '6269 ' TO W-IDLEVNR-ALPHA  END-IF                           
446000     IF W-IDLEVNR-ALPHA = 'D0SYA'                                         
446100        MOVE '6283 ' TO W-IDLEVNR-ALPHA  END-IF                           
446200*************************************************                         
446300     IF W-IDLEVNR-ALPHA = 'BQ8YA'                                         
446400        MOVE '80   ' TO W-IDLEVNR-ALPHA  END-IF                           
446500     IF W-IDLEVNR-ALPHA = 'BQ0FA'                                         
446600        MOVE '511  ' TO W-IDLEVNR-ALPHA  END-IF                           
446700     IF W-IDLEVNR-ALPHA = 'BQAGA'                                         
446800        MOVE '894  ' TO W-IDLEVNR-ALPHA  END-IF                           
446900     IF W-IDLEVNR-ALPHA = 'S5PQB'                                         
447000        MOVE '1345 ' TO W-IDLEVNR-ALPHA  END-IF                           
447100     IF W-IDLEVNR-ALPHA = 'LESLA'                                         
447200        MOVE '2333 ' TO W-IDLEVNR-ALPHA  END-IF                           
447300     IF W-IDLEVNR-ALPHA = 'BQ6MA'                                         
447400        MOVE '3050 ' TO W-IDLEVNR-ALPHA  END-IF                           
447500     IF W-IDLEVNR-ALPHA = 'L9GXB'                                         
447600        MOVE '3135 ' TO W-IDLEVNR-ALPHA  END-IF                           
447700     IF W-IDLEVNR-ALPHA = 'L9GXA'                                         
447800        MOVE '3594 ' TO W-IDLEVNR-ALPHA  END-IF                           
447900     IF W-IDLEVNR-ALPHA = 'C9A3A'                                         
448000        MOVE '3616 ' TO W-IDLEVNR-ALPHA  END-IF                           
448100     IF W-IDLEVNR-ALPHA = 'S356A'                                         
448200        MOVE '3752 ' TO W-IDLEVNR-ALPHA  END-IF                           
448300     IF W-IDLEVNR-ALPHA = 'AYSCB'                                         
448400        MOVE '3755 ' TO W-IDLEVNR-ALPHA  END-IF                           
448500     IF W-IDLEVNR-ALPHA = 'E23LA'                                         
448600        MOVE '3912 ' TO W-IDLEVNR-ALPHA  END-IF                           
448700     IF W-IDLEVNR-ALPHA = 'D04JA'                                         
448800        MOVE '4515 ' TO W-IDLEVNR-ALPHA  END-IF                           
448900     IF W-IDLEVNR-ALPHA = 'A628A'                                         
449000        MOVE '5049 ' TO W-IDLEVNR-ALPHA  END-IF                           
449100     IF W-IDLEVNR-ALPHA = 'C66SJ'                                         
449200        MOVE '5085 ' TO W-IDLEVNR-ALPHA  END-IF                           
449300     IF W-IDLEVNR-ALPHA = 'D3C3A'                                         
449400        MOVE '5093 ' TO W-IDLEVNR-ALPHA  END-IF                           
449500     IF W-IDLEVNR-ALPHA = 'C8V8A'                                         
449600        MOVE '5744 ' TO W-IDLEVNR-ALPHA  END-IF                           
449700     IF W-IDLEVNR-ALPHA = 'F4SWG'                                         
449800        MOVE '6022 ' TO W-IDLEVNR-ALPHA  END-IF                           
449900     IF W-IDLEVNR-ALPHA = 'AN3AA'                                         
450000        MOVE '6118 ' TO W-IDLEVNR-ALPHA  END-IF                           
450100     IF W-IDLEVNR-ALPHA = 'H518X'                                         
450200        MOVE '6215 ' TO W-IDLEVNR-ALPHA  END-IF                           
450300     IF W-IDLEVNR-ALPHA = 'CJ6EA'                                         
450400        MOVE '6345 ' TO W-IDLEVNR-ALPHA  END-IF                           
450500     IF W-IDLEVNR-ALPHA = 'B492E'                                         
450600        MOVE '6538 ' TO W-IDLEVNR-ALPHA  END-IF                           
450700     IF W-IDLEVNR-ALPHA = 'B492A'                                         
450800        MOVE '6587 ' TO W-IDLEVNR-ALPHA  END-IF                           
450900     IF W-IDLEVNR-ALPHA = 'C8W2A'                                         
451000        MOVE '7213 ' TO W-IDLEVNR-ALPHA  END-IF                           
451100     IF W-IDLEVNR-ALPHA = 'CFT5A'                                         
451200        MOVE '7609 ' TO W-IDLEVNR-ALPHA  END-IF                           
451300     IF W-IDLEVNR-ALPHA = 'BPLDA'                                         
451400        MOVE '10131' TO W-IDLEVNR-ALPHA  END-IF                           
451500     IF W-IDLEVNR-ALPHA = 'BPLDD'                                         
451600        MOVE '10138' TO W-IDLEVNR-ALPHA  END-IF                           
451700     IF W-IDLEVNR-ALPHA = 'CRK8A'                                         
451800        MOVE '11099' TO W-IDLEVNR-ALPHA  END-IF                           
451900     IF W-IDLEVNR-ALPHA = 'BPLDC'                                         
452000        MOVE '13633' TO W-IDLEVNR-ALPHA  END-IF                           
452100     IF W-IDLEVNR-ALPHA = 'C7U7A'                                         
452200        MOVE '14493' TO W-IDLEVNR-ALPHA  END-IF                           
452300     IF W-IDLEVNR-ALPHA = 'C685B'                                         
452400        MOVE '16213' TO W-IDLEVNR-ALPHA  END-IF                           
452500     IF W-IDLEVNR-ALPHA = 'R19YA'                                         
452600        MOVE '17779' TO W-IDLEVNR-ALPHA  END-IF                           
452700     IF W-IDLEVNR-ALPHA = 'BTTTA'                                         
452800        MOVE '23071' TO W-IDLEVNR-ALPHA  END-IF                           
452900     IF W-IDLEVNR-ALPHA = 'CYVBA'                                         
453000        MOVE '23937' TO W-IDLEVNR-ALPHA  END-IF                           
453100     IF W-IDLEVNR-ALPHA = 'DSKFA'                                         
453200        MOVE '26013' TO W-IDLEVNR-ALPHA  END-IF                           
453300*************************************************                         
453400     IF W-IDLEVNR-ALPHA = 'E520A'                                         
453500        MOVE '6293 ' TO W-IDLEVNR-ALPHA  END-IF                           
453600     IF W-IDLEVNR-ALPHA = 'DL6MA'                                         
453700        MOVE '6307 ' TO W-IDLEVNR-ALPHA  END-IF                           
453800     IF W-IDLEVNR-ALPHA = 'H8Z2A'                                         
453900        MOVE '6326 ' TO W-IDLEVNR-ALPHA  END-IF                           
454000     IF W-IDLEVNR-ALPHA = 'DL6PA'                                         
454100        MOVE '6357 ' TO W-IDLEVNR-ALPHA  END-IF                           
454200     IF W-IDLEVNR-ALPHA = 'DMZCA'                                         
454300        MOVE '6410 ' TO W-IDLEVNR-ALPHA  END-IF                           
454400     IF W-IDLEVNR-ALPHA = 'U0VSA'                                         
454500        MOVE '6425 ' TO W-IDLEVNR-ALPHA  END-IF                           
454600     IF W-IDLEVNR-ALPHA = 'C91WA'                                         
454700        MOVE '6429 ' TO W-IDLEVNR-ALPHA  END-IF                           
454800     IF W-IDLEVNR-ALPHA = 'DLLFA'                                         
454900        MOVE '6430 ' TO W-IDLEVNR-ALPHA  END-IF                           
455000     IF W-IDLEVNR-ALPHA = 'V04BA'                                         
455100        MOVE '6510 ' TO W-IDLEVNR-ALPHA  END-IF                           
455200     IF W-IDLEVNR-ALPHA = 'C7F4A'                                         
455300        MOVE '6524 ' TO W-IDLEVNR-ALPHA  END-IF                           
455400     IF W-IDLEVNR-ALPHA = 'DLLGA'                                         
455500        MOVE '6562 ' TO W-IDLEVNR-ALPHA  END-IF                           
455600     IF W-IDLEVNR-ALPHA = 'L9SYA'                                         
455700        MOVE '6595 ' TO W-IDLEVNR-ALPHA  END-IF                           
455800     IF W-IDLEVNR-ALPHA = 'DLLJA'                                         
455900        MOVE '6603 ' TO W-IDLEVNR-ALPHA  END-IF                           
456000     IF W-IDLEVNR-ALPHA = 'DLLKA'                                         
456100        MOVE '6641 ' TO W-IDLEVNR-ALPHA  END-IF                           
456200     IF W-IDLEVNR-ALPHA = 'DLLLA'                                         
456300        MOVE '6643 ' TO W-IDLEVNR-ALPHA  END-IF                           
456400     IF W-IDLEVNR-ALPHA = 'CKBRA'                                         
456500        MOVE '6656 ' TO W-IDLEVNR-ALPHA  END-IF                           
456600     IF W-IDLEVNR-ALPHA = 'JFYXA'                                         
456700        MOVE '6691 ' TO W-IDLEVNR-ALPHA  END-IF                           
456800     IF W-IDLEVNR-ALPHA = 'C8Q3A'                                         
456900        MOVE '6698 ' TO W-IDLEVNR-ALPHA  END-IF                           
457000     IF W-IDLEVNR-ALPHA = 'DL6QA'                                         
457100        MOVE '6714 ' TO W-IDLEVNR-ALPHA  END-IF                           
457200     IF W-IDLEVNR-ALPHA = 'DLLNA'                                         
457300        MOVE '6761 ' TO W-IDLEVNR-ALPHA  END-IF                           
457400     IF W-IDLEVNR-ALPHA = 'V0TAA'                                         
457500        MOVE '6783 ' TO W-IDLEVNR-ALPHA  END-IF                           
457600     IF W-IDLEVNR-ALPHA = 'C92KA'                                         
457700        MOVE '6812 ' TO W-IDLEVNR-ALPHA  END-IF                           
457800     IF W-IDLEVNR-ALPHA = 'D25WB'                                         
457900        MOVE '6825 ' TO W-IDLEVNR-ALPHA  END-IF                           
458000     IF W-IDLEVNR-ALPHA = 'P4WSA'                                         
458100        MOVE '6847 ' TO W-IDLEVNR-ALPHA  END-IF                           
458200     IF W-IDLEVNR-ALPHA = 'A708A'                                         
458300        MOVE '6869 ' TO W-IDLEVNR-ALPHA  END-IF                           
458400     IF W-IDLEVNR-ALPHA = 'S3C3A'                                         
458500        MOVE '6870 ' TO W-IDLEVNR-ALPHA  END-IF                           
458600     IF W-IDLEVNR-ALPHA = 'DL6TA'                                         
458700        MOVE '6885 ' TO W-IDLEVNR-ALPHA  END-IF                           
458800     IF W-IDLEVNR-ALPHA = 'DL6VA'                                         
458900        MOVE '6892 ' TO W-IDLEVNR-ALPHA  END-IF                           
459000     IF W-IDLEVNR-ALPHA = 'CN5FA'                                         
459100        MOVE '6899 ' TO W-IDLEVNR-ALPHA  END-IF                           
459200     IF W-IDLEVNR-ALPHA = 'N82PA'                                         
459300        MOVE '7038 ' TO W-IDLEVNR-ALPHA  END-IF                           
459400     IF W-IDLEVNR-ALPHA = 'DLLPA'                                         
459500        MOVE '7106 ' TO W-IDLEVNR-ALPHA  END-IF                           
459600     IF W-IDLEVNR-ALPHA = 'HCR1A'                                         
459700        MOVE '7115 ' TO W-IDLEVNR-ALPHA  END-IF                           
459800     IF W-IDLEVNR-ALPHA = 'DLLQA'                                         
459900        MOVE '7132 ' TO W-IDLEVNR-ALPHA  END-IF                           
460000     IF W-IDLEVNR-ALPHA = 'MTJFA'                                         
460100        MOVE '7134 ' TO W-IDLEVNR-ALPHA  END-IF                           
460200     IF W-IDLEVNR-ALPHA = 'MWAJB'                                         
460300        MOVE '7229 ' TO W-IDLEVNR-ALPHA  END-IF                           
460400     IF W-IDLEVNR-ALPHA = 'DL6WA'                                         
460500        MOVE '7235 ' TO W-IDLEVNR-ALPHA  END-IF                           
460600     IF W-IDLEVNR-ALPHA = 'DLLRA'                                         
460700        MOVE '7357 ' TO W-IDLEVNR-ALPHA  END-IF                           
460800     IF W-IDLEVNR-ALPHA = 'DL6YA'                                         
460900        MOVE '7831 ' TO W-IDLEVNR-ALPHA  END-IF                           
461000     IF W-IDLEVNR-ALPHA = 'MHQSA'                                         
461100        MOVE '7955 ' TO W-IDLEVNR-ALPHA  END-IF                           
461200     IF W-IDLEVNR-ALPHA = 'BLLMA'                                         
461300        MOVE '8004 ' TO W-IDLEVNR-ALPHA  END-IF                           
461400     IF W-IDLEVNR-ALPHA = 'BJW6A'                                         
461500        MOVE '8010 ' TO W-IDLEVNR-ALPHA  END-IF                           
461600     IF W-IDLEVNR-ALPHA = 'BKXUA'                                         
461700        MOVE '8023 ' TO W-IDLEVNR-ALPHA  END-IF                           
461800     IF W-IDLEVNR-ALPHA = 'BLMJA'                                         
461900        MOVE '8061 ' TO W-IDLEVNR-ALPHA  END-IF                           
462000     IF W-IDLEVNR-ALPHA = 'DLLTA'                                         
462100        MOVE '8086 ' TO W-IDLEVNR-ALPHA  END-IF                           
462200     IF W-IDLEVNR-ALPHA = 'BKYNA'                                         
462300        MOVE '8094 ' TO W-IDLEVNR-ALPHA  END-IF                           
462400     IF W-IDLEVNR-ALPHA = 'BK5LA'                                         
462500        MOVE '8103 ' TO W-IDLEVNR-ALPHA  END-IF                           
462600     IF W-IDLEVNR-ALPHA = 'DLLUA'                                         
462700        MOVE '8107 ' TO W-IDLEVNR-ALPHA  END-IF                           
462800     IF W-IDLEVNR-ALPHA = 'DL6ZA'                                         
462900        MOVE '8123 ' TO W-IDLEVNR-ALPHA  END-IF                           
463000     IF W-IDLEVNR-ALPHA = 'D13DA'                                         
463100        MOVE '8138 ' TO W-IDLEVNR-ALPHA  END-IF                           
463200     IF W-IDLEVNR-ALPHA = 'BSN7A'                                         
463300        MOVE '8150 ' TO W-IDLEVNR-ALPHA  END-IF                           
463400     IF W-IDLEVNR-ALPHA = 'DLLVA'                                         
463500        MOVE '8151 ' TO W-IDLEVNR-ALPHA  END-IF                           
463600     IF W-IDLEVNR-ALPHA = 'S6LPA'                                         
463700        MOVE '8168 ' TO W-IDLEVNR-ALPHA  END-IF                           
463800     IF W-IDLEVNR-ALPHA = 'CZTVA'                                         
463900        MOVE '8181 ' TO W-IDLEVNR-ALPHA  END-IF                           
464000     IF W-IDLEVNR-ALPHA = 'BSFPA'                                         
464100        MOVE '8190 ' TO W-IDLEVNR-ALPHA  END-IF                           
464200     IF W-IDLEVNR-ALPHA = 'DLLWA'                                         
464300        MOVE '8192 ' TO W-IDLEVNR-ALPHA  END-IF                           
464400     IF W-IDLEVNR-ALPHA = 'BSKXA'                                         
464500        MOVE '8199 ' TO W-IDLEVNR-ALPHA  END-IF                           
464600     IF W-IDLEVNR-ALPHA = 'DL7AA'                                         
464700        MOVE '8203 ' TO W-IDLEVNR-ALPHA  END-IF                           
464800     IF W-IDLEVNR-ALPHA = 'DLLXA'                                         
464900        MOVE '8212 ' TO W-IDLEVNR-ALPHA  END-IF                           
465000     IF W-IDLEVNR-ALPHA = 'S98JA'                                         
465100        MOVE '8216 ' TO W-IDLEVNR-ALPHA  END-IF                           
465200     IF W-IDLEVNR-ALPHA = 'DLLYA'                                         
465300        MOVE '8233 ' TO W-IDLEVNR-ALPHA  END-IF                           
465400     IF W-IDLEVNR-ALPHA = 'DLLZA'                                         
465500        MOVE '8234 ' TO W-IDLEVNR-ALPHA  END-IF                           
465600     IF W-IDLEVNR-ALPHA = 'BAG3A'                                         
465700        MOVE '8860 ' TO W-IDLEVNR-ALPHA  END-IF                           
465800     IF W-IDLEVNR-ALPHA = 'S7YJA'                                         
465900        MOVE '10139' TO W-IDLEVNR-ALPHA  END-IF                           
466000     IF W-IDLEVNR-ALPHA = 'DL7CA'                                         
466100        MOVE '10355' TO W-IDLEVNR-ALPHA  END-IF                           
466200     IF W-IDLEVNR-ALPHA = 'DLL0A'                                         
466300        MOVE '10803' TO W-IDLEVNR-ALPHA  END-IF                           
466400     IF W-IDLEVNR-ALPHA = 'CRJ8A'                                         
466500        MOVE '10910' TO W-IDLEVNR-ALPHA  END-IF                           
466600     IF W-IDLEVNR-ALPHA = 'Q5FPA'                                         
466700        MOVE '11117' TO W-IDLEVNR-ALPHA  END-IF                           
466800     IF W-IDLEVNR-ALPHA = 'D1K4E'                                         
466900        MOVE '12089' TO W-IDLEVNR-ALPHA  END-IF                           
467000     IF W-IDLEVNR-ALPHA = 'BA7ZA'                                         
467100        MOVE '12098' TO W-IDLEVNR-ALPHA  END-IF                           
467200     IF W-IDLEVNR-ALPHA = 'DLL5A'                                         
467300        MOVE '13311' TO W-IDLEVNR-ALPHA  END-IF                           
467400     IF W-IDLEVNR-ALPHA = 'DL7GA'                                         
467500        MOVE '13344' TO W-IDLEVNR-ALPHA  END-IF                           
467600     IF W-IDLEVNR-ALPHA = 'DL7HA'                                         
467700        MOVE '13346' TO W-IDLEVNR-ALPHA  END-IF                           
467800     IF W-IDLEVNR-ALPHA = 'DL7JA'                                         
467900        MOVE '13348' TO W-IDLEVNR-ALPHA  END-IF                           
468000     IF W-IDLEVNR-ALPHA = 'C62JC'                                         
468100        MOVE '13362' TO W-IDLEVNR-ALPHA  END-IF                           
468200     IF W-IDLEVNR-ALPHA = 'DLMEA'                                         
468300        MOVE '13374' TO W-IDLEVNR-ALPHA  END-IF                           
468400     IF W-IDLEVNR-ALPHA = 'DLMFA'                                         
468500        MOVE '13375' TO W-IDLEVNR-ALPHA  END-IF                           
468600     IF W-IDLEVNR-ALPHA = 'DLMGA'                                         
468700        MOVE '13376' TO W-IDLEVNR-ALPHA  END-IF                           
468800*************************************************                         
468900     IF W-IDLEVNR-ALPHA = 'DLMHA'                                         
469000        MOVE '13379' TO W-IDLEVNR-ALPHA  END-IF                           
469100     IF W-IDLEVNR-ALPHA = 'D3W5A'                                         
469200        MOVE '13381' TO W-IDLEVNR-ALPHA  END-IF                           
469300     IF W-IDLEVNR-ALPHA = 'CT3NA'                                         
469400        MOVE '13383' TO W-IDLEVNR-ALPHA  END-IF                           
469500     IF W-IDLEVNR-ALPHA = 'DLMJA'                                         
469600        MOVE '13384' TO W-IDLEVNR-ALPHA  END-IF                           
469700     IF W-IDLEVNR-ALPHA = 'CGECA'                                         
469800        MOVE '13385' TO W-IDLEVNR-ALPHA  END-IF                           
469900     IF W-IDLEVNR-ALPHA = 'DLMLA'                                         
470000        MOVE '13390' TO W-IDLEVNR-ALPHA  END-IF                           
470100     IF W-IDLEVNR-ALPHA = 'DLMMA'                                         
470200        MOVE '13392' TO W-IDLEVNR-ALPHA  END-IF                           
470300     IF W-IDLEVNR-ALPHA = 'DLMPA'                                         
470400        MOVE '13401' TO W-IDLEVNR-ALPHA  END-IF                           
470500     IF W-IDLEVNR-ALPHA = 'DL7MA'                                         
470600        MOVE '13402' TO W-IDLEVNR-ALPHA  END-IF                           
470700     IF W-IDLEVNR-ALPHA = 'CFJDA'                                         
470800        MOVE '13403' TO W-IDLEVNR-ALPHA  END-IF                           
470900     IF W-IDLEVNR-ALPHA = 'DLMSA'                                         
471000        MOVE '13408' TO W-IDLEVNR-ALPHA  END-IF                           
471100     IF W-IDLEVNR-ALPHA = 'DLMTA'                                         
471200        MOVE '13409' TO W-IDLEVNR-ALPHA  END-IF                           
471300     IF W-IDLEVNR-ALPHA = 'DL7NA'                                         
471400        MOVE '13411' TO W-IDLEVNR-ALPHA  END-IF                           
471500     IF W-IDLEVNR-ALPHA = 'DLMUA'                                         
471600        MOVE '13416' TO W-IDLEVNR-ALPHA  END-IF                           
471700     IF W-IDLEVNR-ALPHA = 'DLMVA'                                         
471800        MOVE '13463' TO W-IDLEVNR-ALPHA  END-IF                           
471900     IF W-IDLEVNR-ALPHA = 'DLMWA'                                         
472000        MOVE '13465' TO W-IDLEVNR-ALPHA  END-IF                           
472100     IF W-IDLEVNR-ALPHA = 'BQMJA'                                         
472200        MOVE '13467' TO W-IDLEVNR-ALPHA  END-IF                           
472300     IF W-IDLEVNR-ALPHA = 'DL7RA'                                         
472400        MOVE '13475' TO W-IDLEVNR-ALPHA  END-IF                           
472500     IF W-IDLEVNR-ALPHA = 'AMAAC'                                         
472600        MOVE '13529' TO W-IDLEVNR-ALPHA  END-IF                           
472700     IF W-IDLEVNR-ALPHA = 'LRZ7A'                                         
472800        MOVE '13561' TO W-IDLEVNR-ALPHA  END-IF                           
472900     IF W-IDLEVNR-ALPHA = 'DL7VA'                                         
473000        MOVE '13565' TO W-IDLEVNR-ALPHA  END-IF                           
473100     IF W-IDLEVNR-ALPHA = 'BVNUC'                                         
473200        MOVE '13576' TO W-IDLEVNR-ALPHA  END-IF                           
473300     IF W-IDLEVNR-ALPHA = 'DLMYA'                                         
473400        MOVE '13621' TO W-IDLEVNR-ALPHA  END-IF                           
473500     IF W-IDLEVNR-ALPHA = 'DL7XA'                                         
473600        MOVE '13776' TO W-IDLEVNR-ALPHA  END-IF                           
473700     IF W-IDLEVNR-ALPHA = 'Q99AA'                                         
473800        MOVE '14483' TO W-IDLEVNR-ALPHA  END-IF                           
473900     IF W-IDLEVNR-ALPHA = 'CFH6A'                                         
474000        MOVE '14578' TO W-IDLEVNR-ALPHA  END-IF                           
474100     IF W-IDLEVNR-ALPHA = 'DLMZA'                                         
474200        MOVE '14592' TO W-IDLEVNR-ALPHA  END-IF                           
474300     IF W-IDLEVNR-ALPHA = 'DLM1A'                                         
474400        MOVE '14921' TO W-IDLEVNR-ALPHA  END-IF                           
474500     IF W-IDLEVNR-ALPHA = 'DLM3A'                                         
474600        MOVE '14935' TO W-IDLEVNR-ALPHA  END-IF                           
474700     IF W-IDLEVNR-ALPHA = 'DLNBA'                                         
474800        MOVE '14935' TO W-IDLEVNR-ALPHA  END-IF                           
474900     IF W-IDLEVNR-ALPHA = 'MHLTA'                                         
475000        MOVE '15065' TO W-IDLEVNR-ALPHA  END-IF                           
475100     IF W-IDLEVNR-ALPHA = 'DLNCA'                                         
475200        MOVE '15180' TO W-IDLEVNR-ALPHA  END-IF                           
475300     IF W-IDLEVNR-ALPHA = 'DLNDA'                                         
475400        MOVE '15207' TO W-IDLEVNR-ALPHA  END-IF                           
475500     IF W-IDLEVNR-ALPHA = 'N6WEA'                                         
475600        MOVE '15227' TO W-IDLEVNR-ALPHA  END-IF                           
475700     IF W-IDLEVNR-ALPHA = 'N7381'                                         
475800        MOVE '15256' TO W-IDLEVNR-ALPHA  END-IF                           
475900     IF W-IDLEVNR-ALPHA = 'DLNEA'                                         
476000        MOVE '15260' TO W-IDLEVNR-ALPHA  END-IF                           
476100     IF W-IDLEVNR-ALPHA = 'DLNFA'                                         
476200        MOVE '15266' TO W-IDLEVNR-ALPHA  END-IF                           
476300     IF W-IDLEVNR-ALPHA = 'DLNGA'                                         
476400        MOVE '15310' TO W-IDLEVNR-ALPHA  END-IF                           
476500     IF W-IDLEVNR-ALPHA = 'DLNHA'                                         
476600        MOVE '15322' TO W-IDLEVNR-ALPHA  END-IF                           
476700     IF W-IDLEVNR-ALPHA = 'DL8BA'                                         
476800        MOVE '15325' TO W-IDLEVNR-ALPHA  END-IF                           
476900     IF W-IDLEVNR-ALPHA = 'DL8CA'                                         
477000        MOVE '16087' TO W-IDLEVNR-ALPHA  END-IF                           
477100     IF W-IDLEVNR-ALPHA = 'S3MZA'                                         
477200        MOVE '16090' TO W-IDLEVNR-ALPHA  END-IF                           
477300     IF W-IDLEVNR-ALPHA = 'CN5PA'                                         
477400        MOVE '16116' TO W-IDLEVNR-ALPHA  END-IF                           
477500     IF W-IDLEVNR-ALPHA = 'T93AB'                                         
477600        MOVE '16123' TO W-IDLEVNR-ALPHA  END-IF                           
477700     IF W-IDLEVNR-ALPHA = 'K760B'                                         
477800        MOVE '16367' TO W-IDLEVNR-ALPHA  END-IF                           
477900     IF W-IDLEVNR-ALPHA = 'S5S2A'                                         
478000        MOVE '16466' TO W-IDLEVNR-ALPHA  END-IF                           
478100     IF W-IDLEVNR-ALPHA = 'D3R3A'                                         
478200        MOVE '17782' TO W-IDLEVNR-ALPHA  END-IF                           
478300     IF W-IDLEVNR-ALPHA = 'C9G4A'                                         
478400        MOVE '17784' TO W-IDLEVNR-ALPHA  END-IF                           
478500     IF W-IDLEVNR-ALPHA = 'DLNLA'                                         
478600        MOVE '17785' TO W-IDLEVNR-ALPHA  END-IF                           
478700     IF W-IDLEVNR-ALPHA = 'DLNMA'                                         
478800        MOVE '17792' TO W-IDLEVNR-ALPHA  END-IF                           
478900     IF W-IDLEVNR-ALPHA = 'DLNNA'                                         
479000        MOVE '17813' TO W-IDLEVNR-ALPHA  END-IF                           
479100     IF W-IDLEVNR-ALPHA = 'BKPQA'                                         
479200        MOVE '18000' TO W-IDLEVNR-ALPHA  END-IF                           
479300     IF W-IDLEVNR-ALPHA = 'BKHYA'                                         
479400        MOVE '18012' TO W-IDLEVNR-ALPHA  END-IF                           
479500     IF W-IDLEVNR-ALPHA = 'BSK0A'                                         
479600        MOVE '18060' TO W-IDLEVNR-ALPHA  END-IF                           
479700     IF W-IDLEVNR-ALPHA = 'DLNQA'                                         
479800        MOVE '18067' TO W-IDLEVNR-ALPHA  END-IF                           
479900     IF W-IDLEVNR-ALPHA = 'DLNRA'                                         
480000        MOVE '18075' TO W-IDLEVNR-ALPHA  END-IF                           
480100     IF W-IDLEVNR-ALPHA = 'CXC8A'                                         
480200        MOVE '18120' TO W-IDLEVNR-ALPHA  END-IF                           
480300     IF W-IDLEVNR-ALPHA = 'DLNSA'                                         
480400        MOVE '18977' TO W-IDLEVNR-ALPHA  END-IF                           
480500     IF W-IDLEVNR-ALPHA = 'DLNTA'                                         
480600        MOVE '19052' TO W-IDLEVNR-ALPHA  END-IF                           
480700     IF W-IDLEVNR-ALPHA = 'BK4FA'                                         
480800        MOVE '19235' TO W-IDLEVNR-ALPHA  END-IF                           
481100     IF W-IDLEVNR-ALPHA = 'AYGHA'                                         
481200        MOVE '20895' TO W-IDLEVNR-ALPHA  END-IF                           
481300     IF W-IDLEVNR-ALPHA = 'CECDA'                                         
481400        MOVE '21580' TO W-IDLEVNR-ALPHA  END-IF                           
481500     IF W-IDLEVNR-ALPHA = 'CGSCA'                                         
481600        MOVE '23939' TO W-IDLEVNR-ALPHA  END-IF                           
481700     IF W-IDLEVNR-ALPHA = 'DL8EA'                                         
481800        MOVE '23941' TO W-IDLEVNR-ALPHA  END-IF                           
481900     IF W-IDLEVNR-ALPHA = 'ATPUB'                                         
482000        MOVE '24633' TO W-IDLEVNR-ALPHA  END-IF                           
482100     IF W-IDLEVNR-ALPHA = 'DL8FA'                                         
482200        MOVE '24837' TO W-IDLEVNR-ALPHA  END-IF                           
482300     IF W-IDLEVNR-ALPHA = 'DLNWA'                                         
482400        MOVE '25012' TO W-IDLEVNR-ALPHA  END-IF                           
482500     IF W-IDLEVNR-ALPHA = 'BLWQA'                                         
482600        MOVE '25745' TO W-IDLEVNR-ALPHA  END-IF                           
482700     IF W-IDLEVNR-ALPHA = 'DN6EA'                                         
482800        MOVE '25955' TO W-IDLEVNR-ALPHA  END-IF                           
482900     IF W-IDLEVNR-ALPHA = 'A224A'                                         
483000        MOVE '50049' TO W-IDLEVNR-ALPHA  END-IF                           
483100     IF W-IDLEVNR-ALPHA = 'G8VUE'                                         
483200        MOVE '55005' TO W-IDLEVNR-ALPHA  END-IF                           
483300     IF W-IDLEVNR-ALPHA = 'CZ6AB'                                         
483400        MOVE '80096' TO W-IDLEVNR-ALPHA  END-IF                           
483500*************************************************                         
483600     IF W-IDLEVNR-ALPHA = 'BQYJA'                                         
483700        MOVE '87   ' TO W-IDLEVNR-ALPHA  END-IF                           
483800     IF W-IDLEVNR-ALPHA = 'N81ZA'                                         
483900        MOVE '1186 ' TO W-IDLEVNR-ALPHA  END-IF                           
484000     IF W-IDLEVNR-ALPHA = 'S6NKA'                                         
484100        MOVE '1447 ' TO W-IDLEVNR-ALPHA  END-IF                           
484200     IF W-IDLEVNR-ALPHA = 'BVNQA'                                         
484300        MOVE '2318 ' TO W-IDLEVNR-ALPHA  END-IF                           
484400     IF W-IDLEVNR-ALPHA = 'BQEKA'                                         
484500        MOVE '2356 ' TO W-IDLEVNR-ALPHA  END-IF                           
484600     IF W-IDLEVNR-ALPHA = 'BWKTA'                                         
484700        MOVE '3169 ' TO W-IDLEVNR-ALPHA  END-IF                           
484800     IF W-IDLEVNR-ALPHA = 'T2FBB'                                         
484900        MOVE '3803 ' TO W-IDLEVNR-ALPHA  END-IF                           
485000     IF W-IDLEVNR-ALPHA = 'D0DAA'                                         
485100        MOVE '3894 ' TO W-IDLEVNR-ALPHA  END-IF                           
485200     IF W-IDLEVNR-ALPHA = 'S17VA'                                         
485300        MOVE '3989 ' TO W-IDLEVNR-ALPHA  END-IF                           
485400     IF W-IDLEVNR-ALPHA = 'J3ZLA'                                         
485500        MOVE '4618 ' TO W-IDLEVNR-ALPHA  END-IF                           
485600     IF W-IDLEVNR-ALPHA = 'R8LDA'                                         
485700        MOVE '5212 ' TO W-IDLEVNR-ALPHA  END-IF                           
485800     IF W-IDLEVNR-ALPHA = 'V0QEA'                                         
485900        MOVE '5684 ' TO W-IDLEVNR-ALPHA  END-IF                           
486000     IF W-IDLEVNR-ALPHA = 'G769B'                                         
486100        MOVE '6063 ' TO W-IDLEVNR-ALPHA  END-IF                           
486200     IF W-IDLEVNR-ALPHA = 'C99ZA'                                         
486300        MOVE '6119 ' TO W-IDLEVNR-ALPHA  END-IF                           
486400     IF W-IDLEVNR-ALPHA = 'D1E4A'                                         
486500        MOVE '6166 ' TO W-IDLEVNR-ALPHA  END-IF                           
486600     IF W-IDLEVNR-ALPHA = 'D2H8A'                                         
486700        MOVE '6437 ' TO W-IDLEVNR-ALPHA  END-IF                           
486800     IF W-IDLEVNR-ALPHA = 'G952A'                                         
486900        MOVE '6515 ' TO W-IDLEVNR-ALPHA  END-IF                           
487000     IF W-IDLEVNR-ALPHA = 'Q520A'                                         
487100        MOVE '6522 ' TO W-IDLEVNR-ALPHA  END-IF                           
487200     IF W-IDLEVNR-ALPHA = 'EFR5A'                                         
487300        MOVE '6719 ' TO W-IDLEVNR-ALPHA  END-IF                           
487400     IF W-IDLEVNR-ALPHA = 'L905D'                                         
487500        MOVE '6771 ' TO W-IDLEVNR-ALPHA  END-IF                           
487600     IF W-IDLEVNR-ALPHA = 'BQ7RA'                                         
487700        MOVE '6828 ' TO W-IDLEVNR-ALPHA  END-IF                           
487800     IF W-IDLEVNR-ALPHA = 'U5P7A'                                         
487900        MOVE '6836 ' TO W-IDLEVNR-ALPHA  END-IF                           
488000     IF W-IDLEVNR-ALPHA = 'U1LFD'                                         
488100        MOVE '6857 ' TO W-IDLEVNR-ALPHA  END-IF                           
488200     IF W-IDLEVNR-ALPHA = 'ABYHA'                                         
488300        MOVE '6950 ' TO W-IDLEVNR-ALPHA  END-IF                           
488400     IF W-IDLEVNR-ALPHA = 'BPXDA'                                         
488500        MOVE '7040 ' TO W-IDLEVNR-ALPHA  END-IF                           
488600     IF W-IDLEVNR-ALPHA = 'T4UVA'                                         
488700        MOVE '7258 ' TO W-IDLEVNR-ALPHA  END-IF                           
488800     IF W-IDLEVNR-ALPHA = 'J5VHA'                                         
488900        MOVE '7919 ' TO W-IDLEVNR-ALPHA  END-IF                           
489000     IF W-IDLEVNR-ALPHA = 'N5LQA'                                         
489100        MOVE '7927 ' TO W-IDLEVNR-ALPHA  END-IF                           
489200     IF W-IDLEVNR-ALPHA = 'BQJDD'                                         
489300        MOVE '7954 ' TO W-IDLEVNR-ALPHA  END-IF                           
489400     IF W-IDLEVNR-ALPHA = 'BKAMB'                                         
489500        MOVE '10056' TO W-IDLEVNR-ALPHA  END-IF                           
489600     IF W-IDLEVNR-ALPHA = 'BPEVA'                                         
489700        MOVE '11369' TO W-IDLEVNR-ALPHA  END-IF                           
489800     IF W-IDLEVNR-ALPHA = 'T3DVA'                                         
489900        MOVE '13619' TO W-IDLEVNR-ALPHA  END-IF                           
490000     IF W-IDLEVNR-ALPHA = 'D797B'                                         
490100        MOVE '16096' TO W-IDLEVNR-ALPHA  END-IF                           
490200     IF W-IDLEVNR-ALPHA = 'ALTXA'                                         
490300        MOVE '16238' TO W-IDLEVNR-ALPHA  END-IF                           
490400     IF W-IDLEVNR-ALPHA = 'B45JC'                                         
490500        MOVE '16275' TO W-IDLEVNR-ALPHA  END-IF                           
490600     IF W-IDLEVNR-ALPHA = 'DP5UA'                                         
490700        MOVE '16301' TO W-IDLEVNR-ALPHA  END-IF                           
490800     IF W-IDLEVNR-ALPHA = 'BH0ZA'                                         
490900        MOVE '17253' TO W-IDLEVNR-ALPHA  END-IF                           
491000     IF W-IDLEVNR-ALPHA = 'H82NE'                                         
491100        MOVE '17825' TO W-IDLEVNR-ALPHA  END-IF                           
491200     IF W-IDLEVNR-ALPHA = 'H137P'                                         
491300        MOVE '17826' TO W-IDLEVNR-ALPHA  END-IF                           
491400     IF W-IDLEVNR-ALPHA = 'N508A'                                         
491500        MOVE '18127' TO W-IDLEVNR-ALPHA  END-IF                           
491600     IF W-IDLEVNR-ALPHA = 'AMMCB'                                         
491700        MOVE '19959' TO W-IDLEVNR-ALPHA  END-IF                           
491800     IF W-IDLEVNR-ALPHA = 'CF9JA'                                         
491900        MOVE '23271' TO W-IDLEVNR-ALPHA  END-IF                           
492000     IF W-IDLEVNR-ALPHA = 'CZQ7A'                                         
492100        MOVE '23914' TO W-IDLEVNR-ALPHA  END-IF                           
492200     IF W-IDLEVNR-ALPHA = 'H5PLA'                                         
492300        MOVE '23947' TO W-IDLEVNR-ALPHA  END-IF                           
492400     IF W-IDLEVNR-ALPHA = 'CU8WB'                                         
492500        MOVE '24374' TO W-IDLEVNR-ALPHA  END-IF                           
492600     IF W-IDLEVNR-ALPHA = 'AYSPA'                                         
492700        MOVE '24630' TO W-IDLEVNR-ALPHA  END-IF                           
492800     IF W-IDLEVNR-ALPHA = 'BHD8A'                                         
492900        MOVE '25763' TO W-IDLEVNR-ALPHA  END-IF                           
493000     IF W-IDLEVNR-ALPHA = 'DLKCA'                                         
493100        MOVE '1525 ' TO W-IDLEVNR-ALPHA  END-IF                           
493200     IF W-IDLEVNR-ALPHA = 'A76VA'                                         
493300        MOVE '1592 ' TO W-IDLEVNR-ALPHA  END-IF                           
493400     IF W-IDLEVNR-ALPHA = 'D33HE'                                         
493500        MOVE '3963 ' TO W-IDLEVNR-ALPHA  END-IF                           
493600     IF W-IDLEVNR-ALPHA = 'C69HA'                                         
493700        MOVE '6386 ' TO W-IDLEVNR-ALPHA  END-IF                           
493800     IF W-IDLEVNR-ALPHA = 'D0MNA'                                         
493900        MOVE '6808 ' TO W-IDLEVNR-ALPHA  END-IF                           
494000     IF W-IDLEVNR-ALPHA = 'C97RA'                                         
494100        MOVE '6914 ' TO W-IDLEVNR-ALPHA  END-IF                           
494200     IF W-IDLEVNR-ALPHA = 'BK2EA'                                         
494300        MOVE '8186 ' TO W-IDLEVNR-ALPHA  END-IF                           
494400     IF W-IDLEVNR-ALPHA = 'BQ8SA'                                         
494500        MOVE '10221' TO W-IDLEVNR-ALPHA  END-IF                           
494600     IF W-IDLEVNR-ALPHA = 'BUWJA'                                         
494700        MOVE '10511' TO W-IDLEVNR-ALPHA  END-IF                           
494800     IF W-IDLEVNR-ALPHA = 'BEJZA'                                         
494900        MOVE '10659' TO W-IDLEVNR-ALPHA  END-IF                           
495000     IF W-IDLEVNR-ALPHA = 'BNSRA'                                         
495100        MOVE '10813' TO W-IDLEVNR-ALPHA  END-IF                           
495200     IF W-IDLEVNR-ALPHA = 'D0BEB'                                         
495300        MOVE '11332' TO W-IDLEVNR-ALPHA  END-IF                           
495400     IF W-IDLEVNR-ALPHA = 'DL7SA'                                         
495500        MOVE '13484' TO W-IDLEVNR-ALPHA  END-IF                           
495600     IF W-IDLEVNR-ALPHA = 'D0MNE'                                         
495700        MOVE '13508' TO W-IDLEVNR-ALPHA  END-IF                           
495800     IF W-IDLEVNR-ALPHA = 'N0KFA'                                         
495900        MOVE '14465' TO W-IDLEVNR-ALPHA  END-IF                           
496000     IF W-IDLEVNR-ALPHA = 'MWZFA'                                         
496100        MOVE '14658' TO W-IDLEVNR-ALPHA  END-IF                           
496200     IF W-IDLEVNR-ALPHA = 'AYG1A'                                         
496300        MOVE '14756' TO W-IDLEVNR-ALPHA  END-IF                           
496400     IF W-IDLEVNR-ALPHA = 'BQ9RA'                                         
496500        MOVE '15053' TO W-IDLEVNR-ALPHA  END-IF                           
496600     IF W-IDLEVNR-ALPHA = 'D0MND'                                         
496700        MOVE '16049' TO W-IDLEVNR-ALPHA  END-IF                           
496800     IF W-IDLEVNR-ALPHA = 'D0MNF'                                         
496900        MOVE '16075' TO W-IDLEVNR-ALPHA  END-IF                           
497000     IF W-IDLEVNR-ALPHA = 'D33HA'                                         
497100        MOVE '17789' TO W-IDLEVNR-ALPHA  END-IF                           
497200     IF W-IDLEVNR-ALPHA = 'D0MNG'                                         
497300        MOVE '17945' TO W-IDLEVNR-ALPHA  END-IF                           
497400     IF W-IDLEVNR-ALPHA = 'S4HWB'                                         
497500        MOVE '19739' TO W-IDLEVNR-ALPHA  END-IF                           
497600     IF W-IDLEVNR-ALPHA = 'AJ2AA'                                         
497700        MOVE '21004' TO W-IDLEVNR-ALPHA  END-IF                           
497800     IF W-IDLEVNR-ALPHA = 'C8Q8A'                                         
497900        MOVE '22389' TO W-IDLEVNR-ALPHA  END-IF                           
498000     IF W-IDLEVNR-ALPHA = 'C94MA'                                         
498100        MOVE '23335' TO W-IDLEVNR-ALPHA  END-IF                           
498200     IF W-IDLEVNR-ALPHA = 'F477B'                                         
498300        MOVE '23862' TO W-IDLEVNR-ALPHA  END-IF                           
498400     IF W-IDLEVNR-ALPHA = 'P8TWA'                                         
498500        MOVE '25916' TO W-IDLEVNR-ALPHA  END-IF                           
498600     IF W-IDLEVNR-ALPHA = 'DNK9A'                                         
498700        MOVE '25931' TO W-IDLEVNR-ALPHA  END-IF                           
498800*************************************************                         
498900*************************************************                         
499000     IF W-IDLEVNR-ALPHA = 'BUPDC'                                         
499100        MOVE '31138' TO W-IDLEVNR-ALPHA  END-IF                           
499200     IF W-IDLEVNR-ALPHA = 'BUPDD'                                         
499300        MOVE '41138' TO W-IDLEVNR-ALPHA  END-IF                           
499400     IF W-IDLEVNR-ALPHA = 'L8K5H'                                         
499500        MOVE '11138' TO W-IDLEVNR-ALPHA  END-IF                           
499600*************************************************                         
499700     IF W-IDLEVNR-ALPHA = 'Q99AA'                                         
499800        MOVE '14483' TO W-IDLEVNR-ALPHA  END-IF                           
499900     IF W-IDLEVNR-ALPHA = 'D38KF'                                         
500000        MOVE '16044' TO W-IDLEVNR-ALPHA  END-IF                           
500100     IF W-IDLEVNR-ALPHA = 'S3MZA'                                         
500200        MOVE '16090' TO W-IDLEVNR-ALPHA  END-IF                           
500300     IF W-IDLEVNR-ALPHA = 'C95YC'                                         
500400        MOVE '16284' TO W-IDLEVNR-ALPHA  END-IF                           
500500     IF W-IDLEVNR-ALPHA = 'C95YB'                                         
500600        MOVE '16378' TO W-IDLEVNR-ALPHA  END-IF                           
500700     IF W-IDLEVNR-ALPHA = 'C95YJ'                                         
500800        MOVE '20501' TO W-IDLEVNR-ALPHA  END-IF                           
500900     IF W-IDLEVNR-ALPHA = 'D02KA'                                         
501000        MOVE '26942' TO W-IDLEVNR-ALPHA  END-IF                           
501100     IF W-IDLEVNR-ALPHA = 'D0UCA'                                         
501200        MOVE '3742 ' TO W-IDLEVNR-ALPHA  END-IF                           
501300     IF W-IDLEVNR-ALPHA = 'B4V0A'                                         
501400        MOVE '6685 ' TO W-IDLEVNR-ALPHA  END-IF                           
501500     IF W-IDLEVNR-ALPHA = 'C95YA'                                         
501600        MOVE '6942 ' TO W-IDLEVNR-ALPHA  END-IF                           
501700     IF W-IDLEVNR-ALPHA = 'S63MA'                                         
501800        MOVE '7683 ' TO W-IDLEVNR-ALPHA  END-IF                           
501900     IF W-IDLEVNR-ALPHA = 'BPFNB'                                         
502000        MOVE '34666' TO W-IDLEVNR-ALPHA  END-IF                           
502100     IF W-IDLEVNR-ALPHA = 'CDCHA'                                         
502200        MOVE '10030' TO W-IDLEVNR-ALPHA  END-IF                           
502300     IF W-IDLEVNR-ALPHA = 'L8K5W'                                         
502400        MOVE '10181' TO W-IDLEVNR-ALPHA  END-IF                           
502500     IF W-IDLEVNR-ALPHA = 'CDV5A'                                         
502600        MOVE '10347' TO W-IDLEVNR-ALPHA  END-IF                           
502700     IF W-IDLEVNR-ALPHA = 'S4MZD'                                         
502800        MOVE '11518' TO W-IDLEVNR-ALPHA  END-IF                           
502900     IF W-IDLEVNR-ALPHA = 'S1XMC'                                         
503000        MOVE '11753' TO W-IDLEVNR-ALPHA  END-IF                           
503100     IF W-IDLEVNR-ALPHA = 'ALRHA'                                         
503200        MOVE '12539' TO W-IDLEVNR-ALPHA  END-IF                           
503300     IF W-IDLEVNR-ALPHA = 'BQ9FA'                                         
503400        MOVE '13444' TO W-IDLEVNR-ALPHA  END-IF                           
503500     IF W-IDLEVNR-ALPHA = 'S1XMD'                                         
503600        MOVE '13543' TO W-IDLEVNR-ALPHA  END-IF                           
503700     IF W-IDLEVNR-ALPHA = 'BUPDA'                                         
503800        MOVE '13635' TO W-IDLEVNR-ALPHA  END-IF                           
503900     IF W-IDLEVNR-ALPHA = 'BQ9PA'                                         
504000        MOVE '14599' TO W-IDLEVNR-ALPHA  END-IF                           
504100     IF W-IDLEVNR-ALPHA = 'P9J4A'                                         
504200        MOVE '14924' TO W-IDLEVNR-ALPHA  END-IF                           
504300     IF W-IDLEVNR-ALPHA = 'D11JA'                                         
504400        MOVE '15442' TO W-IDLEVNR-ALPHA  END-IF                           
504500     IF W-IDLEVNR-ALPHA = 'Q42PA'                                         
504600        MOVE '16062' TO W-IDLEVNR-ALPHA  END-IF                           
504700     IF W-IDLEVNR-ALPHA = 'B4X4A'                                         
504800        MOVE '16071' TO W-IDLEVNR-ALPHA  END-IF                           
504900     IF W-IDLEVNR-ALPHA = 'BPGQA'                                         
505000        MOVE '171  ' TO W-IDLEVNR-ALPHA  END-IF                           
505100     IF W-IDLEVNR-ALPHA = 'DJPSA'                                         
505200        MOVE '17255' TO W-IDLEVNR-ALPHA  END-IF                           
505300     IF W-IDLEVNR-ALPHA = 'AZYXA'                                         
505400        MOVE '18609' TO W-IDLEVNR-ALPHA  END-IF                           
505500     IF W-IDLEVNR-ALPHA = 'R0PRA'                                         
505600        MOVE '19733' TO W-IDLEVNR-ALPHA  END-IF                           
505700     IF W-IDLEVNR-ALPHA = 'BPGQD'                                         
505800        MOVE '2036 ' TO W-IDLEVNR-ALPHA  END-IF                           
505900     IF W-IDLEVNR-ALPHA = 'N2D2E'                                         
506000        MOVE '21756' TO W-IDLEVNR-ALPHA  END-IF                           
506100     IF W-IDLEVNR-ALPHA = 'CTYHA'                                         
506200        MOVE '22396' TO W-IDLEVNR-ALPHA  END-IF                           
506300     IF W-IDLEVNR-ALPHA = 'N81QA'                                         
506400        MOVE '230  ' TO W-IDLEVNR-ALPHA  END-IF                           
506500     IF W-IDLEVNR-ALPHA = 'BJHYA'                                         
506600        MOVE '23245' TO W-IDLEVNR-ALPHA  END-IF                           
506700     IF W-IDLEVNR-ALPHA = 'BQEKA'                                         
506800        MOVE '2356 ' TO W-IDLEVNR-ALPHA  END-IF                           
506900     IF W-IDLEVNR-ALPHA = 'BRTUA'                                         
507000        MOVE '23705' TO W-IDLEVNR-ALPHA  END-IF                           
507100     IF W-IDLEVNR-ALPHA = 'CLDQA'                                         
507200        MOVE '23799' TO W-IDLEVNR-ALPHA  END-IF                           
507300     IF W-IDLEVNR-ALPHA = 'BUPDA'                                         
507400        MOVE '24010' TO W-IDLEVNR-ALPHA  END-IF                           
507500     IF W-IDLEVNR-ALPHA = 'CLMPA'                                         
507600        MOVE '25669' TO W-IDLEVNR-ALPHA  END-IF                           
507700     IF W-IDLEVNR-ALPHA = 'BQ9PB'                                         
507800        MOVE '25691' TO W-IDLEVNR-ALPHA  END-IF                           
507900     IF W-IDLEVNR-ALPHA = 'DA5NA'                                         
508000        MOVE '25764' TO W-IDLEVNR-ALPHA  END-IF                           
508100     IF W-IDLEVNR-ALPHA = 'AQHKA'                                         
508200        MOVE '25874' TO W-IDLEVNR-ALPHA  END-IF                           
508300     IF W-IDLEVNR-ALPHA = 'L8K50'                                         
508400        MOVE '25890' TO W-IDLEVNR-ALPHA  END-IF                           
508500     IF W-IDLEVNR-ALPHA = 'L8K5Z'                                         
508600        MOVE '25891' TO W-IDLEVNR-ALPHA  END-IF                           
508700     IF W-IDLEVNR-ALPHA = 'L8K5Y'                                         
508800        MOVE '25892' TO W-IDLEVNR-ALPHA  END-IF                           
508900     IF W-IDLEVNR-ALPHA = 'L8K5X'                                         
509000        MOVE '25893' TO W-IDLEVNR-ALPHA  END-IF                           
509100     IF W-IDLEVNR-ALPHA = 'DNH1A'                                         
509200        MOVE '25930' TO W-IDLEVNR-ALPHA  END-IF                           
509300     IF W-IDLEVNR-ALPHA = 'CQPRA'                                         
509400        MOVE '2662 ' TO W-IDLEVNR-ALPHA  END-IF                           
509500     IF W-IDLEVNR-ALPHA = 'D0N8A'                                         
509600        MOVE '3350 ' TO W-IDLEVNR-ALPHA  END-IF                           
509700     IF W-IDLEVNR-ALPHA = 'S1XMB'                                         
509800        MOVE '3656 ' TO W-IDLEVNR-ALPHA  END-IF                           
509900     IF W-IDLEVNR-ALPHA = 'S1XMA'                                         
510000        MOVE '3964 ' TO W-IDLEVNR-ALPHA  END-IF                           
510100     IF W-IDLEVNR-ALPHA = 'BPGQB'                                         
510200        MOVE '402  ' TO W-IDLEVNR-ALPHA  END-IF                           
510300     IF W-IDLEVNR-ALPHA = 'S41GF'                                         
510400        MOVE '4234 ' TO W-IDLEVNR-ALPHA  END-IF                           
510500     IF W-IDLEVNR-ALPHA = 'C62JA'                                         
510600        MOVE '5191 ' TO W-IDLEVNR-ALPHA  END-IF                           
510700     IF W-IDLEVNR-ALPHA = 'C9S2A'                                         
510800        MOVE '5670 ' TO W-IDLEVNR-ALPHA  END-IF                           
510900     IF W-IDLEVNR-ALPHA = 'B050B'                                         
511000        MOVE '5674 ' TO W-IDLEVNR-ALPHA  END-IF                           
511100     IF W-IDLEVNR-ALPHA = 'AZYXB'                                         
511200        MOVE '6229 ' TO W-IDLEVNR-ALPHA  END-IF                           
511300     IF W-IDLEVNR-ALPHA = 'CFUDB'                                         
511400        MOVE '6268 ' TO W-IDLEVNR-ALPHA  END-IF                           
511500     IF W-IDLEVNR-ALPHA = 'D3P7B'                                         
511600        MOVE '6282 ' TO W-IDLEVNR-ALPHA  END-IF                           
511700     IF W-IDLEVNR-ALPHA = 'C5408'                                         
511800        MOVE '63058' TO W-IDLEVNR-ALPHA  END-IF                           
511900     IF W-IDLEVNR-ALPHA = 'D1K4A'                                         
512000        MOVE '6598 ' TO W-IDLEVNR-ALPHA  END-IF                           
512100     IF W-IDLEVNR-ALPHA = 'R151A'                                         
512200        MOVE '6666 ' TO W-IDLEVNR-ALPHA  END-IF                           
512300     IF W-IDLEVNR-ALPHA = 'S4MZA'                                         
512400        MOVE '6678 ' TO W-IDLEVNR-ALPHA  END-IF                           
512500     IF W-IDLEVNR-ALPHA = 'ENB6A'                                         
512600        MOVE '6809 ' TO W-IDLEVNR-ALPHA  END-IF                           
512700     IF W-IDLEVNR-ALPHA = 'ABYHA'                                         
512800        MOVE '6950 ' TO W-IDLEVNR-ALPHA  END-IF                           
512900     IF W-IDLEVNR-ALPHA = 'C86HA'                                         
513000        MOVE '6972 ' TO W-IDLEVNR-ALPHA  END-IF                           
513100     IF W-IDLEVNR-ALPHA = 'N2D2A'                                         
513200        MOVE '7847 ' TO W-IDLEVNR-ALPHA  END-IF                           
513300     IF W-IDLEVNR-ALPHA = 'D0GWA'                                         
513400        MOVE '7915 ' TO W-IDLEVNR-ALPHA  END-IF                           
513500     IF W-IDLEVNR-ALPHA = 'BP3JB'                                         
513600        MOVE '843  ' TO W-IDLEVNR-ALPHA  END-IF                           
513700     IF W-IDLEVNR-ALPHA = 'C9P1B'                                         
513800        MOVE '10490' TO W-IDLEVNR-ALPHA  END-IF                           
513900     IF W-IDLEVNR-ALPHA = 'CTX0A'                                         
514000        MOVE '11062' TO W-IDLEVNR-ALPHA  END-IF                           
514100     IF W-IDLEVNR-ALPHA = 'BP8AA'                                         
514200        MOVE '11086' TO W-IDLEVNR-ALPHA  END-IF                           
514300     IF W-IDLEVNR-ALPHA = 'BUWLA'                                         
514400        MOVE '11403' TO W-IDLEVNR-ALPHA  END-IF                           
514500     IF W-IDLEVNR-ALPHA = 'BQH8A'                                         
514600        MOVE '1148 ' TO W-IDLEVNR-ALPHA  END-IF                           
514700     IF W-IDLEVNR-ALPHA = 'CN5NA'                                         
514800        MOVE '11708' TO W-IDLEVNR-ALPHA  END-IF                           
514900     IF W-IDLEVNR-ALPHA = 'BJQNA'                                         
515000        MOVE '12055' TO W-IDLEVNR-ALPHA  END-IF                           
515100     IF W-IDLEVNR-ALPHA = 'Q98ZA'                                         
515200        MOVE '13241' TO W-IDLEVNR-ALPHA  END-IF                           
515300     IF W-IDLEVNR-ALPHA = 'CFUBA'                                         
515400        MOVE '13517' TO W-IDLEVNR-ALPHA  END-IF                           
515500     IF W-IDLEVNR-ALPHA = 'BJPMA'                                         
515600        MOVE '13527' TO W-IDLEVNR-ALPHA  END-IF                           
515700     IF W-IDLEVNR-ALPHA = 'BJPMB'                                         
515800        MOVE '13603' TO W-IDLEVNR-ALPHA  END-IF                           
515900     IF W-IDLEVNR-ALPHA = 'BC3EA'                                         
516000        MOVE '14668' TO W-IDLEVNR-ALPHA  END-IF                           
516100     IF W-IDLEVNR-ALPHA = 'BQ2ZA'                                         
516200        MOVE '1480 ' TO W-IDLEVNR-ALPHA  END-IF                           
516300     IF W-IDLEVNR-ALPHA = 'BQ8DA'                                         
516400        MOVE '1528 ' TO W-IDLEVNR-ALPHA  END-IF                           
516500     IF W-IDLEVNR-ALPHA = 'R9Q4A'                                         
516600        MOVE '16081' TO W-IDLEVNR-ALPHA  END-IF                           
516700     IF W-IDLEVNR-ALPHA = 'J17KA'                                         
516800        MOVE '16358' TO W-IDLEVNR-ALPHA  END-IF                           
516900     IF W-IDLEVNR-ALPHA = 'R3U8A'                                         
517000        MOVE '16371' TO W-IDLEVNR-ALPHA  END-IF                           
517100     IF W-IDLEVNR-ALPHA = 'D0N0E'                                         
517200        MOVE '16735' TO W-IDLEVNR-ALPHA  END-IF                           
517300     IF W-IDLEVNR-ALPHA = 'BQYKB'                                         
517400        MOVE '1689 ' TO W-IDLEVNR-ALPHA  END-IF                           
517500     IF W-IDLEVNR-ALPHA = 'BJPMD'                                         
517600        MOVE '21327' TO W-IDLEVNR-ALPHA  END-IF                           
517700     IF W-IDLEVNR-ALPHA = 'N7WWA'                                         
517800        MOVE '214  ' TO W-IDLEVNR-ALPHA  END-IF                           
517900     IF W-IDLEVNR-ALPHA = 'BPF3A'                                         
518000        MOVE '2213 ' TO W-IDLEVNR-ALPHA  END-IF                           
518100     IF W-IDLEVNR-ALPHA = 'BQ5QA'                                         
518200        MOVE '2222 ' TO W-IDLEVNR-ALPHA  END-IF                           
518300     IF W-IDLEVNR-ALPHA = 'BLNMA'                                         
518400        MOVE '2227 ' TO W-IDLEVNR-ALPHA  END-IF                           
518500     IF W-IDLEVNR-ALPHA = 'H7G6A'                                         
518600        MOVE '2312 ' TO W-IDLEVNR-ALPHA  END-IF                           
518700     IF W-IDLEVNR-ALPHA = 'AXQ1A'                                         
518800        MOVE '24660' TO W-IDLEVNR-ALPHA  END-IF                           
518900     IF W-IDLEVNR-ALPHA = 'AQYSA'                                         
519000        MOVE '25759' TO W-IDLEVNR-ALPHA  END-IF                           
519100     IF W-IDLEVNR-ALPHA = 'BQ6LA'                                         
519200        MOVE '25775' TO W-IDLEVNR-ALPHA  END-IF                           
519300     IF W-IDLEVNR-ALPHA = 'DDCLA'                                         
519400        MOVE '25815' TO W-IDLEVNR-ALPHA  END-IF                           
519500     IF W-IDLEVNR-ALPHA = 'CW0XA'                                         
519600        MOVE '26031' TO W-IDLEVNR-ALPHA  END-IF                           
519700     IF W-IDLEVNR-ALPHA = 'BQ6FA'                                         
519800        MOVE '2634 ' TO W-IDLEVNR-ALPHA  END-IF                           
519900     IF W-IDLEVNR-ALPHA = 'BQYLA'                                         
520000        MOVE '266  ' TO W-IDLEVNR-ALPHA  END-IF                           
520100     IF W-IDLEVNR-ALPHA = 'S9B0B'                                         
520200        MOVE '2670 ' TO W-IDLEVNR-ALPHA  END-IF                           
520300     IF W-IDLEVNR-ALPHA = 'BQ6LA'                                         
520400        MOVE '3043 ' TO W-IDLEVNR-ALPHA  END-IF                           
520500     IF W-IDLEVNR-ALPHA = 'CL3YA'                                         
520600        MOVE '3054 ' TO W-IDLEVNR-ALPHA  END-IF                           
520700     IF W-IDLEVNR-ALPHA = 'BKNKA'                                         
520800        MOVE '3096 ' TO W-IDLEVNR-ALPHA  END-IF                           
520900     IF W-IDLEVNR-ALPHA = 'B34SG'                                         
521000        MOVE '3653 ' TO W-IDLEVNR-ALPHA  END-IF                           
521100     IF W-IDLEVNR-ALPHA = 'AKUMA'                                         
521200        MOVE '3666 ' TO W-IDLEVNR-ALPHA  END-IF                           
521300     IF W-IDLEVNR-ALPHA = 'BQ6TA'                                         
521400        MOVE '3832 ' TO W-IDLEVNR-ALPHA  END-IF                           
521500     IF W-IDLEVNR-ALPHA = 'BP3CA'                                         
521600        MOVE '393  ' TO W-IDLEVNR-ALPHA  END-IF                           
521700     IF W-IDLEVNR-ALPHA = 'BQ6UA'                                         
521800        MOVE '3934 ' TO W-IDLEVNR-ALPHA  END-IF                           
521900     IF W-IDLEVNR-ALPHA = 'Q3MXA'                                         
522000        MOVE '4235 ' TO W-IDLEVNR-ALPHA  END-IF                           
522100     IF W-IDLEVNR-ALPHA = 'A7G7A'                                         
522200        MOVE '4253 ' TO W-IDLEVNR-ALPHA  END-IF                           
522300     IF W-IDLEVNR-ALPHA = 'S9B0A'                                         
522400        MOVE '4597 ' TO W-IDLEVNR-ALPHA  END-IF                           
522500     IF W-IDLEVNR-ALPHA = 'E7R9D'                                         
522600        MOVE '4985 ' TO W-IDLEVNR-ALPHA  END-IF                           
522700     IF W-IDLEVNR-ALPHA = 'BJ6BA'                                         
522800        MOVE '507  ' TO W-IDLEVNR-ALPHA  END-IF                           
522900     IF W-IDLEVNR-ALPHA = 'CN4SA'                                         
523000        MOVE '510  ' TO W-IDLEVNR-ALPHA  END-IF                           
523100     IF W-IDLEVNR-ALPHA = 'E9VKB'                                         
523200        MOVE '5177 ' TO W-IDLEVNR-ALPHA  END-IF                           
523300     IF W-IDLEVNR-ALPHA = 'BLHTA'                                         
523400        MOVE '521  ' TO W-IDLEVNR-ALPHA  END-IF                           
523500     IF W-IDLEVNR-ALPHA = 'F610B'                                         
523600        MOVE '5269 ' TO W-IDLEVNR-ALPHA  END-IF                           
523700     IF W-IDLEVNR-ALPHA = 'C8D2A'                                         
523800        MOVE '6057 ' TO W-IDLEVNR-ALPHA  END-IF                           
523900     IF W-IDLEVNR-ALPHA = 'LBEKA'                                         
524000        MOVE '6154 ' TO W-IDLEVNR-ALPHA  END-IF                           
524100     IF W-IDLEVNR-ALPHA = 'V022A'                                         
524200        MOVE '6175 ' TO W-IDLEVNR-ALPHA  END-IF                           
524300     IF W-IDLEVNR-ALPHA = 'ADSMA'                                         
524400        MOVE '6199 ' TO W-IDLEVNR-ALPHA  END-IF                           
524500     IF W-IDLEVNR-ALPHA = 'D04BB'                                         
524600        MOVE '6244 ' TO W-IDLEVNR-ALPHA  END-IF                           
524700     IF W-IDLEVNR-ALPHA = 'S2Z6B'                                         
524800        MOVE '6330 ' TO W-IDLEVNR-ALPHA  END-IF                           
524900     IF W-IDLEVNR-ALPHA = 'B40XA'                                         
525000        MOVE '6474 ' TO W-IDLEVNR-ALPHA  END-IF                           
525100     IF W-IDLEVNR-ALPHA = 'BQ7QA'                                         
525200        MOVE '6649 ' TO W-IDLEVNR-ALPHA  END-IF                           
525300     IF W-IDLEVNR-ALPHA = 'D0N0A'                                         
525400        MOVE '6849 ' TO W-IDLEVNR-ALPHA  END-IF                           
525500     IF W-IDLEVNR-ALPHA = 'B34SA'                                         
525600        MOVE '7785 ' TO W-IDLEVNR-ALPHA  END-IF                           
525700     IF W-IDLEVNR-ALPHA = 'BUTZA'                                         
525800        MOVE '7958 ' TO W-IDLEVNR-ALPHA  END-IF                           
525900     IF W-IDLEVNR-ALPHA = 'BQYKA'                                         
526000        MOVE '88   ' TO W-IDLEVNR-ALPHA  END-IF                           
526100     IF W-IDLEVNR-ALPHA = 'BKNQA'                                         
526200        MOVE '8888 ' TO W-IDLEVNR-ALPHA  END-IF                           
526300     IF W-IDLEVNR-ALPHA = 'BQ0GA'                                         
526400        MOVE '996  ' TO W-IDLEVNR-ALPHA  END-IF                           
526500*************************************************                         
526600     IF W-IDLEVNR-ALPHA = 'CFNFA'                                         
526700        MOVE '848  ' TO W-IDLEVNR-ALPHA  END-IF                           
526800     IF W-IDLEVNR-ALPHA = 'BX3PA'                                         
526900        MOVE '883  ' TO W-IDLEVNR-ALPHA  END-IF                           
527000     IF W-IDLEVNR-ALPHA = 'BJQRA'                                         
527100        MOVE '1203 ' TO W-IDLEVNR-ALPHA  END-IF                           
527200     IF W-IDLEVNR-ALPHA = 'BQ5SA'                                         
527300        MOVE '2346 ' TO W-IDLEVNR-ALPHA  END-IF                           
527400     IF W-IDLEVNR-ALPHA = 'BQEBA'                                         
527500        MOVE '2363 ' TO W-IDLEVNR-ALPHA  END-IF                           
527600     IF W-IDLEVNR-ALPHA = 'BQ5TA'                                         
527700        MOVE '2368 ' TO W-IDLEVNR-ALPHA  END-IF                           
527800     IF W-IDLEVNR-ALPHA = 'BP3EB'                                         
527900        MOVE '2419 ' TO W-IDLEVNR-ALPHA  END-IF                           
528000     IF W-IDLEVNR-ALPHA = 'AY2QA'                                         
528100        MOVE '3036 ' TO W-IDLEVNR-ALPHA  END-IF                           
528200     IF W-IDLEVNR-ALPHA = 'K3D7A'                                         
528300        MOVE '3712 ' TO W-IDLEVNR-ALPHA  END-IF                           
528400     IF W-IDLEVNR-ALPHA = 'X448A'                                         
528500        MOVE '5249 ' TO W-IDLEVNR-ALPHA  END-IF                           
528600     IF W-IDLEVNR-ALPHA = 'D22FA'                                         
528700        MOVE '6040 ' TO W-IDLEVNR-ALPHA  END-IF                           
528800     IF W-IDLEVNR-ALPHA = 'KKM2A'                                         
528900        MOVE '6174 ' TO W-IDLEVNR-ALPHA  END-IF                           
529000     IF W-IDLEVNR-ALPHA = 'BCK8A'                                         
529100        MOVE '6385 ' TO W-IDLEVNR-ALPHA  END-IF                           
529200     IF W-IDLEVNR-ALPHA = 'T9UZA'                                         
529300        MOVE '6485 ' TO W-IDLEVNR-ALPHA  END-IF                           
529400     IF W-IDLEVNR-ALPHA = 'BQ7NA'                                         
529500        MOVE '6513 ' TO W-IDLEVNR-ALPHA  END-IF                           
529600     IF W-IDLEVNR-ALPHA = 'M279A'                                         
529700        MOVE '6600 ' TO W-IDLEVNR-ALPHA  END-IF                           
529800     IF W-IDLEVNR-ALPHA = 'KTY7A'                                         
529900        MOVE '6614 ' TO W-IDLEVNR-ALPHA  END-IF                           
530000     IF W-IDLEVNR-ALPHA = 'D21UA'                                         
530100        MOVE '6701 ' TO W-IDLEVNR-ALPHA  END-IF                           
530200     IF W-IDLEVNR-ALPHA = 'CFTZA'                                         
530300        MOVE '6952 ' TO W-IDLEVNR-ALPHA  END-IF                           
530400     IF W-IDLEVNR-ALPHA = 'D3T6A'                                         
530500        MOVE '7042 ' TO W-IDLEVNR-ALPHA  END-IF                           
530600     IF W-IDLEVNR-ALPHA = 'BQ7TA'                                         
530700        MOVE '7052 ' TO W-IDLEVNR-ALPHA  END-IF                           
530800     IF W-IDLEVNR-ALPHA = 'R742A'                                         
530900        MOVE '7179 ' TO W-IDLEVNR-ALPHA  END-IF                           
531000     IF W-IDLEVNR-ALPHA = 'CXA0A'                                         
531100        MOVE '7189 ' TO W-IDLEVNR-ALPHA  END-IF                           
531200     IF W-IDLEVNR-ALPHA = 'CXA1A'                                         
531300        MOVE '7190 ' TO W-IDLEVNR-ALPHA  END-IF                           
531400     IF W-IDLEVNR-ALPHA = 'Q725B'                                         
531500        MOVE '7246 ' TO W-IDLEVNR-ALPHA  END-IF                           
531600     IF W-IDLEVNR-ALPHA = 'S35LA'                                         
531700        MOVE '7253 ' TO W-IDLEVNR-ALPHA  END-IF                           
531800     IF W-IDLEVNR-ALPHA = 'G1UHS'                                         
531900        MOVE '7255 ' TO W-IDLEVNR-ALPHA  END-IF                           
532000     IF W-IDLEVNR-ALPHA = 'BP3KA'                                         
532100        MOVE '7470 ' TO W-IDLEVNR-ALPHA  END-IF                           
532200     IF W-IDLEVNR-ALPHA = 'CEFPA'                                         
532300        MOVE '7787 ' TO W-IDLEVNR-ALPHA  END-IF                           
532400     IF W-IDLEVNR-ALPHA = 'CDHVA'                                         
532500        MOVE '10024' TO W-IDLEVNR-ALPHA  END-IF                           
532600     IF W-IDLEVNR-ALPHA = 'BP3EA'                                         
532700        MOVE '10121' TO W-IDLEVNR-ALPHA  END-IF                           
532800     IF W-IDLEVNR-ALPHA = 'CXMTA'                                         
532900        MOVE '10290' TO W-IDLEVNR-ALPHA  END-IF                           
533000     IF W-IDLEVNR-ALPHA = 'BJQRD'                                         
533100        MOVE '10324' TO W-IDLEVNR-ALPHA  END-IF                           
533200     IF W-IDLEVNR-ALPHA = 'C7C5A'                                         
533300        MOVE '10510' TO W-IDLEVNR-ALPHA  END-IF                           
533400     IF W-IDLEVNR-ALPHA = 'BJQRB'                                         
533500        MOVE '10657' TO W-IDLEVNR-ALPHA  END-IF                           
533600     IF W-IDLEVNR-ALPHA = 'V4B1B'                                         
533700        MOVE '11100' TO W-IDLEVNR-ALPHA  END-IF                           
533800     IF W-IDLEVNR-ALPHA = 'G8VTA'                                         
533900        MOVE '11101' TO W-IDLEVNR-ALPHA  END-IF                           
534000     IF W-IDLEVNR-ALPHA = 'BCK8B'                                         
534100        MOVE '11137' TO W-IDLEVNR-ALPHA  END-IF                           
534200     IF W-IDLEVNR-ALPHA = 'BJQRC'                                         
534300        MOVE '11139' TO W-IDLEVNR-ALPHA  END-IF                           
534400     IF W-IDLEVNR-ALPHA = 'BQEBB'                                         
534500        MOVE '11155' TO W-IDLEVNR-ALPHA  END-IF                           
534600     IF W-IDLEVNR-ALPHA = 'E521H'                                         
534700        MOVE '11388' TO W-IDLEVNR-ALPHA  END-IF                           
534800     IF W-IDLEVNR-ALPHA = 'B491E'                                         
534900        MOVE '12637' TO W-IDLEVNR-ALPHA  END-IF                           
535000     IF W-IDLEVNR-ALPHA = 'T3WQA'                                         
535100        MOVE '13537' TO W-IDLEVNR-ALPHA  END-IF                           
535200     IF W-IDLEVNR-ALPHA = 'BPTRA'                                         
535300        MOVE '13550' TO W-IDLEVNR-ALPHA  END-IF                           
535400     IF W-IDLEVNR-ALPHA = 'BPK4A'                                         
535500        MOVE '13777' TO W-IDLEVNR-ALPHA  END-IF                           
535600     IF W-IDLEVNR-ALPHA = 'C79MA'                                         
535700        MOVE '14926' TO W-IDLEVNR-ALPHA  END-IF                           
535800     IF W-IDLEVNR-ALPHA = 'D5E1C'                                         
535900        MOVE '14927' TO W-IDLEVNR-ALPHA  END-IF                           
536000     IF W-IDLEVNR-ALPHA = 'C79ME'                                         
536100        MOVE '14941' TO W-IDLEVNR-ALPHA  END-IF                           
536200     IF W-IDLEVNR-ALPHA = 'T9UZB'                                         
536300        MOVE '16021' TO W-IDLEVNR-ALPHA  END-IF                           
536400     IF W-IDLEVNR-ALPHA = 'B40YA'                                         
536500        MOVE '16084' TO W-IDLEVNR-ALPHA  END-IF                           
536600     IF W-IDLEVNR-ALPHA = 'D38CA'                                         
536700        MOVE '16385' TO W-IDLEVNR-ALPHA  END-IF                           
536800     IF W-IDLEVNR-ALPHA = 'MKB2A'                                         
536900        MOVE '16386' TO W-IDLEVNR-ALPHA  END-IF                           
537000     IF W-IDLEVNR-ALPHA = 'G0MMA'                                         
537100        MOVE '17777' TO W-IDLEVNR-ALPHA  END-IF                           
537200     IF W-IDLEVNR-ALPHA = 'T43NA'                                         
537300        MOVE '17801' TO W-IDLEVNR-ALPHA  END-IF                           
537400     IF W-IDLEVNR-ALPHA = 'S5VSA'                                         
537500        MOVE '18038' TO W-IDLEVNR-ALPHA  END-IF                           
537600     IF W-IDLEVNR-ALPHA = 'CBBNA'                                         
537700        MOVE '18059' TO W-IDLEVNR-ALPHA  END-IF                           
537800     IF W-IDLEVNR-ALPHA = 'B45VD'                                         
537900        MOVE '19725' TO W-IDLEVNR-ALPHA  END-IF                           
538000     IF W-IDLEVNR-ALPHA = 'MKB2B'                                         
538100        MOVE '19995' TO W-IDLEVNR-ALPHA  END-IF                           
538200     IF W-IDLEVNR-ALPHA = 'Q9K3B'                                         
538300        MOVE '23511' TO W-IDLEVNR-ALPHA  END-IF                           
538400     IF W-IDLEVNR-ALPHA = 'S35LB'                                         
538500        MOVE '24248' TO W-IDLEVNR-ALPHA  END-IF                           
538600     IF W-IDLEVNR-ALPHA = 'R742B'                                         
538700        MOVE '24332' TO W-IDLEVNR-ALPHA  END-IF                           
538800     IF W-IDLEVNR-ALPHA = 'ND04W'                                         
538900        MOVE '24967' TO W-IDLEVNR-ALPHA  END-IF                           
539000     IF W-IDLEVNR-ALPHA = 'N8U4A'                                         
539100        MOVE '25784' TO W-IDLEVNR-ALPHA  END-IF                           
539200     IF W-IDLEVNR-ALPHA = 'BPTQC'                                         
539300        MOVE '25809' TO W-IDLEVNR-ALPHA  END-IF                           
539400     IF W-IDLEVNR-ALPHA = 'D08GJ'                                         
539500        MOVE '25889' TO W-IDLEVNR-ALPHA  END-IF                           
539600     IF W-IDLEVNR-ALPHA = 'S044X'                                         
539700        MOVE '25895' TO W-IDLEVNR-ALPHA  END-IF                           
539800     IF W-IDLEVNR-ALPHA = 'B46ZA'                                         
539900        MOVE '25919' TO W-IDLEVNR-ALPHA  END-IF                           
540000     IF W-IDLEVNR-ALPHA = 'KTY7D'                                         
540100        MOVE '25949' TO W-IDLEVNR-ALPHA  END-IF                           
540200*************************************************                         
540300     IF W-IDLEVNR-ALPHA = 'DRDGA'                                         
540400        MOVE '15   ' TO W-IDLEVNR-ALPHA  END-IF                           
540500     IF W-IDLEVNR-ALPHA = 'S69YA'                                         
540600        MOVE '19   ' TO W-IDLEVNR-ALPHA  END-IF                           
540700     IF W-IDLEVNR-ALPHA = 'BLJ6A'                                         
540800        MOVE '177  ' TO W-IDLEVNR-ALPHA  END-IF                           
540900     IF W-IDLEVNR-ALPHA = 'BQ6HA'                                         
541000        MOVE '527  ' TO W-IDLEVNR-ALPHA  END-IF                           
541100     IF W-IDLEVNR-ALPHA = 'AY0MA'                                         
541200        MOVE '745  ' TO W-IDLEVNR-ALPHA  END-IF                           
541300     IF W-IDLEVNR-ALPHA = 'BLZWA'                                         
541400        MOVE '870  ' TO W-IDLEVNR-ALPHA  END-IF                           
541500     IF W-IDLEVNR-ALPHA = 'BQ2BA'                                         
541600        MOVE '1049 ' TO W-IDLEVNR-ALPHA  END-IF                           
541700     IF W-IDLEVNR-ALPHA = 'CFNJA'                                         
541800        MOVE '1456 ' TO W-IDLEVNR-ALPHA  END-IF                           
541900     IF W-IDLEVNR-ALPHA = 'CN4VA'                                         
542000        MOVE '1787 ' TO W-IDLEVNR-ALPHA  END-IF                           
542100     IF W-IDLEVNR-ALPHA = 'BQ3XA'                                         
542200        MOVE '1865 ' TO W-IDLEVNR-ALPHA  END-IF                           
542300     IF W-IDLEVNR-ALPHA = 'CFTNA'                                         
542400        MOVE '2054 ' TO W-IDLEVNR-ALPHA  END-IF                           
542500     IF W-IDLEVNR-ALPHA = 'BX9LA'                                         
542600        MOVE '2103 ' TO W-IDLEVNR-ALPHA  END-IF                           
542700     IF W-IDLEVNR-ALPHA = 'S6R1A'                                         
542800        MOVE '2261 ' TO W-IDLEVNR-ALPHA  END-IF                           
542900     IF W-IDLEVNR-ALPHA = 'BJ6HA'                                         
543000        MOVE '2406 ' TO W-IDLEVNR-ALPHA  END-IF                           
543100     IF W-IDLEVNR-ALPHA = 'BQ5WA'                                         
543200        MOVE '2417 ' TO W-IDLEVNR-ALPHA  END-IF                           
543300     IF W-IDLEVNR-ALPHA = 'BQ5XA'                                         
543400        MOVE '2423 ' TO W-IDLEVNR-ALPHA  END-IF                           
543500     IF W-IDLEVNR-ALPHA = 'CFNUA'                                         
543600        MOVE '2457 ' TO W-IDLEVNR-ALPHA  END-IF                           
543700     IF W-IDLEVNR-ALPHA = 'BYL8A'                                         
543800        MOVE '2490 ' TO W-IDLEVNR-ALPHA  END-IF                           
543900     IF W-IDLEVNR-ALPHA = 'DL1WA'                                         
544000        MOVE '3404 ' TO W-IDLEVNR-ALPHA  END-IF                           
544100     IF W-IDLEVNR-ALPHA = 'K4STA'                                         
544200        MOVE '3572 ' TO W-IDLEVNR-ALPHA  END-IF                           
544300     IF W-IDLEVNR-ALPHA = 'A0VWA'                                         
544400        MOVE '3747 ' TO W-IDLEVNR-ALPHA  END-IF                           
544500     IF W-IDLEVNR-ALPHA = 'R39QA'                                         
544600        MOVE '3815 ' TO W-IDLEVNR-ALPHA  END-IF                           
544700     IF W-IDLEVNR-ALPHA = 'DLJLA'                                         
544800        MOVE '3898 ' TO W-IDLEVNR-ALPHA  END-IF                           
544900     IF W-IDLEVNR-ALPHA = 'BWMAA'                                         
545000        MOVE '3902 ' TO W-IDLEVNR-ALPHA  END-IF                           
545100     IF W-IDLEVNR-ALPHA = 'CQPSA'                                         
545200        MOVE '3918 ' TO W-IDLEVNR-ALPHA  END-IF                           
545300     IF W-IDLEVNR-ALPHA = 'K3D7B'                                         
545400        MOVE '3965 ' TO W-IDLEVNR-ALPHA  END-IF                           
545500     IF W-IDLEVNR-ALPHA = 'G273T'                                         
545600        MOVE '4164 ' TO W-IDLEVNR-ALPHA  END-IF                           
545700     IF W-IDLEVNR-ALPHA = 'G255C'                                         
545800        MOVE '4239 ' TO W-IDLEVNR-ALPHA  END-IF                           
545900     IF W-IDLEVNR-ALPHA = 'C212A'                                         
546000        MOVE '4274 ' TO W-IDLEVNR-ALPHA  END-IF                           
546100     IF W-IDLEVNR-ALPHA = 'EQ17A'                                         
546200        MOVE '4344 ' TO W-IDLEVNR-ALPHA  END-IF                           
546300     IF W-IDLEVNR-ALPHA = 'CN5BA'                                         
546400        MOVE '4528 ' TO W-IDLEVNR-ALPHA  END-IF                           
546500     IF W-IDLEVNR-ALPHA = 'C8Q0A'                                         
546600        MOVE '4955 ' TO W-IDLEVNR-ALPHA  END-IF                           
546700     IF W-IDLEVNR-ALPHA = 'K817J'                                         
546800        MOVE '4994 ' TO W-IDLEVNR-ALPHA  END-IF                           
546900     IF W-IDLEVNR-ALPHA = 'B40QG'                                         
547000        MOVE '5019 ' TO W-IDLEVNR-ALPHA  END-IF                           
547100     IF W-IDLEVNR-ALPHA = 'C7G4A'                                         
547200        MOVE '5051 ' TO W-IDLEVNR-ALPHA  END-IF                           
547300     IF W-IDLEVNR-ALPHA = 'C9A2A'                                         
547400        MOVE '5135 ' TO W-IDLEVNR-ALPHA  END-IF                           
547500     IF W-IDLEVNR-ALPHA = 'CFN4A'                                         
547600        MOVE '5287 ' TO W-IDLEVNR-ALPHA  END-IF                           
547700     IF W-IDLEVNR-ALPHA = 'P8D3A'                                         
547800        MOVE '5595 ' TO W-IDLEVNR-ALPHA  END-IF                           
547900     IF W-IDLEVNR-ALPHA = 'F962A'                                         
548000        MOVE '6026 ' TO W-IDLEVNR-ALPHA  END-IF                           
548100     IF W-IDLEVNR-ALPHA = 'DL6KA'                                         
548200        MOVE '6163 ' TO W-IDLEVNR-ALPHA  END-IF                           
548300     IF W-IDLEVNR-ALPHA = 'D0SAA'                                         
548400        MOVE '6235 ' TO W-IDLEVNR-ALPHA  END-IF                           
548500     IF W-IDLEVNR-ALPHA = 'DLLEA'                                         
548600        MOVE '6296 ' TO W-IDLEVNR-ALPHA  END-IF                           
548700     IF W-IDLEVNR-ALPHA = 'BX6BC'                                         
548800        MOVE '6335 ' TO W-IDLEVNR-ALPHA  END-IF                           
548900     IF W-IDLEVNR-ALPHA = 'CUQLA'                                         
549000        MOVE '6584 ' TO W-IDLEVNR-ALPHA  END-IF                           
549100     IF W-IDLEVNR-ALPHA = 'T1X5A'                                         
549200        MOVE '6838 ' TO W-IDLEVNR-ALPHA  END-IF                           
549300     IF W-IDLEVNR-ALPHA = 'D3R7A'                                         
549400        MOVE '6908 ' TO W-IDLEVNR-ALPHA  END-IF                           
549500     IF W-IDLEVNR-ALPHA = 'CFT1A'                                         
549600        MOVE '7203 ' TO W-IDLEVNR-ALPHA  END-IF                           
549700     IF W-IDLEVNR-ALPHA = 'K8XJA'                                         
549800        MOVE '7231 ' TO W-IDLEVNR-ALPHA  END-IF                           
549900     IF W-IDLEVNR-ALPHA = 'BP3JA'                                         
550000        MOVE '7367 ' TO W-IDLEVNR-ALPHA  END-IF                           
550100     IF W-IDLEVNR-ALPHA = 'DT9EA'                                         
550200        MOVE '7662 ' TO W-IDLEVNR-ALPHA  END-IF                           
550300     IF W-IDLEVNR-ALPHA = 'BH6NA'                                         
550400        MOVE '7724 ' TO W-IDLEVNR-ALPHA  END-IF                           
550500     IF W-IDLEVNR-ALPHA = 'CKSMA'                                         
550600        MOVE '7767 ' TO W-IDLEVNR-ALPHA  END-IF                           
550700     IF W-IDLEVNR-ALPHA = 'BQ8GA'                                         
550800        MOVE '7786 ' TO W-IDLEVNR-ALPHA  END-IF                           
550900     IF W-IDLEVNR-ALPHA = 'DLLSA'                                         
551000        MOVE '7821 ' TO W-IDLEVNR-ALPHA  END-IF                           
551100     IF W-IDLEVNR-ALPHA = 'DBF3A'                                         
551200        MOVE '7956 ' TO W-IDLEVNR-ALPHA  END-IF                           
551300     IF W-IDLEVNR-ALPHA = 'H222A'                                         
551400        MOVE '7961 ' TO W-IDLEVNR-ALPHA  END-IF                           
551500     IF W-IDLEVNR-ALPHA = 'P1NQA'                                         
551600        MOVE '8204 ' TO W-IDLEVNR-ALPHA  END-IF                           
551700     IF W-IDLEVNR-ALPHA = 'CFNNA'                                         
551800        MOVE '10105' TO W-IDLEVNR-ALPHA  END-IF                           
551900     IF W-IDLEVNR-ALPHA = 'CN5LA'                                         
552000        MOVE '10141' TO W-IDLEVNR-ALPHA  END-IF                           
552100     IF W-IDLEVNR-ALPHA = 'L8K5R'                                         
552200        MOVE '10178' TO W-IDLEVNR-ALPHA  END-IF                           
552300     IF W-IDLEVNR-ALPHA = 'BRV3A'                                         
552400        MOVE '10339' TO W-IDLEVNR-ALPHA  END-IF                           
552500     IF W-IDLEVNR-ALPHA = 'L8K5G'                                         
552600        MOVE '10342' TO W-IDLEVNR-ALPHA  END-IF                           
552700     IF W-IDLEVNR-ALPHA = 'U7PMA'                                         
552800        MOVE '10986' TO W-IDLEVNR-ALPHA  END-IF                           
552900     IF W-IDLEVNR-ALPHA = 'E1P5A'                                         
553000        MOVE '11578' TO W-IDLEVNR-ALPHA  END-IF                           
553100     IF W-IDLEVNR-ALPHA = 'BQ9EA'                                         
553200        MOVE '12569' TO W-IDLEVNR-ALPHA  END-IF                           
553300     IF W-IDLEVNR-ALPHA = 'T655C'                                         
553400        MOVE '13011' TO W-IDLEVNR-ALPHA  END-IF                           
553500     IF W-IDLEVNR-ALPHA = 'D3U6A'                                         
553600        MOVE '13386' TO W-IDLEVNR-ALPHA  END-IF                           
553700     IF W-IDLEVNR-ALPHA = 'S4LBA'                                         
553800        MOVE '13391' TO W-IDLEVNR-ALPHA  END-IF                           
553900     IF W-IDLEVNR-ALPHA = 'DPVGA'                                         
554000        MOVE '13486' TO W-IDLEVNR-ALPHA  END-IF                           
554100     IF W-IDLEVNR-ALPHA = 'BQ9HB'                                         
554200        MOVE '13516' TO W-IDLEVNR-ALPHA  END-IF                           
554300     IF W-IDLEVNR-ALPHA = 'M9TPB'                                         
554400        MOVE '13556' TO W-IDLEVNR-ALPHA  END-IF                           
554500     IF W-IDLEVNR-ALPHA = 'BN7SA'                                         
554600        MOVE '13598' TO W-IDLEVNR-ALPHA  END-IF                           
554700     IF W-IDLEVNR-ALPHA = 'BQ9LA'                                         
554800        MOVE '13600' TO W-IDLEVNR-ALPHA  END-IF                           
554900     IF W-IDLEVNR-ALPHA = 'BQ9HC'                                         
555000        MOVE '13661' TO W-IDLEVNR-ALPHA  END-IF                           
555100     IF W-IDLEVNR-ALPHA = 'V4FWA'                                         
555200        MOVE '14313' TO W-IDLEVNR-ALPHA  END-IF                           
555300     IF W-IDLEVNR-ALPHA = 'D45NA'                                         
555400        MOVE '14602' TO W-IDLEVNR-ALPHA  END-IF                           
555500     IF W-IDLEVNR-ALPHA = 'L8K5A'                                         
555600        MOVE '14605' TO W-IDLEVNR-ALPHA  END-IF                           
555700     IF W-IDLEVNR-ALPHA = 'D26YB'                                         
555800        MOVE '15580' TO W-IDLEVNR-ALPHA  END-IF                           
555900     IF W-IDLEVNR-ALPHA = 'BTKBA'                                         
556000        MOVE '16048' TO W-IDLEVNR-ALPHA  END-IF                           
556100     IF W-IDLEVNR-ALPHA = 'S13SA'                                         
556200        MOVE '16058' TO W-IDLEVNR-ALPHA  END-IF                           
556300     IF W-IDLEVNR-ALPHA = 'E510F'                                         
556400        MOVE '16132' TO W-IDLEVNR-ALPHA  END-IF                           
556500     IF W-IDLEVNR-ALPHA = 'R7B3A'                                         
556600        MOVE '16158' TO W-IDLEVNR-ALPHA  END-IF                           
556700     IF W-IDLEVNR-ALPHA = 'CJ0GA'                                         
556800        MOVE '16251' TO W-IDLEVNR-ALPHA  END-IF                           
556900     IF W-IDLEVNR-ALPHA = 'S1YHA'                                         
557000        MOVE '16267' TO W-IDLEVNR-ALPHA  END-IF                           
557100     IF W-IDLEVNR-ALPHA = 'BQ9UA'                                         
557200        MOVE '16325' TO W-IDLEVNR-ALPHA  END-IF                           
557300     IF W-IDLEVNR-ALPHA = 'T446A'                                         
557400        MOVE '16470' TO W-IDLEVNR-ALPHA  END-IF                           
557500     IF W-IDLEVNR-ALPHA = 'B44XE'                                         
557600        MOVE '16490' TO W-IDLEVNR-ALPHA  END-IF                           
557700     IF W-IDLEVNR-ALPHA = 'BRV3B'                                         
557800        MOVE '17712' TO W-IDLEVNR-ALPHA  END-IF                           
557900     IF W-IDLEVNR-ALPHA = 'CN5QA'                                         
558000        MOVE '17713' TO W-IDLEVNR-ALPHA  END-IF                           
558100     IF W-IDLEVNR-ALPHA = 'CN5SA'                                         
558200        MOVE '17819' TO W-IDLEVNR-ALPHA  END-IF                           
558300     IF W-IDLEVNR-ALPHA = 'CW5YA'                                         
558400        MOVE '17819' TO W-IDLEVNR-ALPHA  END-IF                           
558500     IF W-IDLEVNR-ALPHA = 'DSHRA'                                         
558600        MOVE '18971' TO W-IDLEVNR-ALPHA  END-IF                           
558700     IF W-IDLEVNR-ALPHA = 'C9T2C'                                         
558800        MOVE '18997' TO W-IDLEVNR-ALPHA  END-IF                           
558900     IF W-IDLEVNR-ALPHA = 'AA8SA'                                         
559000        MOVE '19892' TO W-IDLEVNR-ALPHA  END-IF                           
559100     IF W-IDLEVNR-ALPHA = 'F641A'                                         
559200        MOVE '23093' TO W-IDLEVNR-ALPHA  END-IF                           
559300     IF W-IDLEVNR-ALPHA = 'DLNVA'                                         
559400        MOVE '23685' TO W-IDLEVNR-ALPHA  END-IF                           
559500     IF W-IDLEVNR-ALPHA = 'P108C'                                         
559600        MOVE '24263' TO W-IDLEVNR-ALPHA  END-IF                           
559700     IF W-IDLEVNR-ALPHA = 'CT3JA'                                         
559800        MOVE '24521' TO W-IDLEVNR-ALPHA  END-IF                           
559900     IF W-IDLEVNR-ALPHA = 'CW0HA'                                         
560000        MOVE '24653' TO W-IDLEVNR-ALPHA  END-IF                           
560100     IF W-IDLEVNR-ALPHA = 'CVVEA'                                         
560200        MOVE '24789' TO W-IDLEVNR-ALPHA  END-IF                           
560300     IF W-IDLEVNR-ALPHA = 'S5U0B'                                         
560400        MOVE '24896' TO W-IDLEVNR-ALPHA  END-IF                           
560500     IF W-IDLEVNR-ALPHA = 'CYSZB'                                         
560600        MOVE '24921' TO W-IDLEVNR-ALPHA  END-IF                           
560700     IF W-IDLEVNR-ALPHA = 'C72GA'                                         
560800        MOVE '24999' TO W-IDLEVNR-ALPHA  END-IF                           
560900     IF W-IDLEVNR-ALPHA = 'CYFWA'                                         
561000        MOVE '25286' TO W-IDLEVNR-ALPHA  END-IF                           
561100     IF W-IDLEVNR-ALPHA = 'BKL3A'                                         
561200        MOVE '25402' TO W-IDLEVNR-ALPHA  END-IF                           
561300     IF W-IDLEVNR-ALPHA = 'CZSEB'                                         
561400        MOVE '25620' TO W-IDLEVNR-ALPHA  END-IF                           
561500     IF W-IDLEVNR-ALPHA = 'DBBTA'                                         
561600        MOVE '25755' TO W-IDLEVNR-ALPHA  END-IF                           
561700     IF W-IDLEVNR-ALPHA = 'DA9EA'                                         
561800        MOVE '25765' TO W-IDLEVNR-ALPHA  END-IF                           
561900     IF W-IDLEVNR-ALPHA = 'JWMJA'                                         
562000        MOVE '25770' TO W-IDLEVNR-ALPHA  END-IF                           
562100     IF W-IDLEVNR-ALPHA = 'CYDAB'                                         
562200        MOVE '25797' TO W-IDLEVNR-ALPHA  END-IF                           
562300     IF W-IDLEVNR-ALPHA = 'EQ62A'                                         
562400        MOVE '25808' TO W-IDLEVNR-ALPHA  END-IF                           
562500     IF W-IDLEVNR-ALPHA = 'DDA4A'                                         
562600        MOVE '25810' TO W-IDLEVNR-ALPHA  END-IF                           
562700     IF W-IDLEVNR-ALPHA = 'DDETA'                                         
562800        MOVE '25818' TO W-IDLEVNR-ALPHA  END-IF                           
562900     IF W-IDLEVNR-ALPHA = 'V2NYA'                                         
563000        MOVE '25847' TO W-IDLEVNR-ALPHA  END-IF                           
563100     IF W-IDLEVNR-ALPHA = 'T655D'                                         
563200        MOVE '25859' TO W-IDLEVNR-ALPHA  END-IF                           
563300     IF W-IDLEVNR-ALPHA = 'G261S'                                         
563400        MOVE '25869' TO W-IDLEVNR-ALPHA  END-IF                           
563500     IF W-IDLEVNR-ALPHA = 'DH0RA'                                         
563600        MOVE '25870' TO W-IDLEVNR-ALPHA  END-IF                           
563700     IF W-IDLEVNR-ALPHA = 'BNWSE'                                         
563800        MOVE '25972' TO W-IDLEVNR-ALPHA  END-IF                           
563900     IF W-IDLEVNR-ALPHA = 'DRTHA'                                         
564000        MOVE '25996' TO W-IDLEVNR-ALPHA  END-IF                           
564100     IF W-IDLEVNR-ALPHA = 'BP8HA'                                         
564200        MOVE '34621' TO W-IDLEVNR-ALPHA  END-IF                           
564300     IF W-IDLEVNR-ALPHA = 'U9JXA'                                         
564400        MOVE '51065' TO W-IDLEVNR-ALPHA  END-IF                           
564500     IF W-IDLEVNR-ALPHA = 'A405A'                                         
564600        MOVE '51103' TO W-IDLEVNR-ALPHA  END-IF                           
564700     IF W-IDLEVNR-ALPHA = 'T733E'                                         
564800        MOVE '51568' TO W-IDLEVNR-ALPHA  END-IF                           
564900     IF W-IDLEVNR-ALPHA = 'F903H'                                         
565000        MOVE '62513' TO W-IDLEVNR-ALPHA  END-IF                           
565100     IF W-IDLEVNR-ALPHA = 'DCYQA'                                         
565200        MOVE '63300' TO W-IDLEVNR-ALPHA  END-IF                           
565300*************************************************                         
565400     IF W-IDLEVNR-ALPHA = 'C61MA'                                         
565500        MOVE '82   ' TO W-IDLEVNR-ALPHA  END-IF                           
565600     IF W-IDLEVNR-ALPHA = 'W064Z'                                         
565700        MOVE '4001 ' TO W-IDLEVNR-ALPHA  END-IF                           
565800     IF W-IDLEVNR-ALPHA = 'F260B'                                         
565900        MOVE '4038 ' TO W-IDLEVNR-ALPHA  END-IF                           
566000     IF W-IDLEVNR-ALPHA = 'BPQUA'                                         
566100        MOVE '4210 ' TO W-IDLEVNR-ALPHA  END-IF                           
566200     IF W-IDLEVNR-ALPHA = 'L217Q'                                         
566300        MOVE '4230 ' TO W-IDLEVNR-ALPHA  END-IF                           
566400     IF W-IDLEVNR-ALPHA = 'BPQUA'                                         
566500        MOVE '4280 ' TO W-IDLEVNR-ALPHA  END-IF                           
566600     IF W-IDLEVNR-ALPHA = 'BPGRA'                                         
566700        MOVE '4536 ' TO W-IDLEVNR-ALPHA  END-IF                           
566800     IF W-IDLEVNR-ALPHA = 'KFBGA'                                         
566900        MOVE '4667 ' TO W-IDLEVNR-ALPHA  END-IF                           
567000     IF W-IDLEVNR-ALPHA = 'D00MD'                                         
567100        MOVE '4921 ' TO W-IDLEVNR-ALPHA  END-IF                           
567200     IF W-IDLEVNR-ALPHA = 'D00MB'                                         
567300        MOVE '4930 ' TO W-IDLEVNR-ALPHA  END-IF                           
567400     IF W-IDLEVNR-ALPHA = 'D2T1E'                                         
567500        MOVE '6434 ' TO W-IDLEVNR-ALPHA  END-IF                           
567600     IF W-IDLEVNR-ALPHA = 'V1W4A'                                         
567700        MOVE '6969 ' TO W-IDLEVNR-ALPHA  END-IF                           
567800     IF W-IDLEVNR-ALPHA = 'J7TDB'                                         
567900        MOVE '7254 ' TO W-IDLEVNR-ALPHA  END-IF                           
568000     IF W-IDLEVNR-ALPHA = 'P981A'                                         
568100        MOVE '7994 ' TO W-IDLEVNR-ALPHA  END-IF                           
568200     IF W-IDLEVNR-ALPHA = 'TC12A'                                         
568300        MOVE '12101' TO W-IDLEVNR-ALPHA  END-IF                           
568400     IF W-IDLEVNR-ALPHA = 'BNWSB'                                         
568500        MOVE '13673' TO W-IDLEVNR-ALPHA  END-IF                           
568600     IF W-IDLEVNR-ALPHA = 'D00ME'                                         
568700        MOVE '14345' TO W-IDLEVNR-ALPHA  END-IF                           
568800     IF W-IDLEVNR-ALPHA = 'BQ6XB'                                         
568900        MOVE '14642' TO W-IDLEVNR-ALPHA  END-IF                           
569000     IF W-IDLEVNR-ALPHA = 'B468X'                                         
569100        MOVE '17064' TO W-IDLEVNR-ALPHA  END-IF                           
569200     IF W-IDLEVNR-ALPHA = 'B535B'                                         
569300        MOVE '18992' TO W-IDLEVNR-ALPHA  END-IF                           
569400     IF W-IDLEVNR-ALPHA = 'CQPPA'                                         
569500        MOVE '19018' TO W-IDLEVNR-ALPHA  END-IF                           
569600     IF W-IDLEVNR-ALPHA = 'T6A3A'                                         
569700        MOVE '23559' TO W-IDLEVNR-ALPHA  END-IF                           
569800     IF W-IDLEVNR-ALPHA = 'D00MF'                                         
569900        MOVE '23560' TO W-IDLEVNR-ALPHA  END-IF                           
570000     IF W-IDLEVNR-ALPHA = 'B40CC'                                         
570100        MOVE '23758' TO W-IDLEVNR-ALPHA  END-IF                           
570200     IF W-IDLEVNR-ALPHA = 'MLE6B'                                         
570300        MOVE '25832' TO W-IDLEVNR-ALPHA  END-IF                           
570400     IF W-IDLEVNR-ALPHA = 'D00MG'                                         
570500        MOVE '25925' TO W-IDLEVNR-ALPHA  END-IF                           
570600     IF W-IDLEVNR-ALPHA = 'D00MH'                                         
570700        MOVE '34345' TO W-IDLEVNR-ALPHA  END-IF                           
570800     IF W-IDLEVNR-ALPHA = 'R235B'                                         
570900        MOVE '50091' TO W-IDLEVNR-ALPHA  END-IF                           
571000     IF W-IDLEVNR-ALPHA = 'M617T'                                         
571100        MOVE '62385' TO W-IDLEVNR-ALPHA  END-IF                           
571200     IF W-IDLEVNR-ALPHA = 'R235G'                                         
571300        MOVE '63544' TO W-IDLEVNR-ALPHA  END-IF                           
571400*************************************************                         
571500     IF W-IDLEVNR-ALPHA = 'CQF3A'                                         
571600        MOVE '124  ' TO W-IDLEVNR-ALPHA  END-IF                           
571700     IF W-IDLEVNR-ALPHA = 'BJRAA'                                         
571800        MOVE '175  ' TO W-IDLEVNR-ALPHA  END-IF                           
571900     IF W-IDLEVNR-ALPHA = 'DFVDA'                                         
572000        MOVE '281  ' TO W-IDLEVNR-ALPHA  END-IF                           
572100     IF W-IDLEVNR-ALPHA = 'BJVLA'                                         
572200        MOVE '312  ' TO W-IDLEVNR-ALPHA  END-IF                           
572300     IF W-IDLEVNR-ALPHA = 'BQ1PA'                                         
572400        MOVE '966  ' TO W-IDLEVNR-ALPHA  END-IF                           
572500     IF W-IDLEVNR-ALPHA = 'S3DHA'                                         
572600        MOVE '1125 ' TO W-IDLEVNR-ALPHA  END-IF                           
572700     IF W-IDLEVNR-ALPHA = 'CFNGA'                                         
572800        MOVE '1235 ' TO W-IDLEVNR-ALPHA  END-IF                           
572900     IF W-IDLEVNR-ALPHA = 'CFNHA'                                         
573000        MOVE '1271 ' TO W-IDLEVNR-ALPHA  END-IF                           
573100     IF W-IDLEVNR-ALPHA = 'BKTVA'                                         
573200        MOVE '1495 ' TO W-IDLEVNR-ALPHA  END-IF                           
573300     IF W-IDLEVNR-ALPHA = 'CFJEA'                                         
573400        MOVE '1757 ' TO W-IDLEVNR-ALPHA  END-IF                           
573500     IF W-IDLEVNR-ALPHA = 'BYL2A'                                         
573600        MOVE '2001 ' TO W-IDLEVNR-ALPHA  END-IF                           
573700     IF W-IDLEVNR-ALPHA = 'CFTPA'                                         
573800        MOVE '2250 ' TO W-IDLEVNR-ALPHA  END-IF                           
573900     IF W-IDLEVNR-ALPHA = 'S3DHC'                                         
574000        MOVE '2429 ' TO W-IDLEVNR-ALPHA  END-IF                           
574100     IF W-IDLEVNR-ALPHA = 'BPUFA'                                         
574200        MOVE '2503 ' TO W-IDLEVNR-ALPHA  END-IF                           
574300     IF W-IDLEVNR-ALPHA = 'BKXQA'                                         
574400        MOVE '2650 ' TO W-IDLEVNR-ALPHA  END-IF                           
574500     IF W-IDLEVNR-ALPHA = 'CFFXA'                                         
574600        MOVE '3143 ' TO W-IDLEVNR-ALPHA  END-IF                           
574700     IF W-IDLEVNR-ALPHA = 'D1V4A'                                         
574800        MOVE '3312 ' TO W-IDLEVNR-ALPHA  END-IF                           
574900     IF W-IDLEVNR-ALPHA = 'L8K5V'                                         
575000        MOVE '3341 ' TO W-IDLEVNR-ALPHA  END-IF                           
575100     IF W-IDLEVNR-ALPHA = 'S552A'                                         
575200        MOVE '6074 ' TO W-IDLEVNR-ALPHA  END-IF                           
575300     IF W-IDLEVNR-ALPHA = 'T226F'                                         
575400        MOVE '6089 ' TO W-IDLEVNR-ALPHA  END-IF                           
575500     IF W-IDLEVNR-ALPHA = 'C8P5A'                                         
575600        MOVE '6670 ' TO W-IDLEVNR-ALPHA  END-IF                           
575700     IF W-IDLEVNR-ALPHA = 'JBA1A'                                         
575800        MOVE '6745 ' TO W-IDLEVNR-ALPHA  END-IF                           
575900     IF W-IDLEVNR-ALPHA = 'BP7YA'                                         
576000        MOVE '7500 ' TO W-IDLEVNR-ALPHA  END-IF                           
576100     IF W-IDLEVNR-ALPHA = 'CY7ZA'                                         
576200        MOVE '7923 ' TO W-IDLEVNR-ALPHA  END-IF                           
576300     IF W-IDLEVNR-ALPHA = 'BJVHA'                                         
576400        MOVE '10057' TO W-IDLEVNR-ALPHA  END-IF                           
576500     IF W-IDLEVNR-ALPHA = 'D36ZA'                                         
576600        MOVE '10947' TO W-IDLEVNR-ALPHA  END-IF                           
576700     IF W-IDLEVNR-ALPHA = 'J3BDA'                                         
576800        MOVE '14922' TO W-IDLEVNR-ALPHA  END-IF                           
576900     IF W-IDLEVNR-ALPHA = 'J6R3A'                                         
577000        MOVE '14990' TO W-IDLEVNR-ALPHA  END-IF                           
577100     IF W-IDLEVNR-ALPHA = 'C0VAH'                                         
577200        MOVE '15284' TO W-IDLEVNR-ALPHA  END-IF                           
577300     IF W-IDLEVNR-ALPHA = 'C8P5C'                                         
577400        MOVE '16037' TO W-IDLEVNR-ALPHA  END-IF                           
577500     IF W-IDLEVNR-ALPHA = 'C8P5D'                                         
577600        MOVE '16179' TO W-IDLEVNR-ALPHA  END-IF                           
577700     IF W-IDLEVNR-ALPHA = 'KTY7B'                                         
577800        MOVE '16336' TO W-IDLEVNR-ALPHA  END-IF                           
577900     IF W-IDLEVNR-ALPHA = 'C8P5J'                                         
578000        MOVE '16372' TO W-IDLEVNR-ALPHA  END-IF                           
578100     IF W-IDLEVNR-ALPHA = 'DR7TA'                                         
578200        MOVE '19206' TO W-IDLEVNR-ALPHA  END-IF                           
578300     IF W-IDLEVNR-ALPHA = 'C9H7A'                                         
578400        MOVE '19611' TO W-IDLEVNR-ALPHA  END-IF                           
578500     IF W-IDLEVNR-ALPHA = 'BXMZA'                                         
578600        MOVE '19914' TO W-IDLEVNR-ALPHA  END-IF                           
578700     IF W-IDLEVNR-ALPHA = 'C72DC'                                         
578800        MOVE '22292' TO W-IDLEVNR-ALPHA  END-IF                           
578900     IF W-IDLEVNR-ALPHA = 'A0VWC'                                         
579000        MOVE '23934' TO W-IDLEVNR-ALPHA  END-IF                           
579100     IF W-IDLEVNR-ALPHA = 'BQ9VB'                                         
579200        MOVE '24081' TO W-IDLEVNR-ALPHA  END-IF                           
579300     IF W-IDLEVNR-ALPHA = 'AJFWA'                                         
579400        MOVE '25072' TO W-IDLEVNR-ALPHA  END-IF                           
579500     IF W-IDLEVNR-ALPHA = 'AJFXA'                                         
579600        MOVE '25073' TO W-IDLEVNR-ALPHA  END-IF                           
579700     IF W-IDLEVNR-ALPHA = 'D2L4A'                                         
579800        MOVE '25082' TO W-IDLEVNR-ALPHA  END-IF                           
579900     IF W-IDLEVNR-ALPHA = 'K261A'                                         
580000        MOVE '25682' TO W-IDLEVNR-ALPHA  END-IF                           
580100     IF W-IDLEVNR-ALPHA = 'A76VE'                                         
580200        MOVE '25817' TO W-IDLEVNR-ALPHA  END-IF                           
580300     IF W-IDLEVNR-ALPHA = 'D36ZC'                                         
580400        MOVE '25826' TO W-IDLEVNR-ALPHA  END-IF                           
580500     IF W-IDLEVNR-ALPHA = 'DSDLA'                                         
580600        MOVE '25871' TO W-IDLEVNR-ALPHA  END-IF                           
580700     IF W-IDLEVNR-ALPHA = 'Q9KNA'                                         
580800        MOVE '25967' TO W-IDLEVNR-ALPHA  END-IF                           
580900     IF W-IDLEVNR-ALPHA = 'L2HWG'                                         
581000        MOVE '63609' TO W-IDLEVNR-ALPHA  END-IF                           
581100*************************************************                         
581200*************************************************                         
581300*************************************************                         
581400     IF W-IDLEVNR-ALPHA = 'BPGTB'                                         
581500        MOVE '211  ' TO W-IDLEVNR-ALPHA  END-IF                           
581600     IF W-IDLEVNR-ALPHA = 'BPGTA'                                         
581700        MOVE '1826 ' TO W-IDLEVNR-ALPHA  END-IF                           
581800     IF W-IDLEVNR-ALPHA = 'AD1YA'                                         
581900        MOVE '3469 ' TO W-IDLEVNR-ALPHA  END-IF                           
582000     IF W-IDLEVNR-ALPHA = 'L8K5U'                                         
582100        MOVE '3471 ' TO W-IDLEVNR-ALPHA  END-IF                           
582200     IF W-IDLEVNR-ALPHA = 'G8UTB'                                         
582300        MOVE '3494 ' TO W-IDLEVNR-ALPHA  END-IF                           
582400     IF W-IDLEVNR-ALPHA = 'MCSQA'                                         
582500        MOVE '3767 ' TO W-IDLEVNR-ALPHA  END-IF                           
582600     IF W-IDLEVNR-ALPHA = 'LNAPA'                                         
582700        MOVE '4540 ' TO W-IDLEVNR-ALPHA  END-IF                           
582800     IF W-IDLEVNR-ALPHA = 'LJVZA'                                         
582900        MOVE '4730 ' TO W-IDLEVNR-ALPHA  END-IF                           
583000     IF W-IDLEVNR-ALPHA = 'DL5LA'                                         
583100        MOVE '4762 ' TO W-IDLEVNR-ALPHA  END-IF                           
583200     IF W-IDLEVNR-ALPHA = 'DDE2A'                                         
583300        MOVE '4790 ' TO W-IDLEVNR-ALPHA  END-IF                           
583400     IF W-IDLEVNR-ALPHA = 'F636B'                                         
583500        MOVE '5154 ' TO W-IDLEVNR-ALPHA  END-IF                           
583600     IF W-IDLEVNR-ALPHA = 'Q33YA'                                         
583700        MOVE '5220 ' TO W-IDLEVNR-ALPHA  END-IF                           
583800     IF W-IDLEVNR-ALPHA = 'DGDKA'                                         
583900        MOVE '5239 ' TO W-IDLEVNR-ALPHA  END-IF                           
584000     IF W-IDLEVNR-ALPHA = 'B517X'                                         
584100        MOVE '5419 ' TO W-IDLEVNR-ALPHA  END-IF                           
584200     IF W-IDLEVNR-ALPHA = 'BRGHA'                                         
584300        MOVE '5614 ' TO W-IDLEVNR-ALPHA  END-IF                           
584400     IF W-IDLEVNR-ALPHA = 'C89GA'                                         
584500        MOVE '5664 ' TO W-IDLEVNR-ALPHA  END-IF                           
584600     IF W-IDLEVNR-ALPHA = 'CKBZA'                                         
584700        MOVE '6133 ' TO W-IDLEVNR-ALPHA  END-IF                           
584800     IF W-IDLEVNR-ALPHA = 'DL6LA'                                         
584900        MOVE '6292 ' TO W-IDLEVNR-ALPHA  END-IF                           
585000     IF W-IDLEVNR-ALPHA = 'D17MA'                                         
585100        MOVE '6363 ' TO W-IDLEVNR-ALPHA  END-IF                           
585200     IF W-IDLEVNR-ALPHA = 'E355B'                                         
585300        MOVE '6369 ' TO W-IDLEVNR-ALPHA  END-IF                           
585400     IF W-IDLEVNR-ALPHA = 'R1N9A'                                         
585500        MOVE '6552 ' TO W-IDLEVNR-ALPHA  END-IF                           
585600     IF W-IDLEVNR-ALPHA = 'M738A'                                         
585700        MOVE '6599 ' TO W-IDLEVNR-ALPHA  END-IF                           
585800     IF W-IDLEVNR-ALPHA = 'DLLMA'                                         
585900        MOVE '6677 ' TO W-IDLEVNR-ALPHA  END-IF                           
586000     IF W-IDLEVNR-ALPHA = 'B45GA'                                         
586100        MOVE '6748 ' TO W-IDLEVNR-ALPHA  END-IF                           
586200     IF W-IDLEVNR-ALPHA = 'D0FVB'                                         
586300        MOVE '7752 ' TO W-IDLEVNR-ALPHA  END-IF                           
586400     IF W-IDLEVNR-ALPHA = 'D0HHD'                                         
586500        MOVE '7800 ' TO W-IDLEVNR-ALPHA  END-IF                           
586600     IF W-IDLEVNR-ALPHA = 'BPTQB'                                         
586700        MOVE '10157' TO W-IDLEVNR-ALPHA  END-IF                           
586800     IF W-IDLEVNR-ALPHA = 'BPTQA'                                         
586900        MOVE '10158' TO W-IDLEVNR-ALPHA  END-IF                           
587000     IF W-IDLEVNR-ALPHA = 'DL7EA'                                         
587100        MOVE '10356' TO W-IDLEVNR-ALPHA  END-IF                           
587200     IF W-IDLEVNR-ALPHA = 'CT3KA'                                         
587300        MOVE '10934' TO W-IDLEVNR-ALPHA  END-IF                           
587400     IF W-IDLEVNR-ALPHA = 'DL7KA'                                         
587500        MOVE '13349' TO W-IDLEVNR-ALPHA  END-IF                           
587600     IF W-IDLEVNR-ALPHA = 'DL7LA'                                         
587700        MOVE '13388' TO W-IDLEVNR-ALPHA  END-IF                           
587800     IF W-IDLEVNR-ALPHA = 'DLMNA'                                         
587900        MOVE '13396' TO W-IDLEVNR-ALPHA  END-IF                           
588000     IF W-IDLEVNR-ALPHA = 'DBQZA'                                         
588100        MOVE '13571' TO W-IDLEVNR-ALPHA  END-IF                           
588200     IF W-IDLEVNR-ALPHA = 'CRKZA'                                         
588300        MOVE '13572' TO W-IDLEVNR-ALPHA  END-IF                           
588400     IF W-IDLEVNR-ALPHA = 'DL7WA'                                         
588500        MOVE '13583' TO W-IDLEVNR-ALPHA  END-IF                           
588600     IF W-IDLEVNR-ALPHA = 'BQAEB'                                         
588700        MOVE '13717' TO W-IDLEVNR-ALPHA  END-IF                           
588800     IF W-IDLEVNR-ALPHA = 'Q5EGA'                                         
588900        MOVE '14947' TO W-IDLEVNR-ALPHA  END-IF                           
589000     IF W-IDLEVNR-ALPHA = 'Q89FA'                                         
589100        MOVE '16030' TO W-IDLEVNR-ALPHA  END-IF                           
589200     IF W-IDLEVNR-ALPHA = 'AQ2MB'                                         
589300        MOVE '16112' TO W-IDLEVNR-ALPHA  END-IF                           
589400     IF W-IDLEVNR-ALPHA = 'CNT5A'                                         
589500        MOVE '20522' TO W-IDLEVNR-ALPHA  END-IF                           
589600     IF W-IDLEVNR-ALPHA = 'BMZJA'                                         
589700        MOVE '24489' TO W-IDLEVNR-ALPHA  END-IF                           
589800     IF W-IDLEVNR-ALPHA = 'ATC2A'                                         
589900        MOVE '25089' TO W-IDLEVNR-ALPHA  END-IF                           
590000     IF W-IDLEVNR-ALPHA = 'MAXWA'                                         
590100        MOVE '25090' TO W-IDLEVNR-ALPHA  END-IF                           
590200     IF W-IDLEVNR-ALPHA = 'BL2CA'                                         
590300        MOVE '25412' TO W-IDLEVNR-ALPHA  END-IF                           
590400     IF W-IDLEVNR-ALPHA = 'M799G'                                         
590500        MOVE '25446' TO W-IDLEVNR-ALPHA  END-IF                           
590600     IF W-IDLEVNR-ALPHA = 'N2KUA'                                         
590700        MOVE '25828' TO W-IDLEVNR-ALPHA  END-IF                           
590800     IF W-IDLEVNR-ALPHA = 'D38ME'                                         
590900        MOVE '25878' TO W-IDLEVNR-ALPHA  END-IF                           
591000     IF W-IDLEVNR-ALPHA = 'D33BA'                                         
591100        MOVE '25910' TO W-IDLEVNR-ALPHA  END-IF                           
591200     IF W-IDLEVNR-ALPHA = 'BPTQD'                                         
591300        MOVE '25924' TO W-IDLEVNR-ALPHA  END-IF                           
591400     IF W-IDLEVNR-ALPHA = 'DPEWA'                                         
591500        MOVE '25962' TO W-IDLEVNR-ALPHA  END-IF                           
591600     IF W-IDLEVNR-ALPHA = 'DP5RA'                                         
591700        MOVE '25973' TO W-IDLEVNR-ALPHA  END-IF                           
591800     IF W-IDLEVNR-ALPHA = 'DSBYA'                                         
591900        MOVE '26009' TO W-IDLEVNR-ALPHA  END-IF                           
592000     IF W-IDLEVNR-ALPHA = 'CRX1D'                                         
592100        MOVE '26012' TO W-IDLEVNR-ALPHA  END-IF                           
592200     IF W-IDLEVNR-ALPHA = 'BKDDB'                                         
592300        MOVE '31014' TO W-IDLEVNR-ALPHA  END-IF                           
592400     IF W-IDLEVNR-ALPHA = 'BKDDC'                                         
592500        MOVE '41014' TO W-IDLEVNR-ALPHA  END-IF                           
592600*************************************************                         
592700**********************************************                            
592800     PERFORM S21-IDLEVNR-ALPHA-TO-NUM                                     
592900     MOVE W-IDLEVNR-NUM       TO UTVCOM1-SLAG-IDLEVNR-NUM                 
593000*                                                                         
593100     PERFORM S14-SKRIV-W33540V-401                                        
593200        .                                                                 
593300        EJECT                                                             
593400 BB-FLYTTA-POST-402-TILL-VCOM SECTION.                                    
593500     SKIP2                                                                
593600     MOVE IN2-IDPTYP          TO UTVCOM2-IDPTYP                           
593700     MOVE IN2-IDVTYP          TO UTVCOM2-IDVTYP                           
593800     MOVE IN2-TEORSAK         TO UTVCOM2-TEORSAK                          
593900     MOVE IN2-TEARTNOT-3      TO UTVCOM2-TEARTNOT-3                       
594000     MOVE IN2-TEARTNOT-7      TO UTVCOM2-TEARTNOT-7                       
594100     PERFORM S15-SKRIV-W33540V-402                                        
594200     .                                                                    
594300     EJECT                                                                
594400 BC-FLYTTA-POST-403-TILL-VCOM SECTION.                                    
594500     SKIP2                                                                
594600     MOVE IN3-IDPTYP          TO UTVCOM3-IDPTYP                           
594700     MOVE IN3-IDVTYP          TO UTVCOM3-IDVTYP                           
594800     MOVE IN3-BEART(1)        TO UTVCOM3-BEART(1)                         
594900     MOVE IN3-BEART(2)        TO UTVCOM3-BEART(2)                         
595000     PERFORM S16-SKRIV-W33540V-403                                        
595100     .                                                                    
595200     EJECT                                                                
595300 C-FLYTTA-INPOST-TILL-UTFIL SECTION.                                      
595400                                                                          
595500     EVALUATE IN1-IDPTYP                                                  
595600       WHEN '401'                                                         
595700         PERFORM CA-FLYTTA-VERS-401-TILL-UTFIL                            
595800       WHEN '402'                                                         
595900         PERFORM CB-FLYTTA-VERS-402-TILL-UTFIL                            
596000       WHEN '403'                                                         
596100         PERFORM CC-FLYTTA-VERS-403-TILL-UTFIL                            
596200       WHEN OTHER                                                         
596300         STRING 'FEL COPYTEXTVERSION : '                                  
596400                IN1-IDPTYP                                                
596500           DELIMITED BY SIZE INTO FELTEXT-STR                             
596600         DISPLAY FELTEXT                                                  
596700         PERFORM S99-ABEND                                                
596800     END-EVALUATE                                                         
596900     .                                                                    
597000     EJECT                                                                
597100                                                                          
597200 CA-FLYTTA-VERS-401-TILL-UTFIL SECTION.                                   
597300     SKIP2                                                                
597400     MOVE IN1-IDPTYP          TO UT1-IDPTYP                               
597500     MOVE IN1-IDVTYP          TO UT1-IDVTYP                               
597600     MOVE IN1-IDARTNR         TO UT1-IDARTNR                              
597700     MOVE IN1-IDFKNGRP        TO UT1-IDFKNGRP                             
597800     MOVE IN1-KDSRA           TO UT1-KDSRA                                
597900     MOVE IN1-KVQPACK-0       TO UT1-KVQPACK-0                            
598000     MOVE IN1-KDARTURS-NUM    TO UT1-KDARTURS-NUM                         
598100     MOVE IN1-KDPRODSL        TO UT1-KDPRODSL                             
598200     MOVE IN1-VLARTNTO        TO UT1-VLARTNTO                             
598300     MOVE IN1-VKART           TO UT1-VKART                                
598400     MOVE IN1-KDVSOP          TO UT1-KDVSOP                               
598500     MOVE IN1-IDSTATNR        TO UT1-IDSTATNR                             
598600     MOVE IN1-KDSORT          TO UT1-KDSORT                               
598700     MOVE IN1-KDERS           TO UT1-KDERS                                
598800     MOVE IN1-KDBPSR          TO UT1-KDBPSR                               
598900     MOVE IN1-KDBBCL          TO UT1-KDBBCL                               
599000     MOVE IN1-IDLEVNR         TO UT1-IDLEVNR                              
599100     MOVE IN1-PRARTSJK        TO UT1-PRARTSJK                             
599200     MOVE IN1-PRARTSTD        TO UT1-PRARTSTD                             
599300     MOVE IN1-FLIART          TO UT1-FLIART                               
599400     MOVE IN1-IDPROJ          TO UT1-IDPROJ                               
599500     MOVE IN1-IDAO(1)         TO UT1-IDAO(1)                              
599600     MOVE IN1-IDAO(2)         TO UT1-IDAO(2)                              
599700     MOVE IN1-TIFINLEV        TO UT1-TIFINLEV                             
599800     MOVE IN1-IDANSK          TO UT1-IDANSK                               
599900     MOVE IN1-KVPB            TO UT1-KVPB                                 
600000     MOVE IN1-KDVVKL          TO UT1-KDVVKL                               
600100     MOVE IN1-BELEVART        TO UT1-BELEVART                             
600200     MOVE IN1-KDTIPPR         TO UT1-KDTIPPR                              
600300     MOVE IN1-IDINK           TO UT1-IDINK                                
600400     MOVE IN1-TIREGDAT        TO UT1-TIREGDAT                             
600500     MOVE IN1-KDUART          TO UT1-KDUART                               
600600     MOVE IN1-IDARTNR-MOTSV   TO UT1-IDARTNR-MOTSV                        
600700     MOVE IN1-PRINK           TO UT1-PRINK                                
600800     MOVE IN1-IDRITN          TO UT1-IDRITN                               
600900     MOVE IN1-PRHANTK         TO UT1-PRHANTK                              
601000     MOVE IN1-KDAGE           TO UT1-KDAGE                                
601100     MOVE IN1-SLAG-IDLEVNR    TO UT1-SLAG-IDLEVNR                         
601200     MOVE IN1-KDPSLLOC        TO UT1-KDPSLLOC                             
601300     MOVE IN1-IDKAT(1)        TO UT1-IDKAT(1)                             
601400     MOVE IN1-IDKAT(2)        TO UT1-IDKAT(2)                             
601500     MOVE IN1-IDKAT(3)        TO UT1-IDKAT(3)                             
601600     MOVE IN1-KDRAB           TO UT1-KDRAB                                
601700     MOVE IN1-PRARTBEL        TO UT1-PRARTBEL                             
601800     MOVE IN1-FLLSRDEL        TO UT1-FLLSRDEL                             
601900     MOVE IN1-IDPROJUP        TO UT1-IDPROJUP                             
602000     MOVE IN1-FLGEMFMC        TO UT1-FLGEMFMC                             
602010     MOVE IN1-TIURPROD        TO UT1-TIURPROD                             
602100     PERFORM S11-SKRIV-W33540U-401                                        
602200     .                                                                    
602300     EJECT                                                                
602400 CB-FLYTTA-VERS-402-TILL-UTFIL SECTION.                                   
602500     SKIP2                                                                
602600     MOVE IN2-IDPTYP          TO UT2-IDPTYP                               
602700     MOVE IN2-IDVTYP          TO UT2-IDVTYP                               
602800     MOVE IN2-TEORSAK         TO UT2-TEORSAK                              
602900     MOVE IN2-TEARTNOT-3      TO UT2-TEARTNOT-3                           
602910     MOVE IN2-TEARTNOT-7      TO UT2-TEARTNOT-7                           
603000     PERFORM S12-SKRIV-W33540U-402                                        
603100     .                                                                    
603200     EJECT                                                                
603300                                                                          
603400 CC-FLYTTA-VERS-403-TILL-UTFIL SECTION.                                   
603500     SKIP2                                                                
603600     MOVE IN3-IDPTYP          TO UT3-IDPTYP                               
603700     MOVE IN3-IDVTYP          TO UT3-IDVTYP                               
603800     MOVE IN3-BEART(1)        TO UT3-BEART(1)                             
603900     MOVE IN3-BEART(2)        TO UT3-BEART(2)                             
604000     PERFORM S13-SKRIV-W33540U-403                                        
604100     .                                                                    
604200     EJECT                                                                
604300 Z-FINIT SECTION.                                                         
604400                                                                          
604500     CLOSE                                                                
604600           W33540I                                                        
604700           W33540U                                                        
604800           W33540V                                                        
604900           W33541                                                         
605000                                                                          
605100     MOVE 'S' TO POSTSUM-OPKOD                                            
605200     CALL POSTSUM USING POSTSUM-PARM                                      
605300     .                                                                    
605400     EJECT                                                                
605500 S02-LAES-W33540I SECTION.                                                
605600                                                                          
605700     READ W33540I INTO IN-AREA                                            
605800     AT END                                                               
605900        SET END-OF-W33540I TO TRUE                                        
606000                                                                          
606100     NOT AT END                                                           
606200        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
606300        MOVE 'W33540'   TO POSTSUM-FDNAMN                                 
606400        MOVE 'W33541D2' TO POSTSUM-DDNAMN2                                
606500        CALL POSTSUM USING POSTSUM-PARM                                   
606600     END-READ                                                             
606700     .                                                                    
606800     EJECT                                                                
606900 S03-LAES-W33541  SECTION.                                                
607000                                                                          
607100     READ W33541  INTO ANTAL-AREA                                         
607200     AT END                                                               
607300        SET END-OF-W33541  TO TRUE                                        
607400                                                                          
607500     NOT AT END                                                           
607600        MOVE 'ANT'      TO POSTSUM-TRANSTYP                               
607700        MOVE 'W33541'   TO POSTSUM-FDNAMN                                 
607800        MOVE 'W33541D5' TO POSTSUM-DDNAMN2                                
607900        CALL POSTSUM USING POSTSUM-PARM                                   
608000     END-READ                                                             
608100     .                                                                    
608200     EJECT                                                                
608300 S10-SKRIV-W33540V-000 SECTION.                                           
608400                                                                          
608500     MOVE ANTAL-IDPTYP TO UTVCOM0-IDPTYP                                  
608600     MOVE ANTAL-IDVTYP TO UTVCOM0-IDVTYP                                  
608700     MOVE ANTAL-KVPOST TO UTVCOM0-KVPOST                                  
608800     WRITE UTVCOM0-POST FROM UTVCOM-AREA                                  
608900                                                                          
609000     MOVE 'PT0 '     TO POSTSUM-TRANSTYP                                  
609100     MOVE 'W33540'   TO POSTSUM-FDNAMN                                    
609200     MOVE 'W33541D4' TO POSTSUM-DDNAMN2                                   
609300     CALL POSTSUM USING POSTSUM-PARM                                      
609400     .                                                                    
609500     EJECT                                                                
609600 S11-SKRIV-W33540U-401 SECTION.                                           
609700                                                                          
609800     WRITE UT1-POST FROM UT-AREA                                          
609900                                                                          
610000     MOVE 'UT1'      TO POSTSUM-TRANSTYP                                  
610100     MOVE 'W33540'   TO POSTSUM-FDNAMN                                    
610200     MOVE 'W33541D3' TO POSTSUM-DDNAMN2                                   
610300     CALL POSTSUM USING POSTSUM-PARM                                      
610400     .                                                                    
610500     EJECT                                                                
610600 S12-SKRIV-W33540U-402 SECTION.                                           
610700                                                                          
610800     WRITE UT2-POST FROM UT-AREA                                          
610900                                                                          
611000     MOVE 'UT2'      TO POSTSUM-TRANSTYP                                  
611100     MOVE 'W33540'   TO POSTSUM-FDNAMN                                    
611200     MOVE 'W33541D3' TO POSTSUM-DDNAMN2                                   
611300     CALL POSTSUM USING POSTSUM-PARM                                      
611400     .                                                                    
611500     EJECT                                                                
611600 S13-SKRIV-W33540U-403 SECTION.                                           
611700                                                                          
611800     WRITE UT3-POST FROM UT-AREA                                          
611900                                                                          
612000     MOVE 'UT3'      TO POSTSUM-TRANSTYP                                  
612100     MOVE 'W33540'   TO POSTSUM-FDNAMN                                    
612200     MOVE 'W33541D3' TO POSTSUM-DDNAMN2                                   
612300     CALL POSTSUM USING POSTSUM-PARM                                      
612400     .                                                                    
612500     EJECT                                                                
612600 S14-SKRIV-W33540V-401 SECTION.                                           
612700                                                                          
612800     WRITE UTVCOM1-POST FROM UTVCOM-AREA                                  
612900                                                                          
613000     MOVE 'PT1'      TO POSTSUM-TRANSTYP                                  
613100     MOVE 'W33540'   TO POSTSUM-FDNAMN                                    
613200     MOVE 'W33541D4' TO POSTSUM-DDNAMN2                                   
613300     CALL POSTSUM USING POSTSUM-PARM                                      
613400     .                                                                    
613500     EJECT                                                                
613600 S15-SKRIV-W33540V-402 SECTION.                                           
613700                                                                          
613800     WRITE UTVCOM2-POST FROM UTVCOM-AREA                                  
613900                                                                          
614000     MOVE 'PT2'      TO POSTSUM-TRANSTYP                                  
614100     MOVE 'W33540'   TO POSTSUM-FDNAMN                                    
614200     MOVE 'W33541D4' TO POSTSUM-DDNAMN2                                   
614300     CALL POSTSUM USING POSTSUM-PARM                                      
614400     .                                                                    
614500     EJECT                                                                
614600 S16-SKRIV-W33540V-403 SECTION.                                           
614700                                                                          
614800     WRITE UTVCOM3-POST FROM UTVCOM-AREA                                  
614900                                                                          
615000     MOVE 'PT3'      TO POSTSUM-TRANSTYP                                  
615100     MOVE 'W33540'   TO POSTSUM-FDNAMN                                    
615200     MOVE 'W33541D4' TO POSTSUM-DDNAMN2                                   
615300     CALL POSTSUM USING POSTSUM-PARM                                      
615400     .                                                                    
615500     EJECT                                                                
615600 S21-IDLEVNR-ALPHA-TO-NUM SECTION.                                        
615700* I/P : W-IDLEVNR-ALPHA     O/P : W-IDLEVNR-NUM                           
615800                                                                          
615900     MOVE ZERO                TO W-TALLY                                  
616000     INSPECT W-IDLEVNR-ALPHA TALLYING W-TALLY                             
616100             FOR CHARACTERS BEFORE INITIAL SPACE                          
616200     IF W-TALLY = ZERO                                                    
616300        MOVE ZERO             TO W-IDLEVNR-NUM                            
616400     ELSE                                                                 
616500        MOVE W-IDLEVNR-ALPHA(1:W-TALLY)                                   
616600                              TO W-IDLEVNR-NUM                            
616700     END-IF                                                               
616800     .                                                                    
616900     EJECT                                                                
617000                                                                          
617100 S99-ABEND SECTION.                                                       
617200                                                                          
617300     MOVE 'S' TO POSTSUM-OPKOD                                            
617400     CALL POSTSUM USING POSTSUM-PARM                                      
617500     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
617600     .                                                                    
