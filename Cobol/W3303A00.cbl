000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3303A00.                                                
000400*AUTHOR.         RONNY STENHOLM                                           
000500*DATE-WRITTEN.   MAJ   1994.                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*          PROGRAMMET LÄSER FIL INEHÅLLANDE STATISTIK TILL                
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
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- FIL MED ART.STAT TILL MB                                   
003300*          --- POSTER ATT SÄNDA TILL MB.                                  
003400     SELECT W3303AI                    ASSIGN TO W3303AD2.                
003500     SKIP2                                                                
003600*          --- FIL MED ART.STAT TILL MB                                   
003700*          --- POSTER KVAR ATT SÄNDA EFTER DENNA SÄNDNING.                
003800     SELECT W3303AU                    ASSIGN TO W3303AD3.                
003900     SKIP2                                                                
004000*          --- FIL MED ART.STAT TILL MB                                   
004100*          --- 'DEL-FIL' ATT SÄNDA VIA VCOM                               
004200     SELECT W3303AV                    ASSIGN TO W3303AD4.                
004300     SKIP2                                                                
004400*          --- FIL INNEHÅLLANDE EN POST.                                  
004500*          --- DENNA POST INNEHÅLLER DET TOTALA ANTALET POSTER.           
004600     SELECT W3304A                     ASSIGN TO W3303AD5.                
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900     SKIP3                                                                
005000 FILE SECTION.                                                            
005100     SKIP3                                                                
005200 FD  W3303AI                                                              
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500     SKIP2                                                                
005600*01  -COPY W33038      -L.                                                
005700     SKIP3                                                                
005800 FD  W3303AU                                                              
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100     SKIP2                                                                
006200*01  POST -COPY W33038 -PRE  UT1-  -L.                                    
006300     SKIP3                                                                
006400 FD  W3303AV                                                              
006500     RECORDING       V                                                    
006600     BLOCK CONTAINS  0.                                                   
006700     SKIP2                                                                
006800*01  POST -COPY W330381A -PRE  UTVCOM1-  -L.                              
006900*01  POST -COPY W330380A -PRE  UTVCOM0-  -L.                              
007000     EJECT                                                                
007100 FD  W3304A                                                               
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS  0.                                                   
007400     SKIP2                                                                
007500*01  POST -COPY W330380A -PRE  ANTAL-  -L.                                
007600     EJECT                                                                
007700 WORKING-STORAGE SECTION.                                                 
007800     SKIP2                                                                
007801                                                                          
007810*    -- CHECKED BY WY2000                                                 
007900 77  IDPGM                       PIC X(8)    VALUE 'W3303A00'.            
008000 77  JA                          PIC X       VALUE 'J'.                   
008100 77  NEJ                         PIC X       VALUE 'N'.                   
008200 77  WS-POSTRAKNARE              PIC S9(7)    VALUE ZERO.                 
008300 77  MAX-POSTER                  PIC S9(7)    VALUE +270000.              
008400 77  RETURKOD                    PIC S9(2)   COMP-3 VALUE ZERO.           
008500                                                                          
008600                                                                          
008700 77  W3303AI-EOF-SW              PIC X       VALUE 'N'.                   
008800     88  END-OF-W3303AI                      VALUE 'J'.                   
008900     EJECT                                                                
009000 77  W3304A-EOF-SW              PIC X       VALUE 'N'.                    
009100     88  END-OF-W3304A                      VALUE 'J'.                    
009200     EJECT                                                                
009300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009400 01  FILLER REDEFINES DAGENS-DATUM.                                       
009500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009800     EJECT                                                                
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000*                                                                         
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010300     SKIP2                                                                
010400*    --- PARAMETRAR TILL ABEND                                            
010500                                                                          
010600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010800     SKIP2                                                                
010900 01  FELTEXT.                                                             
011000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL POSTSUM                                          
011400*                                                                         
011500*01  -COPY W0005   -PRE  POSTSUM-                                         
011600     EJECT                                                                
011700 01  IN-AREA-START               PIC X(24)   VALUE                        
011800                                 'IN-AREA-START  '.                       
011900     SKIP2                                                                
012000 01  IN-AREA.                                                             
012100     03  FILLER                  PIC X(50).                               
012200*01  FILLER -COPY W33038        -PRE IN1-   -RED  IN-AREA                 
012300     EJECT                                                                
012400 01  ANTAL-AREA-START               PIC X(24)   VALUE                     
012500                                 'ANTAL-AREA-START  '.                    
012600     SKIP2                                                                
012700 01  ANTAL-AREA.                                                          
012800     03  FILLER                  PIC X(20).                               
012900*01  FILLER -COPY W330380A      -PRE ANTAL-   -RED  ANTAL-AREA            
013000     EJECT                                                                
013100 01  UT-AREA-START               PIC X(24)   VALUE                        
013200                                 'UT-AREA-START  '.                       
013300     SKIP2                                                                
013400 01  UT-AREA.                                                             
013500     03  FILLER                  PIC X(50).                               
013600*01  FILLER -COPY W33038        -PRE UT1-   -RED  UT-AREA                 
013700     EJECT                                                                
013800 01  UTVCOM-AREA-START           PIC X(24)   VALUE                        
013900                                 'UTVCOM-AREA-START  '.                   
014000     SKIP2                                                                
014100 01  UTVCOM-AREA.                                                         
014200     03  FILLER                  PIC X(50).                               
014300*01  FILLER -COPY W330380A      -PRE UTVCOM0-   -RED  UTVCOM-AREA         
014400*01  FILLER -COPY W330381A      -PRE UTVCOM1-   -RED  UTVCOM-AREA         
014500     EJECT                                                                
014600 PROCEDURE DIVISION.                                                      
014700                                                                          
014800     PERFORM A-INIT                                                       
014900     PERFORM S02-LAES-W3303AI                                             
015000     PERFORM S03-LAES-W3304A                                              
015100     PERFORM S10-SKRIV-W3303AV-000                                        
015200     MOVE 1 TO WS-POSTRAKNARE                                             
015300                                                                          
015400     PERFORM UNTIL WS-POSTRAKNARE > MAX-POSTER OR                         
015500                   END-OF-W3303AI                                         
015600                                                                          
015700       PERFORM B-FLYTTA-INPOST-TILL-VCOMFIL                               
015800                                                                          
015900       PERFORM S02-LAES-W3303AI                                           
016000       ADD 1 TO WS-POSTRAKNARE                                            
016100     END-PERFORM                                                          
016200                                                                          
016300     IF END-OF-W3303AI                                                    
016400       MOVE ZERO TO RETURKOD                                              
016500     ELSE                                                                 
016600       MOVE +8   TO RETURKOD                                              
016700                                                                          
016800       PERFORM UNTIL END-OF-W3303AI                                       
016900                                                                          
017000         PERFORM C-FLYTTA-INPOST-TILL-UTFIL                               
017100                                                                          
017200         PERFORM S02-LAES-W3303AI                                         
017300       END-PERFORM                                                        
017400     END-IF                                                               
017500                                                                          
017600     PERFORM Z-FINIT                                                      
017700                                                                          
017800     MOVE RETURKOD TO RETURN-CODE                                         
017900     GOBACK                                                               
018000     .                                                                    
018100     EJECT                                                                
018200 A-INIT SECTION.                                                          
018300                                                                          
018400     OPEN INPUT                                                           
018500                 W3303AI                                                  
018600                 W3304A                                                   
018700                                                                          
018800     OPEN OUTPUT W3303AU                                                  
018900                 W3303AV                                                  
019000                                                                          
019100     ACCEPT DAGENS-DATUM FROM DATE                                        
019200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019300     .                                                                    
019400     EJECT                                                                
019500 B-FLYTTA-INPOST-TILL-VCOMFIL SECTION.                                    
019600                                                                          
019700                                                                          
019800     MOVE 'A'                  TO UTVCOM1-IDVTYP                          
019900     MOVE '301'                TO UTVCOM1-IDPTYP                          
020000     MOVE IN1-IDARTNR          TO UTVCOM1-IDARTNR                         
020100     MOVE IN1-IDPROMR          TO UTVCOM1-IDPROMR                         
020200     MOVE IN1-IDMARKBO         TO UTVCOM1-IDMARKBO                        
020300     MOVE IN1-IDPROMRN         TO UTVCOM1-IDPROMRN                        
020400     MOVE IN1-SUARTFSG-DO-RAAR TO UTVCOM1-SUARTFSG-DO-RAAR                
020500     MOVE IN1-SULEVANT-DO-RAAR TO UTVCOM1-SULEVANT-DO-RAAR                
020600     MOVE IN1-SUARTFSG-RAAR    TO UTVCOM1-SUARTFSG-RAAR                   
020700     MOVE IN1-SULEVANT-RAAR    TO UTVCOM1-SULEVANT-RAAR                   
020800*       *                                                                 
020900     PERFORM S14-SKRIV-W3303AV                                            
021000        .                                                                 
021100        EJECT                                                             
021200 C-FLYTTA-INPOST-TILL-UTFIL SECTION.                                      
021300     SKIP2                                                                
021400     MOVE IN1-BEST-IDMARKBO    TO UT1-BEST-IDMARKBO                       
021500     MOVE IN1-IDARTNR          TO UT1-IDARTNR                             
021600     MOVE IN1-IDPROMR          TO UT1-IDPROMR                             
021700     MOVE IN1-IDMARKBO         TO UT1-IDMARKBO                            
021800     MOVE IN1-IDPROMRN         TO UT1-IDPROMRN                            
021900     MOVE IN1-SUARTFSG-DO-RAAR TO UT1-SUARTFSG-DO-RAAR                    
022000     MOVE IN1-SULEVANT-DO-RAAR TO UT1-SULEVANT-DO-RAAR                    
022100     MOVE IN1-SUARTFSG-RAAR    TO UT1-SUARTFSG-RAAR                       
022200     MOVE IN1-SULEVANT-RAAR    TO UT1-SULEVANT-RAAR                       
022300     PERFORM S11-SKRIV-W3303AU                                            
022400     .                                                                    
022500     EJECT                                                                
022600 Z-FINIT SECTION.                                                         
022700                                                                          
022800     CLOSE                                                                
022900           W3303AI                                                        
023000           W3303AU                                                        
023100           W3303AV                                                        
023200           W3304A                                                         
023300                                                                          
023400     MOVE 'S' TO POSTSUM-OPKOD                                            
023500     CALL POSTSUM USING POSTSUM-PARM                                      
023600     .                                                                    
023700     EJECT                                                                
023800 S02-LAES-W3303AI SECTION.                                                
023900                                                                          
024000     READ W3303AI INTO IN-AREA                                            
024100     AT END                                                               
024200        SET END-OF-W3303AI TO TRUE                                        
024300                                                                          
024400     NOT AT END                                                           
024500        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
024600        MOVE 'W33040'   TO POSTSUM-FDNAMN                                 
024700        MOVE 'W3303AD2' TO POSTSUM-DDNAMN2                                
024800        CALL POSTSUM USING POSTSUM-PARM                                   
024900     END-READ                                                             
025000     .                                                                    
025100     EJECT                                                                
025200 S03-LAES-W3304A  SECTION.                                                
025300*FIL MED EN POST SOM VISAR ANTALET POSTER I W3303A-FILEN FÖR VCOM.        
025400     READ W3304A  INTO ANTAL-AREA                                         
025500     AT END                                                               
025600        SET END-OF-W3304A  TO TRUE                                        
025700                                                                          
025800     NOT AT END                                                           
025900        MOVE 'ANT'      TO POSTSUM-TRANSTYP                               
026000        MOVE 'W3304A'   TO POSTSUM-FDNAMN                                 
026100        MOVE 'W3303AD5' TO POSTSUM-DDNAMN2                                
026200        CALL POSTSUM USING POSTSUM-PARM                                   
026300     END-READ                                                             
026400     .                                                                    
026500     EJECT                                                                
026600 S10-SKRIV-W3303AV-000 SECTION.                                           
026700                                                                          
026800     MOVE '300'        TO UTVCOM0-IDPTYP                                  
026900     MOVE 'A'          TO UTVCOM0-IDVTYP                                  
027000     MOVE ANTAL-KVPOST TO UTVCOM0-KVPOST                                  
027100     WRITE UTVCOM0-POST FROM UTVCOM-AREA                                  
027200                                                                          
027300     MOVE 'PT0 '     TO POSTSUM-TRANSTYP                                  
027400     MOVE 'W33040'   TO POSTSUM-FDNAMN                                    
027500     MOVE 'W3303AD4' TO POSTSUM-DDNAMN2                                   
027600     CALL POSTSUM USING POSTSUM-PARM                                      
027700     .                                                                    
027800     EJECT                                                                
027900 S11-SKRIV-W3303AU SECTION.                                               
028000                                                                          
028100     WRITE UT1-POST FROM UT-AREA                                          
028200                                                                          
028300     MOVE 'UT1'      TO POSTSUM-TRANSTYP                                  
028400     MOVE 'W3303A'   TO POSTSUM-FDNAMN                                    
028500     MOVE 'W3303AD3' TO POSTSUM-DDNAMN2                                   
028600     CALL POSTSUM USING POSTSUM-PARM                                      
028700     .                                                                    
028800     EJECT                                                                
028900 S14-SKRIV-W3303AV SECTION.                                               
029000                                                                          
029100     WRITE UTVCOM1-POST FROM UTVCOM-AREA                                  
029200                                                                          
029300     MOVE 'PT1'      TO POSTSUM-TRANSTYP                                  
029400     MOVE 'W33040'   TO POSTSUM-FDNAMN                                    
029500     MOVE 'W3303AD4' TO POSTSUM-DDNAMN2                                   
029600     CALL POSTSUM USING POSTSUM-PARM                                      
029700     .                                                                    
029800     EJECT                                                                
029900 S99-ABEND SECTION.                                                       
030000                                                                          
030100     MOVE 'S' TO POSTSUM-OPKOD                                            
030200     CALL POSTSUM USING POSTSUM-PARM                                      
030300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
030400     .                                                                    
