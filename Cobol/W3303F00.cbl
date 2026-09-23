000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3303F00.                                                
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
003400     SELECT W3303FI                    ASSIGN TO W3303FD2.                
003500     SKIP2                                                                
003600*          --- FIL MED ART.STAT TILL MB                                   
003700*          --- POSTER KVAR ATT SÄNDA EFTER DENNA SÄNDNING.                
003800     SELECT W3303FU                    ASSIGN TO W3303FD3.                
003900     SKIP2                                                                
004000*          --- FIL MED ART.STAT TILL MB                                   
004100*          --- 'DEL-FIL' ATT SÄNDA VIA VCOM                               
004200     SELECT W3303FV                    ASSIGN TO W3303FD4.                
004300     SKIP2                                                                
004400*          --- FIL INNEHÅLLANDE EN POST.                                  
004500*          --- DENNA POST INNEHÅLLER DET TOTALA ANTALET TRANSAR.          
004600     SELECT W3304F                     ASSIGN TO W3303FD5.                
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900     SKIP3                                                                
005000 FILE SECTION.                                                            
005100     SKIP3                                                                
005200 FD  W3303FI                                                              
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500     SKIP2                                                                
005600*01  -COPY W33038      -L.                                                
005700     SKIP3                                                                
005800 FD  W3303FU                                                              
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100     SKIP2                                                                
006200*01  POST -COPY W33038 -PRE  UT1-  -L.                                    
006300     SKIP3                                                                
006400 FD  W3303FV                                                              
006500     RECORDING       V                                                    
006600     BLOCK CONTAINS  0.                                                   
006700     SKIP2                                                                
006800*01  POST -COPY W330381A -PRE  UTVCOM1-  -L.                              
006900*01  POST -COPY W330380A -PRE  UTVCOM0-  -L.                              
007000     EJECT                                                                
007100 FD  W3304F                                                               
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS  0.                                                   
007400     SKIP2                                                                
007500*01  POST -COPY W330380A -PRE  ANTAL-  -L.                                
007600     EJECT                                                                
007700 WORKING-STORAGE SECTION.                                                 
007800     SKIP2                                                                
007801                                                                          
007810*    -- CHECKED BY WY2000                                                 
007900 77  IDPGM                       PIC X(8)    VALUE 'W3303F00'.            
008000 77  JA                          PIC X       VALUE 'J'.                   
008100 77  NEJ                         PIC X       VALUE 'N'.                   
008200 77  WS-POSTRAKNARE              PIC S9(7)    VALUE ZERO.                 
008300 77  MAX-POSTER                  PIC S9(7)    VALUE +270000.              
008400 77  RETURKOD                    PIC S9(2)   COMP-3 VALUE ZERO.           
008500                                                                          
008600                                                                          
008700 77  W3303FI-EOF-SW              PIC X       VALUE 'N'.                   
008800     88  END-OF-W3303FI                      VALUE 'J'.                   
008900     EJECT                                                                
009000 77  W3304F-EOF-SW              PIC X       VALUE 'N'.                    
009100     88  END-OF-W3304F                      VALUE 'J'.                    
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
014900     PERFORM S02-LAES-W3303FI                                             
015000     PERFORM S03-LAES-W3304F                                              
015100     PERFORM S10-SKRIV-W3303FV-000                                        
015200     MOVE 1 TO WS-POSTRAKNARE                                             
015300                                                                          
015400     PERFORM UNTIL WS-POSTRAKNARE > MAX-POSTER OR                         
015500                   END-OF-W3303FI                                         
015600                                                                          
015700       PERFORM B-FLYTTA-INPOST-TILL-VCOMFIL                               
015800                                                                          
015900       PERFORM S02-LAES-W3303FI                                           
016000       ADD 1 TO WS-POSTRAKNARE                                            
016100     END-PERFORM                                                          
016200                                                                          
016300     IF END-OF-W3303FI                                                    
016400       MOVE ZERO TO RETURKOD                                              
016500     ELSE                                                                 
016600       MOVE +8   TO RETURKOD                                              
016700                                                                          
016800       PERFORM UNTIL END-OF-W3303FI                                       
016900                                                                          
017000         PERFORM C-FLYTTA-INPOST-TILL-UTFIL                               
017100                                                                          
017200         PERFORM S02-LAES-W3303FI                                         
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
018500                 W3303FI                                                  
018600                 W3304F                                                   
018700                                                                          
018800     OPEN OUTPUT W3303FU                                                  
018900                 W3303FV                                                  
019000                                                                          
019100     ACCEPT DAGENS-DATUM FROM DATE                                        
019200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019300     .                                                                    
019400     EJECT                                                                
019500 B-FLYTTA-INPOST-TILL-VCOMFIL SECTION.                                    
019600                                                                          
019700                                                                          
019800     MOVE 'A'                  TO UTVCOM1-IDVTYP                          
019810     MOVE '301'                TO UTVCOM1-IDPTYP                          
019900     MOVE IN1-IDARTNR          TO UTVCOM1-IDARTNR                         
020500     MOVE IN1-IDPROMR          TO UTVCOM1-IDPROMR                         
020700     MOVE IN1-IDMARKBO         TO UTVCOM1-IDMARKBO                        
021600     MOVE IN1-IDPROMRN         TO UTVCOM1-IDPROMRN                        
021800     MOVE IN1-SUARTFSG-DO-RAAR TO UTVCOM1-SUARTFSG-DO-RAAR                
022100     MOVE IN1-SULEVANT-DO-RAAR TO UTVCOM1-SULEVANT-DO-RAAR                
022400     MOVE IN1-SUARTFSG-RAAR    TO UTVCOM1-SUARTFSG-RAAR                   
022700     MOVE IN1-SULEVANT-RAAR    TO UTVCOM1-SULEVANT-RAAR                   
023100*       *                                                                 
023200     PERFORM S14-SKRIV-W3303FV                                            
023300        .                                                                 
023400        EJECT                                                             
023500 C-FLYTTA-INPOST-TILL-UTFIL SECTION.                                      
025500     SKIP2                                                                
025600     MOVE IN1-BEST-IDMARKBO    TO UT1-BEST-IDMARKBO                       
025700     MOVE IN1-IDARTNR          TO UT1-IDARTNR                             
025800     MOVE IN1-IDPROMR          TO UT1-IDPROMR                             
025900     MOVE IN1-IDMARKBO         TO UT1-IDMARKBO                            
026000     MOVE IN1-IDPROMRN         TO UT1-IDPROMRN                            
026100     MOVE IN1-SUARTFSG-DO-RAAR TO UT1-SUARTFSG-DO-RAAR                    
026200     MOVE IN1-SULEVANT-DO-RAAR TO UT1-SULEVANT-DO-RAAR                    
026300     MOVE IN1-SUARTFSG-RAAR    TO UT1-SUARTFSG-RAAR                       
026400     MOVE IN1-SULEVANT-RAAR    TO UT1-SULEVANT-RAAR                       
029300     PERFORM S11-SKRIV-W3303FU                                            
029400     .                                                                    
029500     EJECT                                                                
029600 Z-FINIT SECTION.                                                         
029700                                                                          
029800     CLOSE                                                                
029900           W3303FI                                                        
030000           W3303FU                                                        
030100           W3303FV                                                        
030200           W3304F                                                         
030300                                                                          
030400     MOVE 'S' TO POSTSUM-OPKOD                                            
030500     CALL POSTSUM USING POSTSUM-PARM                                      
030600     .                                                                    
030700     EJECT                                                                
030800 S02-LAES-W3303FI SECTION.                                                
030900                                                                          
031000     READ W3303FI INTO IN-AREA                                            
031100     AT END                                                               
031200        SET END-OF-W3303FI TO TRUE                                        
031300                                                                          
031400     NOT AT END                                                           
031500        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
031600        MOVE 'W33040'   TO POSTSUM-FDNAMN                                 
031700        MOVE 'W3303FD2' TO POSTSUM-DDNAMN2                                
031800        CALL POSTSUM USING POSTSUM-PARM                                   
031900     END-READ                                                             
032000     .                                                                    
032100     EJECT                                                                
032200 S03-LAES-W3304F  SECTION.                                                
032300*FIL MED EN POST SOM VISAR ANTALET POSTER I W3303F-FILEN FÖR VCOM.        
032400     READ W3304F  INTO ANTAL-AREA                                         
032500     AT END                                                               
032600        SET END-OF-W3304F  TO TRUE                                        
032700                                                                          
032800     NOT AT END                                                           
032900        MOVE 'ANT'      TO POSTSUM-TRANSTYP                               
033000        MOVE 'W3304F'   TO POSTSUM-FDNAMN                                 
033100        MOVE 'W3303FD5' TO POSTSUM-DDNAMN2                                
033200        CALL POSTSUM USING POSTSUM-PARM                                   
033300     END-READ                                                             
033400     .                                                                    
033500     EJECT                                                                
033600 S10-SKRIV-W3303FV-000 SECTION.                                           
033700                                                                          
033800     MOVE '300'        TO UTVCOM0-IDPTYP                                  
033900     MOVE 'A'          TO UTVCOM0-IDVTYP                                  
034000     MOVE ANTAL-KVPOST TO UTVCOM0-KVPOST                                  
034100     WRITE UTVCOM0-POST FROM UTVCOM-AREA                                  
034200                                                                          
034300     MOVE 'PT0 '     TO POSTSUM-TRANSTYP                                  
034400     MOVE 'W33040'   TO POSTSUM-FDNAMN                                    
034500     MOVE 'W3303FD4' TO POSTSUM-DDNAMN2                                   
034600     CALL POSTSUM USING POSTSUM-PARM                                      
034700     .                                                                    
034800     EJECT                                                                
034900 S11-SKRIV-W3303FU SECTION.                                               
035000                                                                          
035100     WRITE UT1-POST FROM UT-AREA                                          
035200                                                                          
035300     MOVE 'UT1'      TO POSTSUM-TRANSTYP                                  
035400     MOVE 'W3303F'   TO POSTSUM-FDNAMN                                    
035500     MOVE 'W3303FD3' TO POSTSUM-DDNAMN2                                   
035600     CALL POSTSUM USING POSTSUM-PARM                                      
035700     .                                                                    
035800     EJECT                                                                
035900 S14-SKRIV-W3303FV SECTION.                                               
036000                                                                          
036100     WRITE UTVCOM1-POST FROM UTVCOM-AREA                                  
036200                                                                          
036300     MOVE 'PT1'      TO POSTSUM-TRANSTYP                                  
036400     MOVE 'W33040'   TO POSTSUM-FDNAMN                                    
036500     MOVE 'W3303FD4' TO POSTSUM-DDNAMN2                                   
036600     CALL POSTSUM USING POSTSUM-PARM                                      
036700     .                                                                    
036800     EJECT                                                                
036900 S99-ABEND SECTION.                                                       
037000                                                                          
037100     MOVE 'S' TO POSTSUM-OPKOD                                            
037200     CALL POSTSUM USING POSTSUM-PARM                                      
037300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
037400     .                                                                    
