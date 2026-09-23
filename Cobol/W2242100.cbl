000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2242100.                                                
000300 AUTHOR.         NIHLBLAD JOHAN.                                          
000400 DATE-WRITTEN.   FEB-13.                                                  
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER WDK7, BESTÄLLS FRÅN 2111 FÖR ATT UPPDATERA                 
000900*        ANSKAFFARE                                                       
001000*                                                                         
001300*        PROGRAMMET LÄSER      WDK7                                       
001400*                              WDK6                                       
001410*                              WDF1                                       
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
003010*          --- UTFIL MED ARTIKLAR SOM SKALL UPPDATERA ANSKAFFARE          
003020     SELECT W22421                     ASSIGN TO W22421D1.                
003030     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W22421                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  POST -COPY W22421 -PRE  UT-  -L.                                     
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W2242100'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
005410 01  PARM-SYSIN.                                                          
005420     03  PARM-IDLEVNR         PIC X(5)  VALUE SPACE.                      
005430     03  PARM-IDDC            PIC X(2)  VALUE SPACE.                      
005460     03  FILLER               PIC X(73).                                  
005470                                                                          
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     SKIP2                                                                
007800*    --- PARAMETRAR TILL ABEND                                            
007900                                                                          
008000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008300     SKIP2                                                                
008400 01  FELTEXT.                                                             
008500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008700     EJECT                                                                
008710*01    -COPY WWDC99                                                       
008800*    --- PARAMETRAR TILL POSTSUM                                          
008900*                                                                         
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200 01  UT-AREA-START               PIC X(24)   VALUE                        
009300                                 'UT-AREA-START  '.                       
009400     SKIP2                                                                
009500                                                                          
009600*01  AREA -COPY W22421     -PRE UT-                                       
009700     EJECT                                                                
009800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009900*                                                                         
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300 01  NYCKLAR-TILL-DLI.                                                    
010400     03  W-IDLEVNR-X.                                                     
010500         05  W-IDLEVNR              PIC X(5)    VALUE SPACE.              
010600     03  W-IDARTNR-X.                                                     
010700         05  W-IDARTNR              PIC S9(9)   COMP-3.                   
010710     03  W-IDDC-X.                                                        
010720         05  W-IDDC                 PIC X(2)    VALUE SPACE.              
010800                                                                          
015300     03 WDK7A1KY-MIN-X.                                                   
015400        05 W-IDDC-K7-MIN        PIC X(2)         VALUE SPACE.             
015500        05 FILLER               PIC X(12)        VALUE LOW-VALUE.         
016000                                                                          
016100     03 WDK7A1KY-MAX-X.                                                   
016200        05  W-IDDC-K7-MAX       PIC X(2)         VALUE SPACE.             
016210        05 FILLER               PIC X(12)        VALUE HIGH-VALUE.        
016800                                                                          
016801     03 WDK6A1KY-MIN-X.                                                   
016802        05 W-IDLEVNR-K6-MIN     PIC X(5)         VALUE SPACE.             
016803        05 FILLER               PIC X(5)         VALUE LOW-VALUE.         
016804                                                                          
016805     03 WDK6A1KY-MAX-X.                                                   
016806        05  W-IDLEVNR-K6-MAX    PIC X(5)         VALUE SPACE.             
016807        05 FILLER               PIC X(5)         VALUE HIGH-VALUE.        
016808                                                                          
016810     03  W-KDSEGKEY-X.                                                    
016820         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016830                                                                          
016900     SKIP2                                                                
017000*    --- STATUS-KOD FRÅN IMS                                              
017100 01  STATUS-WS                   PIC XX.                                  
017200     88  SEGMENT-FINNS                       VALUE '  '.                  
017300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017500     88  BAS-SLUT                            VALUE 'GB'.                  
017600     SKIP2                                                                
017700 01  GODK-STATUSKODER.                                                    
017800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900     SKIP3                                                                
018000 01  SSA1                        PIC X(128).                              
018100 01  SSA2                        PIC X(128).                              
018110 01  SSA3                        PIC X(128).                              
018200     EJECT                                                                
018300*    --- IMS FUNKTIONSKODER                                               
018400*01  -COPY W0003                                                          
018500     EJECT                                                                
018600*    ---  DLI INPUT-OUTPUT AREA                                           
019000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7A1'.                      
019100 01  DLI-IO-WDK7A1.                                                       
019200*    03  -COPY WDK7A1 -PRE K7-                                            
019300                                                                          
019900 01  FILLER               PIC X(16)   VALUE 'WDK711 AREA'.                
020000 01   DLI-IO-WDK711.                                                      
020100*     03  -COPY WDK711                                                    
020200     EJECT                                                                
020201                                                                          
020210 01  FILLER               PIC X(16)   VALUE 'WDK722 AREA'.                
020220 01   DLI-IO-WDK722.                                                      
020230*     03  -COPY WDK722                                                    
020240     EJECT                                                                
020241                                                                          
020250 01  FILLER               PIC X(16)   VALUE 'WDF116 AREA'.                
020260 01   DLI-IO-WDF116.                                                      
020270*     03  -COPY WDF116                                                    
020280     EJECT                                                                
020281                                                                          
020290 01  FILLER               PIC X(16)   VALUE 'WDF101 AREA'.                
020291 01   DLI-IO-WDF101.                                                      
020292*     03  -COPY WDF101                                                    
020293     EJECT                                                                
020294                                                                          
020295 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6A1'.                      
020296 01  DLI-IO-WDK6A1.                                                       
020297*    03  -COPY WDK6A1 -PRE K6-                                            
020298                                                                          
020299 01  FILLER               PIC X(16)   VALUE 'WDK611 AREA'.                
020300 01   DLI-IO-WDK611.                                                      
020301*     03  -COPY WDK611                                                    
020302     EJECT                                                                
020303                                                                          
020310 LINKAGE SECTION.                                                         
020400                                                                          
020500                                                                          
020900*01  -COPY W0008  -PRE WDK7A-                                             
021000     05  FILLER                  PIC X.                                   
021600*01  -COPY W0008  -PRE WDK7-                                              
021700     05  FILLER                  PIC X.                                   
021710*01  -COPY W0008  -PRE WDF1-                                              
021720     05  FILLER                  PIC X.                                   
021730*01  -COPY W0008  -PRE WDK6A-                                             
021740     05  FILLER                  PIC X.                                   
021750*01  -COPY W0008  -PRE WDK6-                                              
021760     05  FILLER                  PIC X.                                   
021800     EJECT                                                                
021900 PROCEDURE DIVISION  USING WDK7A-PCB WDK7-PCB WDF1-PCB                    
022000                           WDK6A-PCB WDK6-PCB.                            
022100 MAIN SECTION.                                                            
022200     ENTRY 'DLITCBL' USING WDK7A-PCB WDK7-PCB WDF1-PCB                    
022300                           WDK6A-PCB WDK6-PCB.                            
022400                                                                          
022500                                                                          
022600     PERFORM A-INIT                                                       
022601     MOVE PARM-IDDC        TO WS-IDDC                                     
022602     MOVE PARM-IDLEVNR     TO W-IDLEVNR                                   
022603     IF CDC-SE                                                            
022604       PERFORM B-KOLLA-CDC                                                
022630     ELSE                                                                 
022640       PERFORM C-KOLLA-NDC-CN                                             
027200     END-IF                                                               
031000                                                                          
031100                                                                          
031200     PERFORM Z-FINIT                                                      
031300                                                                          
031400     MOVE ZERO TO RETURN-CODE                                             
031500     GOBACK                                                               
031600     .                                                                    
031700     EJECT                                                                
031800 A-INIT SECTION.                                                          
031900                                                                          
031980     ACCEPT PARM-SYSIN FROM SYSIN                                         
031990     UNSTRING PARM-SYSIN DELIMITED BY ','                                 
031991       INTO PARM-IDLEVNR                                                  
031992            PARM-IDDC                                                     
031995                                                                          
031997     DISPLAY 'PARM-IDDC     : ' PARM-IDDC                                 
031998     DISPLAY 'PARM-IDLEVNR  : ' PARM-IDLEVNR                              
032001                                                                          
032010     OPEN OUTPUT W22421                                                   
032100                                                                          
032200     ACCEPT DAGENS-DATUM  FROM DATE                                       
032300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
032500     .                                                                    
032600     EJECT                                                                
032700 B-KOLLA-CDC SECTION.                                                     
032701     MOVE PARM-IDLEVNR     TO W-IDLEVNR-K6-MIN                            
032702                              W-IDLEVNR-K6-MAX                            
032703     PERFORM IMS-GU-WDK6A1                                                
032704     IF SEGMENT-FINNS                                                     
032705       PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                           
032706         MOVE K6-SEQA-IDARTNR       TO W-IDARTNR                          
032707         PERFORM IMS-GU-WDK611                                            
032708         IF SEGMENT-FINNS                                                 
032709           PERFORM IMS-GU-WDF101                                          
032710           IF SEGMENT-FINNS                                               
032711             IF CLAG-IDPLANGR-AG = ZERO OR 9                              
032712               CONTINUE                                                   
032713             ELSE                                                         
032714               IF LEV-IDANSK-PG(CLAG-IDPLANGR-AG) = CLAG-IDANSK           
032715                 CONTINUE                                                 
032716               ELSE                                                       
032717                 MOVE K6-SEQA-IDARTNR TO UT-IDARTNR                       
032718                 MOVE PARM-IDDC       TO UT-IDDC                          
032719                 MOVE LEV-IDANSK-PG(CLAG-IDPLANGR-AG)                     
032720                                        TO UT-IDANSK-NEW                  
032721                 PERFORM S11-SKRIV-W22421                                 
032722               END-IF                                                     
032723             END-IF                                                       
032724           END-IF                                                         
032725         END-IF                                                           
032726         PERFORM IMS-GN-WDK6A1                                            
032727       END-PERFORM                                                        
032728     END-IF                                                               
032729     .                                                                    
032730     EJECT                                                                
032800 C-KOLLA-NDC-CN SECTION.                                                  
032810     MOVE PARM-IDDC        TO W-IDDC-K7-MIN                               
032820                            W-IDDC-K7-MAX                                 
032830     PERFORM IMS-GU-WDK7A1                                                
032840     IF SEGMENT-FINNS                                                     
032850       PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                           
032860         MOVE PARM-IDDC             TO W-IDDC                             
032870         MOVE K7-SEQA-IDARTNR       TO W-IDARTNR                          
032880         PERFORM IMS-GU-WDK711                                            
032881         IF SEGMENT-FINNS                                                 
032882           IF SLAG-IDDC-REF = SPACE                                       
032883             PERFORM IMS-GU-WDK722                                        
032890             IF SEGMENT-FINNS                                             
032891               PERFORM IMS-GU-WDF116                                      
032892               IF SEGMENT-FINNS                                           
032893                 IF XLAG-IDPLANGR-AG = ZERO OR 9                          
032895                   CONTINUE                                               
032896                 ELSE                                                     
032897                   IF NDC-IDANSK-PG(XLAG-IDPLANGR-AG) =                   
032898                                            XLAG-IDANSK                   
032899                     CONTINUE                                             
032900                   ELSE                                                   
032901                     MOVE K7-SEQA-IDARTNR TO UT-IDARTNR                   
032902                     MOVE PARM-IDDC       TO UT-IDDC                      
032903                     MOVE NDC-IDANSK-PG(XLAG-IDPLANGR-AG)                 
032904                                          TO UT-IDANSK-NEW                
032905                     PERFORM S11-SKRIV-W22421                             
032906                   END-IF                                                 
032907                 END-IF                                                   
032908               END-IF                                                     
032909             END-IF                                                       
032910           END-IF                                                         
032911         END-IF                                                           
032912         PERFORM IMS-GN-WDK7A1                                            
032913       END-PERFORM                                                        
032914     END-IF                                                               
032920     .                                                                    
033000     EJECT                                                                
035100 Z-FINIT SECTION.                                                         
035300     SKIP2                                                                
035400     MOVE 'S' TO POSTSUM-OPKOD                                            
035500     CALL POSTSUM USING POSTSUM-PARM                                      
035600     .                                                                    
035700     EJECT                                                                
035701                                                                          
035800 S11-SKRIV-W22421 SECTION.                                                
035900                                                                          
036000     WRITE UT-POST FROM UT-AREA                                           
036100                                                                          
036200     MOVE 'UT-'      TO POSTSUM-TRANSTYP                                  
036300     MOVE W-IDDC     TO POSTSUM-FDNAMN                                    
036400     MOVE 'W22421D1' TO POSTSUM-DDNAMN2                                   
036500     CALL POSTSUM USING POSTSUM-PARM                                      
036600     .                                                                    
036700     EJECT                                                                
036800* --- IMS SEKTIONER ---                                                   
036900                                                                          
037000     EJECT                                                                
042600 IMS-GU-WDK7A1 SECTION.                                                   
042700     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
042800                    '&WDK7A1KY<=' WDK7A1KY-MAX-X                          
042810                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
042900          DELIMITED BY SIZE INTO SSA1                                     
043000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
043100     CALL CBLTDLI USING GU WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
043200     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
043300     PERFORM IMS-STATUSKONTROLL                                           
043400     .                                                                    
043500     SKIP3                                                                
043600 IMS-GN-WDK7A1 SECTION.                                                   
043700     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
043800                    '&WDK7A1KY<=' WDK7A1KY-MAX-X                          
043810                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
043900          DELIMITED BY SIZE INTO SSA1                                     
044000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
044100     CALL CBLTDLI USING GN WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
044200     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
044300     PERFORM IMS-STATUSKONTROLL                                           
044400     .                                                                    
044500     SKIP3                                                                
044600 IMS-GU-WDK711 SECTION.                                                   
044700                                                                          
044800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
044900     DELIMITED BY SIZE INTO SSA1                                          
045000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
045100     DELIMITED BY SIZE INTO SSA2                                          
045200     MOVE '  ' TO GODK-STATUSKODER                                        
045300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
045400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
045500     PERFORM IMS-STATUSKONTROLL                                           
045600     .                                                                    
045700     SKIP2                                                                
045701 IMS-GU-WDK722 SECTION.                                                   
045702                                                                          
045703     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
045704     DELIMITED BY SIZE INTO SSA1                                          
045705     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
045706     DELIMITED BY SIZE INTO SSA2                                          
045707     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
045708     DELIMITED BY SIZE INTO SSA3                                          
045709     MOVE '  GE' TO GODK-STATUSKODER                                      
045710     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
045711     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
045712     PERFORM IMS-STATUSKONTROLL                                           
045713     .                                                                    
045714     SKIP2                                                                
045715 IMS-GU-WDK6A1 SECTION.                                                   
045716     STRING 'WDK6A1  (WDK6A1KY=>' WDK6A1KY-MIN-X                          
045717                    '&WDK6A1KY<=' WDK6A1KY-MAX-X ')'                      
045718          DELIMITED BY SIZE INTO SSA1                                     
045719     MOVE '  GEGB' TO GODK-STATUSKODER                                    
045720     CALL CBLTDLI USING GU WDK6A-PCB DLI-IO-WDK6A1 SSA1                   
045721     MOVE WDK6A-STATUS-CODE TO STATUS-WS                                  
045722     PERFORM IMS-STATUSKONTROLL                                           
045723     .                                                                    
045724     SKIP3                                                                
045725 IMS-GN-WDK6A1 SECTION.                                                   
045726     STRING 'WDK6A1  (WDK6A1KY=>' WDK6A1KY-MIN-X                          
045727                    '&WDK6A1KY<=' WDK6A1KY-MAX-X ')'                      
045728          DELIMITED BY SIZE INTO SSA1                                     
045729     MOVE '  GEGB' TO GODK-STATUSKODER                                    
045730     CALL CBLTDLI USING GN WDK6A-PCB DLI-IO-WDK6A1 SSA1                   
045731     MOVE WDK6A-STATUS-CODE TO STATUS-WS                                  
045732     PERFORM IMS-STATUSKONTROLL                                           
045733     .                                                                    
045734     SKIP3                                                                
045735 IMS-GU-WDK611 SECTION.                                                   
045736                                                                          
045737     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
045738     DELIMITED BY SIZE INTO SSA1                                          
045739     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
045740     DELIMITED BY SIZE INTO SSA2                                          
045741     MOVE '  GE' TO GODK-STATUSKODER                                      
045742     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
045743     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
045744     PERFORM IMS-STATUSKONTROLL                                           
045745     .                                                                    
045746     SKIP2                                                                
045747 IMS-GU-WDF101 SECTION.                                                   
045748                                                                          
045749     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
045750          DELIMITED BY SIZE INTO SSA1                                     
045770     MOVE '  GE' TO GODK-STATUSKODER                                      
045780     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
045790     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
045791     PERFORM IMS-STATUSKONTROLL                                           
045792     .                                                                    
045793     EJECT                                                                
045794 IMS-GU-WDF116 SECTION.                                                   
045795                                                                          
045796     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
045797          DELIMITED BY SIZE INTO SSA1                                     
045798     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
045799          DELIMITED BY SIZE INTO SSA2                                     
045800     MOVE '  GE' TO GODK-STATUSKODER                                      
045801     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
045802     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
045803     PERFORM IMS-STATUSKONTROLL                                           
045804     .                                                                    
045805     EJECT                                                                
045810 IMS-STATUSKONTROLL SECTION.                                              
045900                                                                          
046000     SET STATUS-IX TO 1                                                   
046100     SEARCH GODK-STATUS                                                   
046200       AT END                                                             
046300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
046400           DELIMITED BY SIZE INTO FELTEXT                                 
046500         DISPLAY FELTEXT                                                  
046600         CALL FELLOG                                                      
046700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046800         CONTINUE                                                         
046900     END-SEARCH                                                           
047000     .                                                                    
