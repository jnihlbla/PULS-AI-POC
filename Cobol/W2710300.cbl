000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2710300.                                                
000300 AUTHOR.         NIHLBLAD JOHAN.                                          
000400 DATE-WRITTEN.   05/08/30.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SUMMERAR BACK ORDRAR PÅ IDARTNR OCH SEDAN PÅ DC                  
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- INFIL                                                      
002401     SELECT W44061                     ASSIGN TO W27103D1.                
002402     SKIP2                                                                
002403*          --- INFIL                                                      
002404     SELECT W27105                     ASSIGN TO W27103D2.                
002405     SKIP2                                                                
002406*          --- UTFIL                                                      
002407     SELECT W27103                     ASSIGN TO W27103D3.                
002408     EJECT                                                                
002409 DATA DIVISION.                                                           
002410     SKIP3                                                                
002420 FILE SECTION.                                                            
002430     SKIP3                                                                
002440 FD  W44061                                                               
002450     RECORDING       F                                                    
002460     BLOCK CONTAINS  0.                                                   
002470                                                                          
002480*01  -COPY W44060       -PRE  IN2-  -L.                                   
002490     SKIP3                                                                
002500 FD  W27105                                                               
002600     RECORDING       F                                                    
002700     BLOCK CONTAINS  0.                                                   
002800                                                                          
002900*01  -COPY W27105       -PRE  IN1-  -L.                                   
003000     SKIP3                                                                
003010 FD  W27103                                                               
003011     RECORDING       F                                                    
003012     BLOCK CONTAINS  0.                                                   
003013                                                                          
003014*01  POST -COPY W27105  -PRE  UT-  -L.                                    
003015     EJECT                                                                
003016 WORKING-STORAGE SECTION.                                                 
003017                                                                          
003018 77  IDPGM                       PIC X(8)    VALUE 'W2710300'.            
003019 77  JA                          PIC X       VALUE 'J'.                   
003020 77  NEJ                         PIC X       VALUE 'N'.                   
003030                                                                          
003040 77  W44061-EOF-SW               PIC X       VALUE 'N'.                   
003050     88  END-OF-W44061                       VALUE 'J'.                   
003060 77  W27105-EOF-SW               PIC X       VALUE 'N'.                   
003070     88  END-OF-W27105                       VALUE 'J'.                   
003080     EJECT                                                                
003090 01  WS-IDARTNR                  PIC S9(9)  VALUE ZERO COMP-3.            
003100 01  WS-KVBEART-Q                PIC S9(7)  VALUE ZERO COMP-3.            
003200 01  WS-SPARA-IDDC               PIC X(2)   VALUE SPACE.                  
003300 01  W-IDARTNR                   PIC S9(9)  VALUE ZERO COMP-3.            
003400 01  W-IDDC                      PIC X(2)   VALUE SPACE.                  
003500 01  WS-SPARA-IDDISTR            PIC S9(5)  VALUE ZERO COMP-3.            
003600*                                                                         
003700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003800 01  FILLER REDEFINES DAGENS-DATUM.                                       
003900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004200     EJECT                                                                
004300 01 WS-ANTAL-W27105             PIC 9(9)   VALUE ZERO.                    
004400 01 WS-ANTAL-W44061             PIC 9(9)   VALUE ZERO.                    
004500*                                                                         
004510 01  FILLER                      PIC X(16)   VALUE 'DIST-DC-TAB'.         
004520     -COPY WWDIST57                                                       
004530                                                                          
004540*01  -COPY WWDIST35                                                       
004550     EJECT                                                                
004560*      --- VALID IDDC CODES                                               
004570*                                                                         
004580*01    -COPY WWDC99                                                       
004590*                                                                         
004591       EJECT                                                              
004592 01  DYNAMISKA-SUBPROGRAM.                                                
004593*                                                                         
004594     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004595     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004596     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004597     SKIP2                                                                
004598*    --- PARAMETRAR TILL ABEND                                            
004599                                                                          
004600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
004700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
004800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
004900     SKIP2                                                                
005000 01  FELTEXT.                                                             
005100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005300     EJECT                                                                
005400*    --- STATUS-KOD FRÅN IMS                                              
005500 01  STATUS-WS                   PIC XX.                                  
005600     88  SEGMENT-FINNS                       VALUE '  '.                  
005700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
005800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
005900     SKIP2                                                                
006000 01  GODK-STATUSKODER.                                                    
006001     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
006002     SKIP3                                                                
006003*    --- PARAMETRAR TILL POSTSUM                                          
006004*                                                                         
006005*01  -COPY W0005   -PRE  POSTSUM-                                         
006006     EJECT                                                                
006007 01  IN2-AREA-START               PIC X(24)   VALUE                       
006008                                 'IN2-AREA-START  '.                      
006009     SKIP2                                                                
006010                                                                          
006020*01  AREA -COPY W44060      -PRE IN2-                                     
006030     EJECT                                                                
006040 01  IN1-AREA-START              PIC X(24)   VALUE                        
006050                                 'IN1-AREA-START  '.                      
006060     SKIP2                                                                
006070                                                                          
006080*01  AREA -COPY W27105      -PRE IN1-                                     
006090     EJECT                                                                
006100 01  UT-AREA-START               PIC X(24)   VALUE                        
006200                                 'UT-AREA-START  '.                       
006210     SKIP2                                                                
006211                                                                          
006212*01  AREA -COPY W27105      -PRE UT-                                      
006213     EJECT                                                                
006214 PROCEDURE DIVISION.                                                      
006215 MAIN SECTION.                                                            
006216     SKIP2                                                                
006217                                                                          
006218     PERFORM A-INIT                                                       
006219     PERFORM S01-LAES-W27105                                              
006220     PERFORM S02-LAES-W44061                                              
006230     PERFORM UNTIL END-OF-W44061 OR END-OF-W27105                         
006240       PERFORM B-BEARBETA                                                 
006250     END-PERFORM                                                          
006260     PERFORM Z-FINIT                                                      
006270                                                                          
006280     MOVE ZERO TO RETURN-CODE                                             
006290     GOBACK                                                               
006300     .                                                                    
006400     EJECT                                                                
006500 A-INIT SECTION.                                                          
006600                                                                          
006700     OPEN INPUT  W44061                                                   
006800                 W27105                                                   
006900                                                                          
007000     OPEN OUTPUT W27103                                                   
007100     SKIP2                                                                
007200     ACCEPT DAGENS-DATUM  FROM DATE                                       
007300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
007400     .                                                                    
007500     EJECT                                                                
007600 B-BEARBETA SECTION.                                                      
007700                                                                          
007800     IF END-OF-W27105                                                     
007900     OR  IN1-IDARTNR > IN2-RAD-IDARTNR                                    
008000     OR (IN1-IDARTNR = IN2-RAD-IDARTNR                                    
008100     AND IN1-IDDISTR > IN2-RAD-IDDISTR)                                   
008200                                                                          
008300       ADD 1               TO WS-ANTAL-W44061                             
008400       PERFORM S02-LAES-W44061                                            
008500                                                                          
008600     ELSE                                                                 
008700       IF END-OF-W44061                                                   
008800       OR  IN2-RAD-IDARTNR > IN1-IDARTNR                                  
008900       OR (IN2-RAD-IDARTNR = IN1-IDARTNR                                  
009000       AND IN2-RAD-IDDISTR > IN1-IDDISTR)                                 
009100                                                                          
009200         ADD 1               TO WS-ANTAL-W27105                           
009300         PERFORM S01-LAES-W27105                                          
009400                                                                          
009500       ELSE                                                               
009510*                                                                         
009520***** MATCHNING                                                           
009530*                                                                         
009540         MOVE IN1-IDPERSON               TO UT-IDPERSON                   
009541         MOVE IN1-PRARTSTD               TO UT-PRARTSTD                   
009542         MOVE IN1-KVPB-REF               TO UT-KVPB-REF                   
009543         MOVE IN1-AVAILABLE              TO UT-AVAILABLE                  
009544         MOVE IN2-RAD-IDARTNR            TO UT-IDARTNR                    
009545                                            WS-IDARTNR                    
009546         MOVE IN2-RAD-IDDC               TO WS-IDDC                       
009547         MOVE IN1-IDDISTR                TO WS-SPARA-IDDISTR              
009548                                            UT-IDDISTR                    
009549         MOVE ZERO                       TO WS-KVBEART-Q                  
009550         MOVE IN1-PRMATRL                TO UT-PRMATRL                    
009551         PERFORM UNTIL END-OF-W44061                                      
009552         OR  NOT (IN2-RAD-IDARTNR = WS-IDARTNR                            
009553         AND  IN2-RAD-IDDISTR = WS-SPARA-IDDISTR)                         
009554           IF IN2-RAD-KDSTARAD = '2'                                      
009555             ADD IN2-RAD-KVBEART-Q TO WS-KVBEART-Q                        
009556           END-IF                                                         
009557           PERFORM C-KOLLA-DC                                             
009558           PERFORM S02-LAES-W44061                                        
009559         END-PERFORM                                                      
009560         IF NDC-CN OR NDC-NA                                              
009561           COMPUTE UT-TOTALSUM = WS-KVBEART-Q * IN1-PRMATRL               
009562         ELSE                                                             
009563           COMPUTE UT-TOTALSUM = WS-KVBEART-Q * IN1-PRARTSTD              
009564         END-IF                                                           
009565         PERFORM UNTIL END-OF-W27105                                      
009566         OR NOT (IN1-IDARTNR = WS-IDARTNR                                 
009567         AND IN1-IDDISTR  = WS-SPARA-IDDISTR)                             
009568           PERFORM S01-LAES-W27105                                        
009569         END-PERFORM                                                      
009570         PERFORM S11-SKRIV-W27103                                         
009571       END-IF                                                             
009572     END-IF                                                               
009573     .                                                                    
009574     EJECT                                                                
009575 C-KOLLA-DC SECTION.                                                      
009576                                                                          
009577     IF CDC                                                               
009578       MOVE IN2-RAD-IDDISTR TO DIST35-IDDISTR                             
009579       IF DIST35-REFILL                                                   
009580         SEARCH ALL DIST57-REFILL-DC                                      
009581           AT END                                                         
009582             MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                         
009583                               TO FELTEXT                                 
009584             CALL FELLOG                                                  
009585           WHEN DIST57-SOK-IDDISTR(DIST57-IX) = IN2-RAD-IDDISTR           
009586             MOVE DIST57-REFILL-TO-DC(DIST57-IX)                          
009587                               TO UT-IDDC                                 
009588         END-SEARCH                                                       
009589       END-IF                                                             
009590     ELSE                                                                 
009591       MOVE IN2-RAD-IDDC       TO UT-IDDC                                 
009592     END-IF                                                               
009593     .                                                                    
009594     EJECT                                                                
009595                                                                          
009596                                                                          
009597 Z-FINIT SECTION.                                                         
009598     CLOSE W44061                                                         
009599           W27105                                                         
009600           W27103                                                         
009700     SKIP2                                                                
009800     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009820     .                                                                    
009830     EJECT                                                                
009840 S01-LAES-W27105  SECTION.                                                
009850     READ W27105 INTO IN1-AREA                                            
009860     AT END                                                               
009870        MOVE HIGH-VALUE TO IN1-AREA                                       
009880        SET END-OF-W27105 TO TRUE                                         
009890                                                                          
009900     NOT AT END                                                           
010000        MOVE 'W27105' TO POSTSUM-FDNAMN                                   
010010        MOVE 'W27103D2' TO POSTSUM-DDNAMN2                                
010011*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
010012*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
010013        MOVE SPACE     TO POSTSUM-TRANSTYP                                
010014        CALL POSTSUM USING POSTSUM-PARM                                   
010015     END-READ                                                             
010016     .                                                                    
010017     EJECT                                                                
010018 S02-LAES-W44061  SECTION.                                                
010019     READ W44061 INTO IN2-AREA                                            
010020     AT END                                                               
010021        MOVE HIGH-VALUE TO IN2-AREA                                       
010022        SET END-OF-W44061 TO TRUE                                         
010023                                                                          
010024     NOT AT END                                                           
010025        MOVE 'W44061' TO POSTSUM-FDNAMN                                   
010026        MOVE 'W27103D1' TO POSTSUM-DDNAMN2                                
010027*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
010028*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
010029        MOVE SPACE     TO POSTSUM-TRANSTYP                                
010030        CALL POSTSUM USING POSTSUM-PARM                                   
010031     END-READ                                                             
010032     .                                                                    
010033     EJECT                                                                
010034 S11-SKRIV-W27103 SECTION.                                                
010035                                                                          
010036     WRITE UT-POST FROM UT-AREA                                           
010037                                                                          
010038     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
010039     MOVE 'W27103' TO POSTSUM-FDNAMN                                      
010040     MOVE 'W27103D3' TO POSTSUM-DDNAMN2                                   
010050     CALL POSTSUM USING POSTSUM-PARM                                      
010060     .                                                                    
010070     EJECT                                                                
010080 S99-ABEND SECTION.                                                       
010090                                                                          
010100     SKIP2                                                                
010200     MOVE 'S' TO POSTSUM-OPKOD                                            
010300     CALL POSTSUM USING POSTSUM-PARM                                      
010400     CALL ABEND USING RKOD-ABEND                                          
010500     .                                                                    
010600     EJECT                                                                
010700 IMS-STATUSKONTROLL SECTION.                                              
010800                                                                          
010900     SET STATUS-IX TO 1                                                   
011000     SEARCH GODK-STATUS                                                   
011100       AT END                                                             
011200         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
011300           DELIMITED BY SIZE INTO FELTEXT-STR                             
011400         DISPLAY FELTEXT                                                  
011500         CALL FELLOG                                                      
011600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
011700         CONTINUE                                                         
011800     END-SEARCH                                                           
011900     .                                                                    
