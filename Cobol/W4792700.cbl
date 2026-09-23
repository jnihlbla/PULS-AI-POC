001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4792700.                                                
001300 AUTHOR.         STEFAN KIHLBERG.                                         
001400 DATE-WRITTEN.   98/10/17.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700                                                                          
001800*    FUNKTION:                                                            
001900*        TAR BORT FÖR GAMLA DELIVERY NOTE UPPGIFTER FRÅN WDQ5             
002000*                                                                         
002110*        PROGRAMMET UPPDATERAR WLORQP (WDQ5)                              
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003001     SKIP2                                                                
003002*          --- ORDRAR SOM SKALL TAS BORT FRÅN WDQ5                        
003010     SELECT W47926                     ASSIGN TO W47927D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003601     SKIP3                                                                
003602 FD  W47926                                                               
003603     RECORDING       F                                                    
003604     BLOCK CONTAINS  0.                                                   
003605                                                                          
003610*01  -COPY W47926      -L.                                                
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W4792700'.            
004100 01  CHKP-VAR.                                                            
004200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004700     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000     SKIP2                                                                
005100 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005501                                                                          
005502 01  ARBESFALT.                                                           
005503     03 WS-IDORDNR               PIC S9(07)  VALUE ZERO.                  
005504                                                                          
005505 77  W47926-EOF-SW               PIC X       VALUE 'N'.                   
005510     88  END-OF-W47926                       VALUE 'J'.                   
005800     EJECT                                                                
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006400     EJECT                                                                
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006600*                                                                         
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  BORT-AREA-START             PIC X(24)   VALUE                        
007303                                             'BORT-AREA-START'.           
007304     SKIP2                                                                
007305                                                                          
007310*01  AREA -COPY W47926     -PRE BORT-                                     
007400*                                                                         
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007700     SKIP3                                                                
007800 01  NYCKLAR-TILL-DLI.                                                    
007920     03  W-WDQ501KY-MIN-X.                                                
007930         05  W-IDORDER-MIN       PIC S9(7) COMP-3 VALUE ZERO.             
007940         05  W-IDARTNR-MIN       PIC S9(9) COMP-3 VALUE ZERO.             
007950         05  W-IDLOPNR-MIN       PIC S9(3) COMP-3 VALUE ZERO.             
007960         05  W-IDSEKVNR-MIN      PIC S9(3) COMP-3 VALUE ZERO.             
007970         05  W-IDDC-MIN          PIC  X(2) VALUE SPACE.                   
007980         05  W-KDORDBEK-MIN      PIC  9(2) VALUE ZERO.                    
007990                                                                          
007991     03  W-WDQ501KY-MAX-X.                                                
007992         05  W-IDORDER-MAX       PIC S9(7) COMP-3 VALUE ZERO.             
007993         05  W-IDARTNR-MAX       PIC S9(9) COMP-3 VALUE ZERO.             
007995         05  W-IDLOPNR-MAX       PIC S9(3) COMP-3 VALUE ZERO.             
007996         05  W-IDSEKVNR-MAX      PIC S9(3) COMP-3 VALUE ZERO.             
007997         05  W-IDDC-MAX          PIC  X(2) VALUE HIGH-VALUE.              
007998         05  W-KDORDBEK-MAX      PIC  9(2) VALUE ZERO.                    
007999                                                                          
008000                                                                          
008010     SKIP2                                                                
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                       VALUE '  '.                  
008400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008700     88  IMS-EJ-OK                           VALUE 'XD'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(160).                              
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000                                                                          
010101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLORQP01'.                    
010102 01  DLI-IO-WLORQP01.                                                     
010110*    03  -COPY WDQ501                                                     
010200                                                                          
010600     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010800                                                                          
010900*01  -COPY W0009   -PRE MSG-                                              
011001                                                                          
011002*01  -COPY W0008  -PRE ORQP-                                              
011010     05  FILLER                  PIC X.                                   
011300     EJECT                                                                
011401 PROCEDURE DIVISION  USING MSG-PCB ORQP-PCB.                              
011402 MAIN SECTION.                                                            
011410     ENTRY 'DLITCBL' USING MSG-PCB ORQP-PCB.                              
011500                                                                          
011700     SKIP2                                                                
011800     PERFORM A-INIT                                                       
011910     PERFORM S01-LAES-W47926                                              
012000     PERFORM UNTIL END-OF-W47926                                          
012310        MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                 
012320        MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                 
012330        MOVE BORT-IDORDER             TO W-IDORDER-MIN                    
012340                                         W-IDORDER-MAX                    
012350        PERFORM IMS-GHU-ORQP-RADB-MIN-MAX                                 
012400        PERFORM UNTIL SEGMENT-SAKNAS                                      
012500           PERFORM IMS-DLET-ORQP-RADB                                     
012501           ADD +1                     TO CHKP-ANT                         
012502           IF CHKP-ANT > CHKP-MAX                                         
012503              PERFORM X-TAG-CHECKPOINT                                    
012504           END-IF                                                         
012510           PERFORM IMS-GHN-ORQP-RADB-MIN-MAX                              
012600        END-PERFORM                                                       
013010        PERFORM S01-LAES-W47926                                           
013100     END-PERFORM                                                          
013200                                                                          
013300                                                                          
013400     PERFORM Z-FINIT                                                      
013500                                                                          
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 A-INIT SECTION.                                                          
014100     SKIP2                                                                
014200                                                                          
014300     PERFORM IMS-RESTART                                                  
014501                                                                          
014510     OPEN INPUT W47926                                                    
014800                                                                          
015100                                                                          
015210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015500     .                                                                    
015700     EJECT                                                                
015710                                                                          
015720                                                                          
015800 Z-FINIT SECTION.                                                         
015900                                                                          
016310     CLOSE W47926                                                         
016501                                                                          
016502     MOVE 'S' TO POSTSUM-OPKOD                                            
016510     CALL POSTSUM USING POSTSUM-PARM                                      
016700     .                                                                    
016801     EJECT                                                                
016802                                                                          
016803                                                                          
016804 S01-LAES-W47926  SECTION.                                                
016805                                                                          
016806     READ W47926 INTO BORT-AREA                                           
016807     AT END                                                               
016809        SET END-OF-W47926 TO TRUE                                         
016810                                                                          
016811     NOT AT END                                                           
016812        MOVE 'W47926' TO POSTSUM-FDNAMN                                   
016813        MOVE 'W47927D1' TO POSTSUM-DDNAMN2                                
016815        CALL POSTSUM USING POSTSUM-PARM                                   
016816                                                                          
016818     END-READ                                                             
016820     .                                                                    
017100     EJECT                                                                
017200 X-TAG-CHECKPOINT   SECTION.                                              
017300                                                                          
017900     PERFORM IMS-CHECKPOINT                                               
018000     MOVE ZERO TO CHKP-ANT                                                
018100     MOVE LOW-VALUE                TO W-WDQ501KY-MIN-X                    
018110     MOVE HIGH-VALUE               TO W-WDQ501KY-MAX-X                    
018120     MOVE BORT-IDORDER             TO W-IDORDER-MIN                       
018130                                      W-IDORDER-MAX                       
018140     PERFORM IMS-GHU-ORQP-RADB-MIN-MAX                                    
018141     IF SEGMENT-FINNS                                                     
018142        PERFORM IMS-DLET-ORQP-RADB                                        
018143     END-IF                                                               
018200     .                                                                    
018300     EJECT                                                                
018310                                                                          
018320                                                                          
018400* --- IMS SEKTIONER ---                                                   
018500                                                                          
018602                                                                          
018603                                                                          
018604 IMS-GHU-ORQP-RADB-MIN-MAX SECTION.                                       
018605     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-MIN-X                        
018606                    '&WDQ501KY<=' W-WDQ501KY-MAX-X ')'                    
018607          DELIMITED BY SIZE INTO SSA1                                     
018608     MOVE '  GE' TO GODK-STATUSKODER                                      
018609     CALL CBLTDLI USING GHU ORQP-PCB DLI-IO-WLORQP01 SSA1                 
018610     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
018620     PERFORM IMS-STATUSKONTROLL                                           
018621     .                                                                    
018622     EJECT                                                                
018623                                                                          
018624                                                                          
018625 IMS-GHN-ORQP-RADB-MIN-MAX SECTION.                                       
018626     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-MIN-X                        
018627                    '&WDQ501KY<=' W-WDQ501KY-MAX-X ')'                    
018628          DELIMITED BY SIZE INTO SSA1                                     
018629     MOVE '  GE' TO GODK-STATUSKODER                                      
018630     CALL CBLTDLI USING GHN ORQP-PCB DLI-IO-WLORQP01 SSA1                 
018631     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
018632     PERFORM IMS-STATUSKONTROLL                                           
018633     .                                                                    
018634     EJECT                                                                
018635                                                                          
018636                                                                          
018637 IMS-DLET-ORQP-RADB SECTION.                                              
018638                                                                          
018639     MOVE '  ' TO GODK-STATUSKODER                                        
018640     CALL CBLTDLI USING DLET ORQP-PCB DLI-IO-WLORQP01                     
018641     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
018642     PERFORM IMS-STATUSKONTROLL                                           
018650     .                                                                    
018700     EJECT                                                                
018710                                                                          
018720                                                                          
018800 IMS-RESTART SECTION.                                                     
018900                                                                          
019000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019100     MOVE '  ' TO GODK-STATUSKODER                                        
019200     CALL CBLTDLI USING XRST MSG-PCB                                      
019300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019400                        CHKP-AREA-LENGTH CHKP-AREA                        
019500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019600     PERFORM IMS-STATUSKONTROLL                                           
019700     .                                                                    
019800                                                                          
019810                                                                          
019900 IMS-CHECKPOINT SECTION.                                                  
020000     SKIP2                                                                
020100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020200     MOVE '  XD' TO GODK-STATUSKODER                                      
020300     CALL CBLTDLI USING CHKP MSG-PCB                                      
020400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020500                        CHKP-AREA-LENGTH CHKP-AREA                        
020600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020700     PERFORM IMS-STATUSKONTROLL                                           
020800                                                                          
020900     IF IMS-EJ-OK                                                         
021000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021100       DISPLAY FELTEXT                                                    
021200       CALL FELLOG                                                        
021300     END-IF                                                               
021400     .                                                                    
021500     EJECT                                                                
021600 IMS-STATUSKONTROLL SECTION.                                              
021700     SKIP2                                                                
021800     SET STATUS-IX TO 1                                                   
021900     SEARCH GODK-STATUS                                                   
022000       AT END                                                             
022100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022200           DELIMITED BY SIZE INTO FELTEXT                                 
022300         DISPLAY FELTEXT                                                  
022400         CALL FELLOG                                                      
022500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022600         CONTINUE                                                         
022700     END-SEARCH                                                           
022800     .                                                                    
