000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W5128P00.                                                
000400 AUTHOR.         SARASWATHY.                                              
000500 DATE-WRITTEN.   20/02/24.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        MATCHES WDK6 AND WDK7 PARTS                                      
001100*                                                                         
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- WDK6 DATA                                                  
002403     SELECT W01160                     ASSIGN TO W5128PD1.                
002404     SKIP2                                                                
002405*          --- WDK7 DATA                                                  
002406     SELECT W01184                     ASSIGN TO W5128PD2.                
002407     SKIP2                                                                
002408*          --- MATCHED PARTS                                              
002410     SELECT W5128P                     ASSIGN TO W5128PD3.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W01160                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY W01160      -L.                                                
003007     SKIP3                                                                
003008 FD  W01184                                                               
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003011                                                                          
003012*01  -COPY W01184      -L.                                                
003013     SKIP3                                                                
003014 FD  W5128P                                                               
003015     RECORDING       F                                                    
003016     BLOCK CONTAINS  0.                                                   
003017                                                                          
003020*01  RECORD -COPY W5128P -PRE  UT-  -L.                                   
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W5128P00'.            
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
003803     88  END-OF-W01160                       VALUE 'J'.                   
003804                                                                          
003805 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W01184                       VALUE 'J'.                   
003900     EJECT                                                                
004000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES TODAYS-DATE.                                        
004200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004400     03  TODAYS-DATE-DAY         PIC 9(2).                                
004500     EJECT                                                                
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     SKIP2                                                                
005100*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  ERROR-TEXT.                                                          
005800     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201     EJECT                                                                
006202*01  -COPY WWDC99                                                         
006203     EJECT                                                                
006204 01  IN-AREA-START             PIC X(24)   VALUE                          
006205                                 'IN-AREA-START  '.                       
006206     SKIP2                                                                
006207                                                                          
006208 01  IN-AREA1.                                                            
006209*    03  -COPY W01160                                                     
006210     EJECT                                                                
006211 01  IN2-AREA-START             PIC X(24)   VALUE                         
006212                                 'IN2-AREA-START  '.                      
006213     SKIP2                                                                
006214                                                                          
006215 01  IN-AREA2.                                                            
006216*    03  -COPY W01184                                                     
006217     EJECT                                                                
006218 01  UT-AREA-START               PIC X(24)   VALUE                        
006219                                 'UT-AREA-START  '.                       
006220     SKIP2                                                                
006221                                                                          
006222 01  UT-AREA.                                                             
006223*    03  -COPY W5128P   -PRE UT-                                          
006300     EJECT                                                                
006310                                                                          
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007001     PERFORM S01-READ-W01160                                              
007010     PERFORM S02-READ-W01184                                              
007100     PERFORM UNTIL END-OF-W01160 OR END-OF-W01184                         
007200       EVALUATE TRUE                                                      
007300         WHEN CLAG-IDARTNR = SLAG-IDARTNR                                 
007301           PERFORM UNTIL END-OF-W01184                                    
007302           OR SLAG-IDARTNR NOT = CLAG-IDARTNR                             
007303             PERFORM B-PROCESS                                            
007305             PERFORM S02-READ-W01184                                      
007306           END-PERFORM                                                    
007310           PERFORM S01-READ-W01160                                        
007400         WHEN CLAG-IDARTNR > SLAG-IDARTNR                                 
007401           PERFORM S02-READ-W01184                                        
007410         WHEN CLAG-IDARTNR < SLAG-IDARTNR                                 
007420           PERFORM S01-READ-W01160                                        
007430       END-EVALUATE                                                       
007900     END-PERFORM                                                          
008100                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008902     OPEN INPUT  W01160                                                   
008910                 W01184                                                   
009001                                                                          
009010     OPEN OUTPUT W5128P                                                   
009100     SKIP2                                                                
009200     ACCEPT TODAYS-DATE  FROM DATE                                        
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009600 B-PROCESS SECTION.                                                       
009601                                                                          
009602     MOVE SLAG-IDDC       TO WS-IDDC                                      
009603     IF NDC-NA OR LDC-CN OR XDC-NON-VCC-OWNED                             
009604       MOVE CLAG-IDARTNR    TO UT-IDARTNR                                 
009605       MOVE CLAG-KDERS      TO UT-KDERS                                   
009606       MOVE CLAG-KDPSLLOC   TO UT-KDPSLLOC                                
009607       MOVE CLAG-KDPRODSL   TO UT-KDPRODSL                                
009608       MOVE CLAG-TIFINLV    TO UT-TIFINLV                                 
009609       MOVE SLAG-IDDC       TO UT-IDDC                                    
009610       MOVE SLAG-KVPB-REF   TO UT-KVPB-REF                                
009611       MOVE SLAG-KVEFRS     TO UT-KVEFRS                                  
009612       MOVE SLAG-KVLS       TO UT-KVLS                                    
009613       MOVE SLAG-PRAVCOST   TO UT-PRAVCOST                                
009614       PERFORM S11-WRITE-W5128P                                           
009615     END-IF                                                               
009616     .                                                                    
009620     EJECT                                                                
009700 Z-FINIT SECTION.                                                         
009701     CLOSE W01160                                                         
009702           W01184                                                         
009710           W5128P                                                         
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-READ-W01160  SECTION.                                                
010003     READ W01160 INTO IN-AREA1                                            
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO IN-AREA1                                       
010006        SET END-OF-W01160 TO TRUE                                         
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
010010        MOVE 'W5128PD1' TO POSTSUM-DDNAMN2                                
010012        MOVE SPACE     TO POSTSUM-TRANSTYP                                
010013        CALL POSTSUM USING POSTSUM-PARM                                   
010014     END-READ                                                             
010015     .                                                                    
010016     EJECT                                                                
010017 S02-READ-W01184  SECTION.                                                
010018     READ W01184 INTO IN-AREA2                                            
010019     AT END                                                               
010020        MOVE HIGH-VALUE TO IN-AREA2                                       
010021        SET END-OF-W01184 TO TRUE                                         
010022                                                                          
010023     NOT AT END                                                           
010024        MOVE 'W01184' TO POSTSUM-FDNAMN                                   
010025        MOVE 'W5128PD2' TO POSTSUM-DDNAMN2                                
010027        MOVE SPACE      TO POSTSUM-TRANSTYP                               
010028        CALL POSTSUM USING POSTSUM-PARM                                   
010029     END-READ                                                             
010030     .                                                                    
010101     EJECT                                                                
010102 S11-WRITE-W5128P SECTION.                                                
010103                                                                          
010104     WRITE UT-RECORD FROM UT-AREA                                         
010105                                                                          
010106     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
010107     MOVE 'W5128P' TO POSTSUM-FDNAMN                                      
010108     MOVE 'W5128PD3' TO POSTSUM-DDNAMN2                                   
010109     CALL POSTSUM USING POSTSUM-PARM                                      
010110     .                                                                    
010300     EJECT                                                                
