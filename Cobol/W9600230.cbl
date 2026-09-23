000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9600230.                                                
000400 AUTHOR.         KARIN OLSSON.                                            
000500 DATE-WRITTEN.   92/12/18.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAM FÖR ATT KONTROLLERA KOMMANDO SOM SKRIVITS PÅ             
001100*        KOMMANDORADEN.                                                   
001200*                                                                         
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700     EJECT                                                                
001800 DATA DIVISION.                                                           
001900     SKIP3                                                                
002000 WORKING-STORAGE SECTION.                                                 
002100     SKIP2                                                                
002101                                                                          
002110*    -- CHECKED BY WY2000                                                 
002200 77  IDPGM                       PIC X(8)    VALUE 'W9600230'.            
002300 77  JA                          PIC X       VALUE 'J'.                   
002400 77  NEJ                         PIC X       VALUE 'N'.                   
002600     EJECT                                                                
002700 01  DYNAMISKA-SUBPROGRAM.                                                
002800*                                                                         
002900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
003000     03  ISPLINK                 PIC X(8)    VALUE 'ISPLINK'.             
003100     SKIP2                                                                
003200* --- PARAMETRAR TILL ABEND                                               
003300                                                                          
003400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
003500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
003600     SKIP2                                                                
003700* --- PARAMETRAR TILL ISPLINK                                             
003800 77  ISP-VPUT                    PIC X(8)    VALUE 'VPUT    '.            
003901 77  ISP-VDEFINE                 PIC X(8)    VALUE 'VDEFINE '.            
003910 77  ISP-VDELETE                 PIC X(8)    VALUE 'VDELETE '.            
003920 77  ISP-VERASE                  PIC X(8)    VALUE 'VERASE  '.            
003930 77  ISP-VRESET                  PIC X(8)    VALUE 'VRESET  '.            
004000 77  ISP-SHARED                  PIC X(8)    VALUE 'SHARED  '.            
004100 77  CHAR                        PIC X(8)    VALUE 'CHAR    '.            
004200 77  PACK                        PIC X(8)    VALUE 'PACK    '.            
004300 77  VDEFINE-OPT                 PIC X(16)                                
004400                              VALUE '(COPY NOBSCAN)'.                     
004500     EJECT                                                                
005000 01  TOPRADNR                    PIC 9(5)    COMP-3.                      
005100 01  N-TOPRADNR                  PIC X(8)    VALUE 'TOPRADNR'.            
005200 01  L-TOPRADNR                  PIC S9(9)   COMP VALUE +3.               
005300     SKIP2                                                                
005400 01  MAXRADNR                    PIC 9(5)    COMP-3.                      
005500 01  N-MAXRADNR                  PIC X(8)    VALUE 'MAXRADNR'.            
005600 01  L-MAXRADNR                  PIC S9(9)   COMP VALUE +3.               
005900     SKIP2                                                                
006000 01  START2                      PIC 9(5)    COMP-3.                      
006100 01  N-START2                    PIC X(8)    VALUE 'START2'.              
006200 01  L-START                     PIC S9(9)   COMP VALUE +3.               
006300     SKIP2                                                                
006400 01  START3                      PIC 9(5)    COMP-3.                      
006500 01  N-START3                    PIC X(8)    VALUE 'START3'.              
006600     SKIP2                                                                
006700 01  START4                      PIC 9(5)    COMP-3.                      
006800 01  N-START4                    PIC X(8)    VALUE 'START4'.              
006900     SKIP2                                                                
007000 01  START5                      PIC 9(5)    COMP-3.                      
007100 01  N-START5                    PIC X(8)    VALUE 'START5'.              
007200     SKIP2                                                                
007300 01  START6                      PIC 9(5)    COMP-3.                      
007400 01  N-START6                    PIC X(8)    VALUE 'START6'.              
007401     SKIP2                                                                
007410 01  START7                      PIC 9(5)    COMP-3.                      
007420 01  N-START7                    PIC X(8)    VALUE 'START7'.              
007500     SKIP2                                                                
007510 01  START8                      PIC 9(5)    COMP-3.                      
007520 01  N-START8                    PIC X(8)    VALUE 'START8'.              
007530     SKIP2                                                                
007600 01  MAX1                        PIC 9(5)    COMP-3.                      
007700 01  N-MAX1                      PIC X(8)    VALUE 'MAX1'.                
007800 01  L-MAX                       PIC S9(9)   COMP VALUE +3.               
007900     SKIP2                                                                
008000 01  MAX2                        PIC 9(5)    COMP-3.                      
008100 01  N-MAX2                      PIC X(8)    VALUE 'MAX2'.                
008200     SKIP2                                                                
008300 01  MAX3                        PIC 9(5)    COMP-3.                      
008400 01  N-MAX3                      PIC X(8)    VALUE 'MAX3'.                
008500     SKIP2                                                                
008600 01  MAX4                        PIC 9(5)    COMP-3.                      
008700 01  N-MAX4                      PIC X(8)    VALUE 'MAX4'.                
008800     SKIP2                                                                
008900 01  MAX5                        PIC 9(5)    COMP-3.                      
009000 01  N-MAX5                      PIC X(8)    VALUE 'MAX5'.                
009100     SKIP2                                                                
009200 01  MAX6                        PIC 9(5)    COMP-3.                      
009300 01  N-MAX6                      PIC X(8)    VALUE 'MAX6'.                
009400     SKIP2                                                                
009410 01  MAX7                        PIC 9(5)    COMP-3.                      
009420 01  N-MAX7                      PIC X(8)    VALUE 'MAX7'.                
009430     SKIP2                                                                
009440 01  MAX8                        PIC 9(5)    COMP-3.                      
009450 01  N-MAX8                      PIC X(8)    VALUE 'MAX8'.                
009460     SKIP2                                                                
009500 01  SECT                        PIC X(1).                                
009600 01  N-SECT                      PIC X(8)    VALUE 'SECT'.                
009700 01  L-SECT                      PIC S9(9)   COMP VALUE +1.               
009710     SKIP2                                                                
009711 01  MAXSECT                     PIC X(1).                                
009712 01  N-MAXSECT                   PIC X(8)    VALUE 'MAXSECT'.             
009713 01  L-MAXSECT                   PIC S9(9)   COMP VALUE +1.               
009714     SKIP2                                                                
009715 01  NYSECT                      PIC X(1).                                
009716 01  N-NYSECT                    PIC X(8)    VALUE 'NYSECT'.              
009717 01  L-NYSECT                    PIC S9(9)   COMP VALUE +1.               
009718     SKIP2                                                                
009720 01  ZCMD                        PIC X(40).                               
009730 01  N-ZCMD                      PIC X(8)    VALUE 'ZCMD'.                
009740 01  L-ZCMD                      PIC S9(9)   COMP VALUE +40.              
009741     SKIP2                                                                
009750 01  RKOD                        PIC S9(4)   COMP.                        
009800     EJECT                                                                
009900 LINKAGE SECTION.                                                         
010000                                                                          
010100 01  FUNCTION-CODE               PIC 9(6)    COMP.                        
010200 01  DDAPTR                      PIC S9(8)   COMP SYNC.                   
010300                                                                          
010400     EJECT                                                                
010500 PROCEDURE DIVISION USING FUNCTION-CODE DDAPTR.                           
010600     SKIP2                                                                
010700     IF FUNCTION-CODE = 10                                                
010800       MOVE 4 TO RKOD                                                     
010900     ELSE                                                                 
010910       CALL ISPLINK USING ISP-VDELETE N-SECT                              
010920       CALL ISPLINK USING ISP-VDELETE N-MAXSECT                           
010930                                                                          
011000       CALL ISPLINK USING ISP-VDEFINE N-SECT SECT CHAR                    
011100                           L-SECT VDEFINE-OPT                             
011200       IF RETURN-CODE > 0                                                 
011300         DISPLAY 'HITTAR INTE SECT'                                       
011410         PERFORM S99-ABEND                                                
011500       END-IF                                                             
011600                                                                          
011602       CALL ISPLINK USING ISP-VDEFINE N-MAXSECT MAXSECT CHAR              
011603                           L-MAXSECT VDEFINE-OPT                          
011604       IF RETURN-CODE > 0                                                 
011605         DISPLAY 'HITTAR INTE MAXSECT'                                    
011606         PERFORM S99-ABEND                                                
011607       END-IF                                                             
011608                                                                          
011611       CALL ISPLINK USING ISP-VDEFINE N-ZCMD ZCMD CHAR                    
011620                           L-ZCMD VDEFINE-OPT                             
011630       IF RETURN-CODE > 0                                                 
011640         DISPLAY 'HITTAR INTE ZCMD'                                       
011650         PERFORM S99-ABEND                                                
011660       END-IF                                                             
011670                                                                          
011700       IF ZCMD = '1' OR '2' OR '3' OR '4' OR '5'                          
011701                     OR '6' OR '7' OR '8'                                 
011702         MOVE 0 TO RKOD                                                   
011710         IF ZCMD NOT = SECT                                               
011720           MOVE ZCMD TO SECT                                              
011721           IF SECT <= MAXSECT                                             
011800             PERFORM A-NYA-VAERDEN                                        
011801           ELSE                                                           
011802             MOVE 4 TO RKOD                                               
011803           END-IF                                                         
011810         END-IF                                                           
012000       ELSE                                                               
012100         MOVE 4 TO RKOD                                                   
012200       END-IF                                                             
012220       CALL ISPLINK USING ISP-VERASE  N-ZCMD ISP-SHARED                   
012230       CALL ISPLINK USING ISP-VDELETE N-ZCMD                              
012300     END-IF                                                               
012310                                                                          
012320     MOVE RKOD TO RETURN-CODE                                             
012400                                                                          
012500     GOBACK                                                               
012600     .                                                                    
012700 A-NYA-VAERDEN   SECTION.                                                 
012800     SKIP2                                                                
012820     CALL ISPLINK USING ISP-VDELETE N-START2                              
012830     CALL ISPLINK USING ISP-VDELETE N-START3                              
012840     CALL ISPLINK USING ISP-VDELETE N-START4                              
012850     CALL ISPLINK USING ISP-VDELETE N-START5                              
012860     CALL ISPLINK USING ISP-VDELETE N-START6                              
012870     CALL ISPLINK USING ISP-VDELETE N-START7                              
012871     CALL ISPLINK USING ISP-VDELETE N-START8                              
012880                                                                          
012890     CALL ISPLINK USING ISP-VDELETE N-MAX1                                
012891     CALL ISPLINK USING ISP-VDELETE N-MAX2                                
012892     CALL ISPLINK USING ISP-VDELETE N-MAX3                                
012893     CALL ISPLINK USING ISP-VDELETE N-MAX4                                
012894     CALL ISPLINK USING ISP-VDELETE N-MAX5                                
012895     CALL ISPLINK USING ISP-VDELETE N-MAX6                                
012896     CALL ISPLINK USING ISP-VDELETE N-MAX7                                
012897     CALL ISPLINK USING ISP-VDELETE N-MAX8                                
012898                                                                          
012899     CALL ISPLINK USING ISP-VDELETE N-NYSECT                              
012900     CALL ISPLINK USING ISP-VDELETE N-TOPRADNR                            
012901     CALL ISPLINK USING ISP-VDELETE N-MAXRADNR                            
012902                                                                          
012910     CALL ISPLINK USING ISP-VDEFINE N-NYSECT NYSECT CHAR                  
013000                         L-NYSECT VDEFINE-OPT                             
013100     IF RETURN-CODE > 8                                                   
013200       DISPLAY 'KAN INTE VDEFFA NYSECT'                                   
013300       PERFORM S99-ABEND                                                  
013400     END-IF                                                               
013500                                                                          
013600     CALL ISPLINK USING ISP-VDEFINE N-TOPRADNR TOPRADNR PACK              
013700                         L-TOPRADNR VDEFINE-OPT                           
013800     IF RETURN-CODE > 0                                                   
013900       DISPLAY 'KAN INTE VDEFFA TOPRADNR'                                 
014010       PERFORM S99-ABEND                                                  
014100     END-IF                                                               
014200                                                                          
014300     CALL ISPLINK USING ISP-VDEFINE N-MAXRADNR MAXRADNR PACK              
014400                         L-MAXRADNR VDEFINE-OPT                           
014500     IF RETURN-CODE > 0                                                   
014600       DISPLAY 'KAN INTE VDEFFA MAXRADNR'                                 
014710       PERFORM S99-ABEND                                                  
014800     END-IF                                                               
014900                                                                          
015000     CALL ISPLINK USING ISP-VDEFINE N-START2 START2 PACK                  
015100                         L-START VDEFINE-OPT                              
015200     IF RETURN-CODE > 0                                                   
015300       DISPLAY 'KAN INTE VDEFFA START2'                                   
015410       PERFORM S99-ABEND                                                  
015500     END-IF                                                               
015600                                                                          
015700     CALL ISPLINK USING ISP-VDEFINE N-START3 START3 PACK                  
015800                         L-START VDEFINE-OPT                              
015900     IF RETURN-CODE > 0                                                   
016000       DISPLAY 'KAN INTE VDEFFA START3'                                   
016110       PERFORM S99-ABEND                                                  
016200     END-IF                                                               
016300                                                                          
016400     CALL ISPLINK USING ISP-VDEFINE N-START4 START4 PACK                  
016500                         L-START VDEFINE-OPT                              
016600     IF RETURN-CODE > 0                                                   
016700       DISPLAY 'KAN INTE VDEFFA START4'                                   
016810       PERFORM S99-ABEND                                                  
016900     END-IF                                                               
017000                                                                          
017100     CALL ISPLINK USING ISP-VDEFINE N-START5 START5 PACK                  
017200                         L-START VDEFINE-OPT                              
017300     IF RETURN-CODE > 0                                                   
017400       DISPLAY 'KAN INTE VDEFFA START5'                                   
017510       PERFORM S99-ABEND                                                  
017600     END-IF                                                               
017700                                                                          
017800     CALL ISPLINK USING ISP-VDEFINE N-START6 START6 PACK                  
017900                         L-START VDEFINE-OPT                              
018000     IF RETURN-CODE > 0                                                   
018100       DISPLAY 'KAN INTE VDEFFA START6'                                   
018210       PERFORM S99-ABEND                                                  
018300     END-IF                                                               
018400                                                                          
018410     CALL ISPLINK USING ISP-VDEFINE N-START7 START7 PACK                  
018420                         L-START VDEFINE-OPT                              
018430     IF RETURN-CODE > 0                                                   
018440       DISPLAY 'KAN INTE VDEFFA START7'                                   
018450       PERFORM S99-ABEND                                                  
018460     END-IF                                                               
018470                                                                          
018480     CALL ISPLINK USING ISP-VDEFINE N-START8 START8 PACK                  
018490                         L-START VDEFINE-OPT                              
018491     IF RETURN-CODE > 0                                                   
018492       DISPLAY 'KAN INTE VDEFFA START8'                                   
018493       PERFORM S99-ABEND                                                  
018494     END-IF                                                               
018495                                                                          
018500     CALL ISPLINK USING ISP-VDEFINE N-MAX1 MAX1 PACK                      
018600                         L-MAX VDEFINE-OPT                                
018700     IF RETURN-CODE > 0                                                   
018800       DISPLAY 'KAN INTE VDEFFA MAX1'                                     
018910       PERFORM S99-ABEND                                                  
019000     END-IF                                                               
019100                                                                          
019200     CALL ISPLINK USING ISP-VDEFINE N-MAX2 MAX2 PACK                      
019300                         L-MAX VDEFINE-OPT                                
019400     IF RETURN-CODE > 0                                                   
019500       DISPLAY 'KAN INTE VDEFFA MAX2'                                     
019610       PERFORM S99-ABEND                                                  
019700     END-IF                                                               
019800                                                                          
019900     CALL ISPLINK USING ISP-VDEFINE N-MAX3 MAX3 PACK                      
020000                         L-MAX VDEFINE-OPT                                
020100     IF RETURN-CODE > 0                                                   
020200       DISPLAY 'KAN INTE VDEFFA MAX3'                                     
020310       PERFORM S99-ABEND                                                  
020400     END-IF                                                               
020500                                                                          
020600     CALL ISPLINK USING ISP-VDEFINE N-MAX4 MAX4 PACK                      
020700                         L-MAX VDEFINE-OPT                                
020800     IF RETURN-CODE > 0                                                   
020900       DISPLAY 'KAN INTE VDEFFA MAX4'                                     
021010       PERFORM S99-ABEND                                                  
021100     END-IF                                                               
021200                                                                          
021300     CALL ISPLINK USING ISP-VDEFINE N-MAX5 MAX5 PACK                      
021400                         L-MAX VDEFINE-OPT                                
021500     IF RETURN-CODE > 0                                                   
021600       DISPLAY 'KAN INTE VDEFFA MAX5'                                     
021710       PERFORM S99-ABEND                                                  
021800     END-IF                                                               
021900                                                                          
022000     CALL ISPLINK USING ISP-VDEFINE N-MAX6 MAX6 PACK                      
022100                         L-MAX VDEFINE-OPT                                
022200     IF RETURN-CODE > 0                                                   
022300       DISPLAY 'KAN INTE VDEFFA MAX6'                                     
022410       PERFORM S99-ABEND                                                  
022500     END-IF                                                               
022600                                                                          
022610     CALL ISPLINK USING ISP-VDEFINE N-MAX7 MAX7 PACK                      
022620                         L-MAX VDEFINE-OPT                                
022630     IF RETURN-CODE > 0                                                   
022640       DISPLAY 'KAN INTE VDEFFA MAX7'                                     
022650       PERFORM S99-ABEND                                                  
022660     END-IF                                                               
022670                                                                          
022680     CALL ISPLINK USING ISP-VDEFINE N-MAX8 MAX8 PACK                      
022690                         L-MAX VDEFINE-OPT                                
022700     IF RETURN-CODE > 0                                                   
022800       DISPLAY 'KAN INTE VDEFFA MAX8'                                     
022810       PERFORM S99-ABEND                                                  
022820     END-IF                                                               
022830                                                                          
022900     EVALUATE TRUE                                                        
023000       WHEN SECT = '1'                                                    
023100         MOVE ZERO TO TOPRADNR                                            
023200         MOVE MAX1 TO MAXRADNR                                            
023300                                                                          
023400       WHEN SECT = '2'                                                    
023500         MOVE START2 TO TOPRADNR                                          
023600         MOVE MAX2 TO MAXRADNR                                            
023700                                                                          
023800       WHEN SECT = '3'                                                    
023900         MOVE START3 TO TOPRADNR                                          
024000         MOVE MAX3 TO MAXRADNR                                            
024100                                                                          
024200       WHEN SECT = '4'                                                    
024300         MOVE START4 TO TOPRADNR                                          
024400         MOVE MAX4 TO MAXRADNR                                            
024500                                                                          
024600       WHEN SECT = '5'                                                    
024700         MOVE START5 TO TOPRADNR                                          
024800         MOVE MAX5 TO MAXRADNR                                            
024900                                                                          
025000       WHEN SECT = '6'                                                    
025100         MOVE START6 TO TOPRADNR                                          
025200         MOVE MAX6 TO MAXRADNR                                            
025400                                                                          
025410       WHEN SECT = '7'                                                    
025420         MOVE START7 TO TOPRADNR                                          
025430         MOVE MAX7 TO MAXRADNR                                            
025440                                                                          
025450       WHEN SECT = '8'                                                    
025460         MOVE START8 TO TOPRADNR                                          
025470         MOVE MAX8 TO MAXRADNR                                            
025480                                                                          
025500       WHEN OTHER                                                         
025600         DISPLAY 'OKÄND SEKTION ' SECT                                    
025700         PERFORM S99-ABEND                                                
025800     END-EVALUATE                                                         
026100                                                                          
026700     MOVE 'J' TO NYSECT                                                   
027300                                                                          
027352     CALL ISPLINK USING ISP-VPUT N-NYSECT ISP-SHARED                      
027353     IF RETURN-CODE > 0                                                   
027354       DISPLAY 'KAN INTE VPUTTA NYSECT'                                   
027355       PERFORM S99-ABEND                                                  
027356     END-IF                                                               
027357                                                                          
027358     CALL ISPLINK USING ISP-VPUT N-SECT ISP-SHARED                        
027359     IF RETURN-CODE > 0                                                   
027360       DISPLAY 'KAN INTE VPUTTA SECT'                                     
027361       PERFORM S99-ABEND                                                  
027362     END-IF                                                               
027370                                                                          
027400     CALL ISPLINK USING ISP-VPUT N-TOPRADNR ISP-SHARED                    
027500     IF RETURN-CODE > 0                                                   
027600       DISPLAY 'KAN INTE VPUTTA TOPRADNR'                                 
027700       PERFORM S99-ABEND                                                  
027800     END-IF                                                               
027900                                                                          
028000     CALL ISPLINK USING ISP-VPUT N-MAXRADNR ISP-SHARED                    
028100     IF RETURN-CODE > 0                                                   
028200       DISPLAY 'KAN INTE VPUTTA MAXRADNR'                                 
028300       PERFORM S99-ABEND                                                  
028400     END-IF                                                               
028500     .                                                                    
028600 S99-ABEND   SECTION.                                                     
028700     SKIP2                                                                
028800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
028900     .                                                                    
