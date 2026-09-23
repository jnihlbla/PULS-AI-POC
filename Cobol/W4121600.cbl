000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4121600.                                                
000400*AUTHOR.         LASSI OLGRENER.                                          
000500*DATE-WRITTEN.   93/12/07.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        TAR BORT FRÅN VOR-REG ETT ÅR GAMLA POSTER OCH                    
001000*        LÄGGER TILL SISTA VECKANS POSTER.                                
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     EJECT                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- VOR-LEDTIDER                                               
002403     SELECT W41216                     ASSIGN TO W41216D1.                
002404     SKIP2                                                                
002405*          --- VOR-RADER+VÄRDE                                            
002406     SELECT W41215                     ASSIGN TO W41216D2.                
002407     SKIP2                                                                
002408*          --- VOR-REG LEDTIDER                                           
002409     SELECT WXTRD1I                    ASSIGN TO W41216D3.                
002410     SKIP2                                                                
002411*          --- VOR-REG RAD-VÄRDE                                          
002412     SELECT WXTRD2I                    ASSIGN TO W41216D4.                
002413     SKIP2                                                                
002414*          --- UT1                                                        
002415     SELECT WXTRD1U                    ASSIGN TO W41216D5.                
002416     SKIP2                                                                
002417*          --- UT2                                                        
002420     SELECT WXTRD2U                    ASSIGN TO W41216D6.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W41216                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005     SKIP2                                                                
003006*01  -COPY W41216      -L.                                                
003007     SKIP3                                                                
003008 FD  W41215                                                               
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003011     SKIP2                                                                
003012*01  -COPY W41215      -L.                                                
003013     SKIP3                                                                
003014 FD  WXTRD1I                                                              
003015     RECORDING       F                                                    
003016     BLOCK CONTAINS  0.                                                   
003017     SKIP2                                                                
003018*01  -COPY W41216      -L.                                                
003020     SKIP3                                                                
003021 FD  WXTRD2I                                                              
003022     RECORDING       F                                                    
003023     BLOCK CONTAINS  0.                                                   
003024     SKIP2                                                                
003025*01  -COPY W41215      -L.                                                
003026     SKIP3                                                                
003027 FD  WXTRD1U                                                              
003028     RECORDING       F                                                    
003029     BLOCK CONTAINS  0.                                                   
003030     SKIP2                                                                
003031*01  POST -COPY W41216 -PRE  UT1-  -L.                                    
003032     SKIP3                                                                
003033 FD  WXTRD2U                                                              
003034     RECORDING       F                                                    
003035     BLOCK CONTAINS  0.                                                   
003036     SKIP2                                                                
003040*01  POST -COPY W41215 -PRE  UT2-  -L.                                    
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003210                                                                          
003220*    -COPY WY2000W3                                                       
003300     SKIP2                                                                
003301                                                                          
003302                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W4121600'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  W-AKT-VECKA                 PIC 9(4)    VALUE ZERO.                  
003800 77  W-TAL                       PIC 9(4)    VALUE ZERO.                  
003801                                                                          
003802 77  W41216-EOF-SW               PIC X       VALUE 'N'.                   
003803     88  END-OF-W41216                       VALUE 'J'.                   
003804                                                                          
003805 77  W41215-EOF-SW               PIC X       VALUE 'N'.                   
003806     88  END-OF-W41215                       VALUE 'J'.                   
003807                                                                          
003808 77  WXTRD1-EOF-SW               PIC X       VALUE 'N'.                   
003809     88  END-OF-WXTRD1                       VALUE 'J'.                   
003810                                                                          
003811 77  WXTRD2-EOF-SW               PIC X       VALUE 'N'.                   
003820     88  END-OF-WXTRD2                       VALUE 'J'.                   
003900     EJECT                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004901     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005000     SKIP2                                                                
005100*    --- PARAMETRAR TILL ABEND                                            
005200                                                                          
005300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005901     EJECT                                                                
005902*    --- PARAMETRAR TILL POSTSUM                                          
005903*                                                                         
005910*01  -COPY W0005   -PRE  POSTSUM-                                         
006001     EJECT                                                                
006010*01  -COPY WDATAREA                                                       
006101     EJECT                                                                
006102 01  I16-AREA-START              PIC X(24)   VALUE                        
006103                                 'I16-AREA-START  '.                      
006104     SKIP2                                                                
006105                                                                          
006106*01  AREA -COPY W41216     -PRE I16-                                      
006107     EJECT                                                                
006108 01  I15-AREA-START              PIC X(24)   VALUE                        
006109                                 'I15-AREA-START  '.                      
006110     SKIP2                                                                
006111                                                                          
006112*01  AREA -COPY W41215     -PRE I15-                                      
006113     EJECT                                                                
006114 01  IN1-AREA-START              PIC X(24)   VALUE                        
006115                                 'IN1-AREA-START  '.                      
006116     SKIP2                                                                
006117                                                                          
006118*01  AREA -COPY W41216     -PRE IN1-                                      
006119     EJECT                                                                
006120 01  IN2-AREA-START              PIC X(24)   VALUE                        
006121                                 'IN2-AREA-START  '.                      
006122     SKIP2                                                                
006123                                                                          
006124*01  AREA -COPY W41215     -PRE IN2-                                      
006125     EJECT                                                                
006126 01  UT1-AREA-START              PIC X(24)   VALUE                        
006127                                 'UT1-AREA-START  '.                      
006128     SKIP2                                                                
006129                                                                          
006130*01  AREA -COPY W41216     -PRE UT1-                                      
006131     EJECT                                                                
006132 01  UT2-AREA-START              PIC X(24)   VALUE                        
006133                                 'UT2-AREA-START  '.                      
006134     SKIP2                                                                
006135                                                                          
006140*01  AREA -COPY W41215     -PRE UT2-                                      
006200     EJECT                                                                
006300 PROCEDURE DIVISION.                                                      
006600                                                                          
006700     PERFORM A-INIT                                                       
006811                                                                          
006820     PERFORM S03-LAES-WXTRD1                                              
006900     PERFORM UNTIL END-OF-WXTRD1                                          
007000       PERFORM B-SKAPA-NY-WXTRD1                                          
007510       PERFORM S03-LAES-WXTRD1                                            
007600     END-PERFORM                                                          
007601                                                                          
007602     PERFORM S01-LAES-W41216                                              
007603     PERFORM UNTIL END-OF-W41216                                          
007604       MOVE I16-W41216 TO UT1-W41216                                      
007605       PERFORM S11-SKRIV-WXTRD1                                           
007606       PERFORM S01-LAES-W41216                                            
007700     END-PERFORM                                                          
007701                                                                          
007705     PERFORM S04-LAES-WXTRD2                                              
007706     PERFORM UNTIL END-OF-WXTRD2                                          
007707       PERFORM C-SKAPA-NY-WXTRD2                                          
007710       PERFORM S04-LAES-WXTRD2                                            
007720     END-PERFORM                                                          
007800                                                                          
007810     PERFORM S02-LAES-W41215                                              
007811     PERFORM UNTIL END-OF-W41215                                          
007812       MOVE I15-W41215 TO UT2-W41215                                      
007813       PERFORM S12-SKRIV-WXTRD2                                           
007820       PERFORM S02-LAES-W41215                                            
007830     END-PERFORM                                                          
007900                                                                          
008000     PERFORM Z-FINIT                                                      
008100                                                                          
008200     MOVE ZERO TO RETURN-CODE                                             
008300     GOBACK                                                               
008400     .                                                                    
008500     EJECT                                                                
008600 A-INIT SECTION.                                                          
008701                                                                          
008702     OPEN INPUT  W41216                                                   
008703                 W41215                                                   
008704                 WXTRD1I                                                  
008710                 WXTRD2I                                                  
008801                                                                          
008802     OPEN OUTPUT WXTRD1U                                                  
008810                 WXTRD2U                                                  
008900     SKIP2                                                                
009100     MOVE 'IDAG' TO DAT-KDDATFORM                                         
009101                                                                          
009102     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
009103                     DAT-O-TIDATUM DAT-KDSVAR                             
009104                                                                          
009105     IF DAT-KDSVAR-FEL                                                    
009106        MOVE 'FEL FRÅN WDATKONV I A-SECTION'                              
009107                          TO FELTEXT                                      
009108        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
009109     END-IF                                                               
009110     MOVE DAT-TIAAVV-GRP  TO W-AKT-VECKA                                  
009120     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009200     .                                                                    
009300     EJECT                                                                
009310 B-SKAPA-NY-WXTRD1 SECTION.                                               
009311                                                                          
009313     MOVE W-AKT-VECKA TO TMP1-YYWW                                        
009314     MOVE IN1-TIAAVV  TO TMP2-YYWW                                        
009315     PERFORM WY2000P3                                                     
009316     COMPUTE W-TAL = TMP1-YYWW - TMP2-YYWW                                
009317     IF W-TAL < 99                                                        
009318       MOVE IN1-W41216 TO UT1-W41216                                      
009319       PERFORM S11-SKRIV-WXTRD1                                           
009320     END-IF                                                               
009321     .                                                                    
009330     EJECT                                                                
009340 C-SKAPA-NY-WXTRD2 SECTION.                                               
009350                                                                          
009352     MOVE W-AKT-VECKA TO TMP1-YYWW                                        
009353     MOVE IN2-TIAAVV  TO TMP2-YYWW                                        
009354     PERFORM WY2000P3                                                     
009360     COMPUTE W-TAL = TMP1-YYWW - TMP2-YYWW                                
009370     IF W-TAL < 99                                                        
009380       MOVE IN2-W41215 TO UT2-W41215                                      
009390       PERFORM S12-SKRIV-WXTRD2                                           
009391     END-IF                                                               
009392     .                                                                    
009393     EJECT                                                                
009400 Z-FINIT SECTION.                                                         
009501     CLOSE W41216                                                         
009502           W41215                                                         
009503           WXTRD1I                                                        
009504           WXTRD2I                                                        
009505           WXTRD1U                                                        
009510           WXTRD2U                                                        
009601     SKIP2                                                                
009602     MOVE 'S' TO POSTSUM-OPKOD                                            
009610     CALL POSTSUM USING POSTSUM-PARM                                      
009700     .                                                                    
009801     EJECT                                                                
009802 S01-LAES-W41216  SECTION.                                                
009803     SKIP2                                                                
009804     READ W41216 INTO I16-AREA                                            
009805     AT END                                                               
009807        SET END-OF-W41216 TO TRUE                                         
009808                                                                          
009809     NOT AT END                                                           
009810        MOVE 'W41216' TO POSTSUM-FDNAMN                                   
009811        MOVE 'W41216D1' TO POSTSUM-DDNAMN2                                
009812        MOVE 'I16'     TO POSTSUM-TRANSTYP                                
009813        CALL POSTSUM USING POSTSUM-PARM                                   
009814     END-READ                                                             
009815     .                                                                    
009816     EJECT                                                                
009817 S02-LAES-W41215  SECTION.                                                
009818     SKIP2                                                                
009819     READ W41215 INTO I15-AREA                                            
009820     AT END                                                               
009822        SET END-OF-W41215 TO TRUE                                         
009823                                                                          
009824     NOT AT END                                                           
009825        MOVE 'W41215' TO POSTSUM-FDNAMN                                   
009826        MOVE 'W41216D2' TO POSTSUM-DDNAMN2                                
009827        MOVE 'I15'      TO POSTSUM-TRANSTYP                               
009828        CALL POSTSUM USING POSTSUM-PARM                                   
009829     END-READ                                                             
009830     .                                                                    
009831     EJECT                                                                
009832 S03-LAES-WXTRD1  SECTION.                                                
009833     SKIP2                                                                
009834     READ WXTRD1I INTO IN1-AREA                                           
009835     AT END                                                               
009837        SET END-OF-WXTRD1 TO TRUE                                         
009838                                                                          
009839     NOT AT END                                                           
009840        MOVE 'WXTRD1I' TO POSTSUM-FDNAMN                                  
009841        MOVE 'W41216D3' TO POSTSUM-DDNAMN2                                
009842        MOVE 'IN1'      TO POSTSUM-TRANSTYP                               
009844        CALL POSTSUM USING POSTSUM-PARM                                   
009845     END-READ                                                             
009846     .                                                                    
009847     EJECT                                                                
009848 S04-LAES-WXTRD2  SECTION.                                                
009849     SKIP2                                                                
009850     READ WXTRD2I INTO IN2-AREA                                           
009851     AT END                                                               
009853        SET END-OF-WXTRD2 TO TRUE                                         
009854                                                                          
009855     NOT AT END                                                           
009856        MOVE 'WXTRD2I' TO POSTSUM-FDNAMN                                  
009857        MOVE 'W41216D4' TO POSTSUM-DDNAMN2                                
009858        MOVE 'IN2'      TO POSTSUM-TRANSTYP                               
009860        CALL POSTSUM USING POSTSUM-PARM                                   
009861     END-READ                                                             
009870     .                                                                    
009901     EJECT                                                                
009902 S11-SKRIV-WXTRD1 SECTION.                                                
009903     SKIP2                                                                
009904     WRITE UT1-POST FROM UT1-AREA                                         
009905                                                                          
009906     MOVE 'UT1' TO POSTSUM-TRANSTYP                                       
009907     MOVE 'WXTRD1U' TO POSTSUM-FDNAMN                                     
009908     MOVE 'W41216D5' TO POSTSUM-DDNAMN2                                   
009909     CALL POSTSUM USING POSTSUM-PARM                                      
009910     .                                                                    
009911     EJECT                                                                
009912 S12-SKRIV-WXTRD2 SECTION.                                                
009913     SKIP2                                                                
009914     WRITE UT2-POST FROM UT2-AREA                                         
009915                                                                          
009917     MOVE 'UT2' TO POSTSUM-TRANSTYP                                       
009918     MOVE 'WXTRD2U' TO POSTSUM-FDNAMN                                     
009919     MOVE 'W41216D6' TO POSTSUM-DDNAMN2                                   
009920     CALL POSTSUM USING POSTSUM-PARM                                      
009930     .                                                                    
009940     -COPY WY2000P3                                                       
009950     EJECT                                                                
