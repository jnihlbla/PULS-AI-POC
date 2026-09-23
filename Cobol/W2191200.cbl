000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2191200.                                                
000400 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500 DATE-WRITTEN.   APRIL 2003.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
000910*        MATCHAR KAMPANJER FRÅN QW90                                      
000920*        AKTUELL VECKA MOT FÖREGÅENDE VECKAS                              
000930*        (POSTTYP = Q99 / KAMPANJINFO)                                    
001100*                                                                         
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
003200     SELECT W21910NY                   ASSIGN TO W21912D1.                
003300     SKIP2                                                                
003330     SELECT W21910GAM                  ASSIGN TO W21912D2.                
003810*                                                                         
003820     SELECT W21912UT                   ASSIGN TO W21912D3.                
003830                                                                          
003923                                                                          
003930     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
005000 FD  W21910NY                                                             
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST     -COPY W21910  -PRE W21910NY-    -L                          
005413     SKIP3                                                                
005414 FD  W21910GAM                                                            
005415     RECORDING       F                                                    
005416     BLOCK CONTAINS  0.                                                   
005417                                                                          
005418*01  POST     -COPY W21910  -PRE W21910GAM-    -L                         
005419     SKIP3                                                                
005459 FD  W21912UT                                                             
005460     RECORDING       F                                                    
005461     BLOCK CONTAINS  0.                                                   
005470                                                                          
005480*01  POST -COPY W21912 -PRE  W21912UT-     -L.                            
005490                                                                          
006700     EJECT                                                                
006800 WORKING-STORAGE SECTION.                                                 
006900     SKIP2                                                                
006901                                                                          
006910*    -- CHECKED BY WY2000                                                 
007000 77  IDPGM                       PIC X(8)    VALUE 'W2191200'.            
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
008000     SKIP2                                                                
008500                                                                          
008510 01 WS-IDNY.                                                              
008520  03 WS-IDNY-IDARTNR            PIC S9(9)  VALUE ZERO.                    
008530  03 WS-IDNY-IDDC               PIC X(2)   VALUE SPACE.                   
008540 01 WS-IDGAM.                                                             
008550  03 WS-IDGAM-IDARTNR           PIC S9(9)  VALUE ZERO.                    
008560  03 WS-IDGAM-IDDC              PIC X(2)   VALUE SPACE.                   
008592 01 WS-ANTAL-W21910NY           PIC 9(9)   VALUE ZERO.                    
008593 01 WS-ANTAL-W21910GAM          PIC 9(9)   VALUE ZERO.                    
008595 01 WS-ANTAL-W21912UT           PIC 9(9)   VALUE ZERO.                    
008609 01 WS-ANTAL-DISPLAY            PIC 9(9)   VALUE ZERO.                    
008611                                                                          
008612                                                                          
008620 01  FELTEXT.                                                             
008700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008900                                                                          
009000 77  W21910NY-EOF-SW             PIC X       VALUE 'N'.                   
009100     88  END-OF-W21910NY                     VALUE 'J'.                   
009110                                                                          
009120 77  W21910GAM-EOF-SW            PIC X       VALUE 'N'.                   
009130     88  END-OF-W21910GAM                    VALUE 'J'.                   
009131     EJECT                                                                
009132*      --- VALID IDDC CODES                                               
009133*                                                                         
009134*01    -COPY WWDC99                                                       
009140                                                                          
009200     EJECT                                                                
009300 01  DYNAMISKA-SUBPROGRAM.                                                
009400*                                                                         
009500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009900     EJECT                                                                
010000*    ---- PARAMETRAR TILL WDATKONV                                        
010100 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
010200*01  -COPY WDATAREA                                                       
010400*    --- PARAMETRAR TILL POSTSUM                                          
010500*                                                                         
010600*01  -COPY W0005   -PRE  POSTSUM-                                         
010700     EJECT                                                                
011214 01  W21910NY-AREA-START         PIC X(24)   VALUE                        
011215                                 'W21910NY-AREA-START  '.                 
011216     SKIP2                                                                
011217                                                                          
011218 01  W21910NY-AREA.                                                       
011220         05  -COPY W21910   -PRE W21910NY-                                
011250                                                                          
011260     EJECT                                                                
011270 01  W21910GAM-AREA-START        PIC X(24)   VALUE                        
011280                                 'W21910GAM-AREA-START  '.                
011290     SKIP2                                                                
011291                                                                          
011292 01  W21910GAM-AREA.                                                      
011293         05  -COPY W21910   -PRE W21910GAM-                               
011294                                                                          
011295     EJECT                                                                
011296 01  W21912UT-AREA-START         PIC X(24)   VALUE                        
011297                                 'W21912UT-AREA-START  '.                 
011298     SKIP2                                                                
011299                                                                          
011300 01  W21912UT-AREA.                                                       
011310         05  -COPY W21912   -PRE W21912UT-                                
011320                                                                          
011400     EJECT                                                                
012300                                                                          
017200     EJECT                                                                
017300 LINKAGE SECTION.                                                         
017400                                                                          
018600 PROCEDURE DIVISION.                                                      
018900                                                                          
019000     SKIP2                                                                
019100     PERFORM A-INIT                                                       
019110                                                                          
019200     PERFORM S01-LAES-W21910NY                                            
019210     PERFORM S02-LAES-W21910GAM                                           
019300                                                                          
019400     PERFORM UNTIL END-OF-W21910NY                                        
019410     AND           END-OF-W21910GAM                                       
019500       PERFORM B-BEARBETA                                                 
019700     END-PERFORM                                                          
019710                                                                          
019900     PERFORM Z-FINIT                                                      
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 A-INIT SECTION.                                                          
020600                                                                          
020700     OPEN INPUT  W21910NY                                                 
020810                 W21910GAM                                                
020820                                                                          
021110     OPEN OUTPUT W21912UT                                                 
021196                                                                          
021200                                                                          
021400     .                                                                    
021500     EJECT                                                                
021600 B-BEARBETA SECTION.                                                      
021700                                                                          
021710     IF END-OF-W21910GAM                                                  
021720     OR W21910GAM-IDKAMP > W21910NY-IDKAMP                                
021730                                                                          
021731       MOVE W21910NY-IDPTYP  TO W21912UT-IDPTYP                           
021732       MOVE 'N'              TO W21912UT-KDSTATUS-KAMP                    
021733       MOVE W21910NY-IDKAMP  TO W21912UT-IDKAMP                           
021734       MOVE W21910NY-TISTADAT-KAMP                                        
021735                             TO W21912UT-TISTADAT-KAMP                    
021736       MOVE W21910NY-TISTODAT-KAMP                                        
021737                             TO W21912UT-TISTODAT-KAMP                    
021738       MOVE W21910NY-KVKAMP-CARS                                          
021739                             TO W21912UT-KVKAMP-CARS                      
021740       PERFORM S11-SKRIV-W21912UT                                         
021750       PERFORM S01-LAES-W21910NY                                          
021760                                                                          
021770     ELSE                                                                 
021780                                                                          
021790       IF END-OF-W21910NY                                                 
021800       OR W21910NY-IDKAMP > W21910GAM-IDKAMP                              
021813                                                                          
021814         MOVE W21910GAM-IDPTYP                                            
021815                             TO W21912UT-IDPTYP                           
021816         MOVE 'D'            TO W21912UT-KDSTATUS-KAMP                    
021817         MOVE W21910GAM-IDKAMP                                            
021818                             TO W21912UT-IDKAMP                           
021819         MOVE W21910GAM-TISTADAT-KAMP                                     
021820                             TO W21912UT-TISTADAT-KAMP                    
021821         MOVE W21910GAM-TISTODAT-KAMP                                     
021822                             TO W21912UT-TISTODAT-KAMP                    
021823         MOVE W21910GAM-KVKAMP-CARS                                       
021824                             TO W21912UT-KVKAMP-CARS                      
021825         PERFORM S11-SKRIV-W21912UT                                       
021826         PERFORM S02-LAES-W21910GAM                                       
021827                                                                          
021828       ELSE                                                               
021829*                                                                         
021830* TRÄFF                                                                   
021831*                                                                         
021833         IF NOT (W21910NY-TISTADAT-KAMP = W21910GAM-TISTADAT-KAMP         
021834         AND     W21910NY-TISTODAT-KAMP = W21910GAM-TISTODAT-KAMP         
021835         AND     W21910NY-KVKAMP-CARS   = W21910GAM-KVKAMP-CARS)          
021836                                                                          
021837           MOVE W21910NY-IDPTYP                                           
021838                             TO W21912UT-IDPTYP                           
021839           MOVE 'C'          TO W21912UT-KDSTATUS-KAMP                    
021840           MOVE W21910NY-IDKAMP                                           
021841                             TO W21912UT-IDKAMP                           
021842           MOVE W21910NY-TISTADAT-KAMP                                    
021843                             TO W21912UT-TISTADAT-KAMP                    
021844           MOVE W21910NY-TISTODAT-KAMP                                    
021845                             TO W21912UT-TISTODAT-KAMP                    
021846           MOVE W21910NY-KVKAMP-CARS                                      
021847                             TO W21912UT-KVKAMP-CARS                      
021848           PERFORM S11-SKRIV-W21912UT                                     
021868         END-IF                                                           
021869         PERFORM S01-LAES-W21910NY                                        
021870         PERFORM S02-LAES-W21910GAM                                       
021871                                                                          
021872       END-IF                                                             
021880     END-IF                                                               
026100     .                                                                    
026165     EJECT                                                                
026170                                                                          
049400 Z-FINIT SECTION.                                                         
049500                                                                          
049540     DISPLAY 'WS-ANTAL-W21910NY   : '   WS-ANTAL-W21910NY                 
049541     DISPLAY 'WS-ANTAL-W21910GAM   : '  WS-ANTAL-W21910GAM                
049543     DISPLAY 'WS-ANTAL-W21912UT  : '    WS-ANTAL-W21912UT                 
049600     CLOSE W21910NY                                                       
049610           W21910GAM                                                      
049900     CLOSE W21912UT                                                       
050193     SKIP2                                                                
050200     .                                                                    
050300     EJECT                                                                
050400 S01-LAES-W21910NY SECTION.                                               
050500                                                                          
050600     READ W21910NY        INTO W21910NY-AREA                              
050700     AT END                                                               
050900        SET END-OF-W21910NY TO TRUE                                       
050901        MOVE HIGH-VALUE TO W21910NY-IDKAMP                                
050910     NOT AT END                                                           
050920        ADD 1             TO WS-ANTAL-W21910NY                            
051600     END-READ                                                             
051700     .                                                                    
051800     EJECT                                                                
051820 S02-LAES-W21910GAM SECTION.                                              
051830                                                                          
051840     READ W21910GAM       INTO W21910GAM-AREA                             
051850     AT END                                                               
051860        SET END-OF-W21910GAM TO TRUE                                      
051861        MOVE HIGH-VALUE TO W21910GAM-IDKAMP                               
051870     NOT AT END                                                           
051871        ADD 1             TO WS-ANTAL-W21910GAM                           
051890                                                                          
051891     END-READ                                                             
051892     .                                                                    
052000     EJECT                                                                
063200 S11-SKRIV-W21912UT SECTION.                                              
063300                                                                          
063400     WRITE W21912UT-POST FROM W21912UT-AREA                               
063410     ADD 1                   TO WS-ANTAL-W21912UT                         
063900     .                                                                    
