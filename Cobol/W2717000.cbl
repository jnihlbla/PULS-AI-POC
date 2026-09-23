000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2717000.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   FEB 1998.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                PROGRAMMET LÄSER RESTORDER OCH SUMMERAR                  
001000*                ORDERRADER PER ARTIKEL FÖR ATT SENARE                    
001100*                SKRIVA LISTAN 'TOPP 100 RESTORDER'                       
001400*                I ORDERLINES ORDNING                                     
001500*                                                                         
001600*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002810     SELECT W27170IN                   ASSIGN TO W27170D1.                
002900*                                                                         
003000     SELECT W27170UT                   ASSIGN TO W27170D2.                
004000     SKIP2                                                                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004410     SKIP3                                                                
004420                                                                          
004421 FD  W27170IN                                                             
004422     RECORDING       F                                                    
004423     BLOCK CONTAINS  0.                                                   
004424                                                                          
004425*01  POST  -COPY W27162    -PRE IN-    -L.                                
004426                                                                          
004427     SKIP3                                                                
004428                                                                          
004429 FD  W27170UT                                                             
004430     RECORDING       F                                                    
004440     BLOCK CONTAINS  0.                                                   
004450                                                                          
004460*01  POST  -COPY W27162    -PRE UT-    -L.                                
004470                                                                          
009100*                                                                         
009200     EJECT                                                                
009300 WORKING-STORAGE SECTION.                                                 
009400*    -- CHECKED BY WY2000                                                 
009500 77  IDPGM                       PIC X(8)    VALUE 'W2717000'.            
010500 77  JA                          PIC X       VALUE 'J'.                   
010700                                                                          
010800 01  WS.                                                                  
010802     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
010803     03  WS-ANTAL-ORDRAD         PIC 9(7)    VALUE ZERO.                  
010805     03  WS-ANTAL-QTY            PIC 9(7)    VALUE ZERO.                  
014200                                                                          
018500 77  W27170-EOF-SW               PIC X       VALUE 'N'.                   
018600     88  END-OF-W27170                       VALUE 'J'.                   
018900                                                                          
019000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
019100 01  FILLER REDEFINES DAGENS-DATUM.                                       
019200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
019300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
019400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
020000                                                                          
021100 01  IN-AREA-START               PIC X(24)   VALUE                        
021200                                             'IN-AREA-START'.             
021300     SKIP2                                                                
021400*01  AREA -COPY W27162     -PRE IN-                                       
022800                                                                          
022900     EJECT                                                                
023000 01  UT-AREA-START              PIC X(24)   VALUE                         
023100                                 'UT-AREA-START  '.                       
023200     SKIP2                                                                
023201*01  AREA -COPY W27162     -PRE UT-                                       
023210                                                                          
023310                                                                          
023400     EJECT                                                                
023500 PROCEDURE DIVISION.                                                      
023600                                                                          
023700     PERFORM A-INIT                                                       
023800                                                                          
023910     PERFORM S01-LAES-W27170IN                                            
024100                                                                          
024200     PERFORM UNTIL END-OF-W27170                                          
024400                                                                          
024437       MOVE IN-AREA          TO UT-AREA                                   
024438       MOVE ZERO             TO WS-ANTAL-ORDRAD                           
024439                                WS-ANTAL-QTY                              
024440                                UT-DARODAT                                
024445       MOVE IN-IDARTNR       TO WS-IDARTNR                                
024460                                                                          
024500       PERFORM UNTIL END-OF-W27170                                        
024520       OR  IN-IDARTNR    NOT = WS-IDARTNR                                 
024522                                                                          
024531         ADD IN-ANTAL-ORDRAD TO WS-ANTAL-ORDRAD                           
024535         ADD IN-ANTAL-QTY    TO WS-ANTAL-QTY                              
024540                                                                          
024900         PERFORM S01-LAES-W27170IN                                        
025100       END-PERFORM                                                        
025110                                                                          
025692       MOVE WS-ANTAL-ORDRAD  TO UT-ANTAL-ORDRAD                           
025695       MOVE WS-ANTAL-QTY     TO UT-ANTAL-QTY                              
025700                                                                          
025800       PERFORM S02-SKRIV-UTFIL                                            
026000                                                                          
026100     END-PERFORM                                                          
026200                                                                          
027000                                                                          
030000                                                                          
030100     PERFORM Z-FINIT                                                      
030200                                                                          
030300     MOVE ZERO TO RETURN-CODE                                             
030400     GOBACK                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 A-INIT SECTION.                                                          
030800     SKIP2                                                                
030900                                                                          
031000     OPEN INPUT  W27170IN                                                 
031200     OPEN OUTPUT W27170UT                                                 
031600     .                                                                    
031700     EJECT                                                                
031800                                                                          
031900 Z-FINIT SECTION.                                                         
032000                                                                          
032100                                                                          
032300     CLOSE W27170IN                                                       
032400           W27170UT                                                       
032600     .                                                                    
032700     EJECT                                                                
032800 S01-LAES-W27170IN SECTION.                                               
032900     SKIP2                                                                
033000     READ W27170IN           INTO IN-AREA                                 
033100     AT END                                                               
033200        MOVE JA TO W27170-EOF-SW                                          
033300                                                                          
033400     END-READ                                                             
033500     .                                                                    
033600     EJECT                                                                
034600 S02-SKRIV-UTFIL SECTION.                                                 
034700     SKIP2                                                                
035000     WRITE UT-POST           FROM UT-AREA                                 
035600     .                                                                    
