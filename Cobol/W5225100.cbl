001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W5225100.                                                
001400 AUTHOR.         HAMMARIN BO.                                             
001500 DATE-WRITTEN.   NOV 2003.                                                
001600 DATE-COMPILED.                                                           
001800                                                                          
001810*                                                                         
001900*    FUNCTION:                                                            
001910*                                                                         
002000*        THE PROGRAM READS FILE W52204 AND CREATES A                      
002010*        CUSTOMS TRANSACTION FILE FOR VOLVO LOGISTICS                     
002200*                                                                         
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201                                                                          
003202*          --- SELECTED CUSTOMS-DATA FROM IVW-TABLE -INPUT                
003203     SELECT W52204                     ASSIGN TO W52251D1.                
003204                                                                          
003205*          --- CUSTOMS TRANSACTIONS VOLVO LOGISTICS -OUTPUT               
003210     SELECT W52251                     ASSIGN TO W52251D2.                
003211                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003801                                                                          
003802 FD  W52204                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003806 01  IN-POST.                                                             
003807*    03  -COPY W522CUS   -L.                                              
003809                                                                          
003810 FD  W52251                                                               
003811     RECORDING       F                                                    
003812     BLOCK CONTAINS  0.                                                   
003813                                                                          
003814 01  W52251-POST.                                                         
003815*    03  -COPY W475030   -L.                                              
003817                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W5225100'.            
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500     SKIP2                                                                
004910 01  IDPTYP.                                                              
004920     03  WS-IDPTYP               PIC X(3)    VALUE SPACE.                 
004921     03  WS-IDPTYP-HEAD          PIC X(3)    VALUE SPACE.                 
004922                                                                          
004928 01  WS-IDLANDX3-REC-OLD         PIC X(3)    VALUE SPACE.                 
004929                                                                          
004930 01  WS-MISC-NUM.                                                         
004931     03  WS-IDARTNR-NUM          PIC S9(9)   COMP-3.                      
004932     03  WS-IDFINDOC-NUM         PIC S9(9)   COMP-3.                      
004940                                                                          
005011 77  W52204-EOF-SW               PIC X       VALUE 'N'.                   
005020     88  END-OF-W52204                       VALUE 'J'.                   
005400 77  IDLANDX3-REC-SW             PIC X       VALUE 'N'.                   
005500     88  IDLANDX3-REC-FIRST                  VALUE 'J'.                   
005600     EJECT                                                                
005700                                                                          
005910 01  ERRTEXT.                                                             
005920     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005930     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005950 01  KDRC-DISPLAY                PIC Z(5).                                
005960                                                                          
006000 01  GENERAL-SUBPROGRAMS.                                                 
006100     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
006600     EJECT                                                                
006700                                                                          
006800*    --- PARAMETRAR TILL W009CIA                                          
006900*01   -COPY W009CIA                                                       
007000     EJECT                                                                
007100                                                                          
007606*    --- INFIL                                                            
007607 01  IN-AREA-START               PIC X(24)   VALUE                        
007608                                                 'IN-AREA-START'.         
007609 01  IN-AREA.                                                             
007610*    03  -COPY W522CUS     -PRE IN-                                       
007611     EJECT                                                                
007612                                                                          
007619*    --- UTFIL W52251                                                     
007620*    --- FIL VOLVO LOGISTICS                                              
007621 01  W52251-AREA-START           PIC X(24)   VALUE                        
007622                                    'W52251-AREA-START       '.           
007623 01  W52251-AREA.                                                         
007624*    03  -COPY W475030     -PRE W52251-                                   
007630     EJECT                                                                
007801                                                                          
011501 PROCEDURE DIVISION.                                                      
011502                                                                          
011503 MAIN SECTION.                                                            
011900     PERFORM A-INIT                                                       
012000                                                                          
012010     PERFORM S01-READ-W52204                                              
012100     PERFORM UNTIL END-OF-W52204                                          
012313       PERFORM B-EXECUTE                                                  
012830       PERFORM S01-READ-W52204                                            
012900     END-PERFORM                                                          
013100                                                                          
013200     PERFORM Z-FINIT                                                      
013300                                                                          
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014100     .                                                                    
014200     EJECT                                                                
014210                                                                          
014300 A-INIT SECTION.                                                          
014510     OPEN INPUT  W52204                                                   
014610     OPEN OUTPUT W52251                                                   
015400     .                                                                    
015500     EJECT                                                                
015521                                                                          
015522 B-EXECUTE SECTION.                                                       
015539     IF IN-IDLANDX3-SEND = 'SE'                                           
015541       PERFORM BA-CREATE-W52251                                           
015542       PERFORM S11-WRITE-W52251                                           
015556     END-IF                                                               
015615     .                                                                    
015616     EJECT                                                                
015618                                                                          
015619 BA-CREATE-W52251 SECTION.                                                
015620*    --- BYGG UPP POST W52251                                             
015622     MOVE SPACE             TO W52251-AREA                                
015623                                                                          
015624     MOVE '03'              TO W52251-TREST03-IDPTYP2                     
015625     MOVE IN-IDARTNR        TO WS-IDARTNR-NUM                             
015629     MOVE 'VO'              TO CIA-IDARTPRE-IN                            
015630     MOVE WS-IDARTNR-NUM    TO CIA-IDARTBET-IN                            
015631     CALL W009CIA USING        CIA-W009CIA                                
015632     MOVE CIA-IDARTBET-UT   TO W52251-TREST03-IDARTNR                     
015633                                                                          
015634     MOVE IN-IDDISTR        TO W52251-TREST03-IDDISTR                     
015635     MOVE IN-IDFINDOC       TO WS-IDFINDOC-NUM                            
015636     MOVE 'VO'              TO CIA-IDARTPRE-IN                            
015637     MOVE WS-IDFINDOC-NUM   TO CIA-IDARTBET-IN                            
015639     CALL W009CIA USING        CIA-W009CIA                                
015640     MOVE CIA-IDARTBET-UT   TO W52251-TREST03-IDFAKT                      
015641                                                                          
015642     MOVE IN-DAFINDOC       TO W52251-TREST03-DAFAKT                      
015643     MOVE 'N'               TO W52251-TREST03-KOD                         
015644     MOVE IN-KVLEVART       TO W52251-TREST03-KVLEVART                    
015650     MOVE IN-IDLANDX3-BET   TO W52251-TREST03-IDLANDX2                    
015654     .                                                                    
015660     EJECT                                                                
015670                                                                          
015795 Z-FINIT SECTION.                                                         
015798     CLOSE W52204                                                         
015799           W52251                                                         
015800     .                                                                    
015801     EJECT                                                                
015802                                                                          
015810*    --- READ AND WRITE SECTIONS                                          
015900 S01-READ-W52204 SECTION.                                                 
016100     READ                                                                 
016110       W52204 INTO IN-AREA                                                
016300     AT END MOVE YES TO W52204-EOF-SW                                     
016400     END-READ                                                             
016500     .                                                                    
016600     EJECT                                                                
016610                                                                          
016700 S11-WRITE-W52251 SECTION.                                                
016900     WRITE W52251-POST FROM W52251-AREA                                   
017000     .                                                                    
017100     EJECT                                                                
