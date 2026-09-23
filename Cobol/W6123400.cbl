000010*                                                                         
000020******************************************************************        
000030*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL1002      *        
000040******************************************************************        
000050*                                                                         
000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6123400.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   97/01/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR LISTA SAMTLIGA KOLLIN I FAKTURA                           
001100*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- LISTFIL                                                    
002403     SELECT W61233                     ASSIGN TO W61234D1.                
002404     SKIP2                                                                
002405*          --- LISTA                                                      
002410     SELECT W61234-001                 ASSIGN TO W61234D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W61233                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY W61233      -L.                                                
003007     SKIP3                                                                
003008 FD  W61234-001                                                           
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003011     SKIP2                                                                
003020 01  W61234-001-RAD              PIC X(121).                              
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W6123400'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W61233-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W61233                       VALUE 'J'.                   
003820*      --- VALID IDDC CODES                                               
003830*                                                                         
003840*01    -COPY WWDC99                                                       
003850       EJECT                                                              
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004500     EJECT                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
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
006101     EJECT                                                                
006102 01  IN-AREA-START               PIC X(24)   VALUE                        
006103                                 'IN-AREA-START  '.                       
006104     SKIP2                                                                
006105                                                                          
006106*01  AREA -COPY W61233     -PRE IN-                                       
006107     EJECT                                                                
006108 01  W001-AREA-START             PIC X(24)   VALUE                        
006109                                 'W001-AREA-START  '.                     
006110     SKIP2                                                                
006111 01  W001-HJALPAREOR.                                                     
006112*                                                                         
006113     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
006114     03  W001-ANTAL-RADER                                                 
006115                                 PIC 9(3)    VALUE 999.                   
006116     03  W001-MAX-RADER-PER-SIDA                                          
006117                                 PIC 9(3)    VALUE 42.                    
006118     03  W001-MAX-POSITIONER-PER-RAD                                      
006119                                 PIC 9(3)    VALUE 120.                   
006120     03  W001-LISTNR             PIC X(11)   VALUE 'W61234-001'.          
006121     03  W001-LISTID             PIC X(11)   VALUE 'W61234-001'.          
006122     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
006124     EJECT                                                                
006125 01  W001-RAD.                                                            
006126     03  FILLER                  PIC X(121)  VALUE SPACE.                 
006127     EJECT                                                                
006149 01  FILLER                      PIC X(118)                               
006150                                      VALUE ' RUBRIRAD-1 '.               
006151 01  W001-RUBRIK-1.                                                       
006152     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006153     03  FILLER                  PIC X(15)                                
006155                                VALUE 'UNPACKING LIST '.                  
006156*    03  FILLER                  PIC X(51) VALUE SPACE.                   
006157     03  FILLER                  PIC X(10) VALUE SPACE.                   
006158     03  FILLER                  PIC X(5)  VALUE 'VOLVO'.                 
006159     03  FILLER                  PIC X(36) VALUE SPACE.                   
006160     03  FILLER                  PIC X(7)  VALUE 'DATE '.                 
006161     03  W001-DATUM              PIC XXBXXBXX.                            
006162     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006170     03  W001-SID                PIC Z(4)9.                               
006171     03  FILLER                  PIC X(28) VALUE SPACE.                   
006172     EJECT                                                                
006173 01  FILLER                      PIC X(118)                               
006174                                      VALUE ' RUBRIKRAD-2'.               
006175 01  W001-RUBRIK-2.                                                       
006176     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006177     03  FILLER                  PIC X(11) VALUE 'INVOICE NO.'.           
006178     03  FILLER                  PIC X(2)  VALUE SPACE.                   
006180     03  W001-IDFAKT             PIC Z(6)9 VALUE SPACE.                   
006181     03  FILLER                  PIC X(6)  VALUE SPACE.                   
006182     03  FILLER                  PIC X(7)  VALUE 'CARRIER'.               
006183     03  FILLER                  PIC X(2)  VALUE SPACE.                   
006184     03  W001-IDLBBET            PIC X(12) VALUE SPACE.                   
006185     03  FILLER                  PIC X(6)  VALUE SPACE.                   
006186     03  W001-ETA                PIC X(3)  VALUE SPACE.                   
006187     03  FILLER                  PIC X(2)  VALUE SPACE.                   
006188     03  W001-TIBERANK           PIC X(6)  VALUE SPACE.                   
006189     03  FILLER                  PIC X(53) VALUE SPACE.                   
006196                                                                          
006197 01  FILLER                      PIC X(118)                               
006198                                      VALUE ' RUBRIKRAD2 '.               
006199 01  W001-RUBRIK2.                                                        
006200     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006201     03  FILLER                  PIC X(9)  VALUE 'ORDER NO.'.             
006202     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006203     03  FILLER                  PIC X(4)  VALUE 'CASE'.                  
006204     03  FILLER                  PIC X(5)  VALUE SPACE.                   
006205     03  FILLER                  PIC X(5)  VALUE 'CUST.'.                 
006206     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006207     03  FILLER                  PIC X(10) VALUE 'LINES/CASE'.            
006208     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006209     03  FILLER                  PIC X(9)  VALUE 'PRIO/CASE'.             
006210     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006215     03  FILLER                  PIC X(9)  VALUE 'B.O./CASE'.             
006216     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006217     03  FILLER                  PIC X(9)  VALUE 'NEW/CASE'.              
006218     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006219     03  FILLER                  PIC X(11) VALUE 'INFORMATION'.           
006220     03  FILLER                  PIC X(28) VALUE SPACE.                   
006221     EJECT                                                                
006222 01  W001-DETALJ1.                                                        
006223     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006224     03  W001-IDKUNDRF           PIC X(5)  VALUE SPACE.                   
006225     03  FILLER                  PIC X(6)  VALUE SPACE.                   
006226     03  W001-IDKOLLI            PIC Z(4)9 VALUE SPACE.                   
006227     03  FILLER                  PIC X(3)  VALUE SPACE.                   
006228     03  W001-IDKUNDNR           PIC Z(6)9 VALUE ZERO.                    
006229     03  FILLER                  PIC X(7)  VALUE SPACE.                   
006230     03  W001-IDARTNR-KOLLI      PIC Z(5)9 VALUE ZERO.                    
006231     03  FILLER                  PIC X(6)  VALUE SPACE.                   
006232     03  W001-IDARTNR-PRIO       PIC Z(5)9 VALUE ZERO.                    
006233     03  FILLER                  PIC X(6)  VALUE SPACE.                   
006234     03  W001-IDARTNR-BO         PIC Z(5)9 VALUE ZERO.                    
006235     03  FILLER                  PIC X(5)  VALUE SPACE.                   
006236     03  W001-IDARTNR-NEW        PIC Z(5)9 VALUE ZERO.                    
006237     03  FILLER                  PIC X(4)  VALUE SPACE.                   
006238     03  W001-TEINFO             PIC X(7)  VALUE SPACE.                   
006239     03  FILLER                  PIC X(32) VALUE SPACE.                   
006250     EJECT                                                                
006300 PROCEDURE DIVISION.                                                      
006400 MAIN SECTION.                                                            
006700                                                                          
006800     PERFORM A-INIT                                                       
006900                                                                          
006910     PERFORM S01-LAES-W61233                                              
007000     PERFORM UNTIL END-OF-W61233                                          
007200        PERFORM B-SKAPA-LISTA                                             
007710        PERFORM S01-LAES-W61233                                           
007800     END-PERFORM                                                          
008000                                                                          
008100     PERFORM Z-FINIT                                                      
008200                                                                          
008300     MOVE ZERO TO RETURN-CODE                                             
008400     GOBACK                                                               
008500     .                                                                    
008600     SKIP3                                                                
008700 A-INIT SECTION.                                                          
008801                                                                          
008810     OPEN INPUT  W61233                                                   
008910     OPEN OUTPUT W61234-001                                               
009000                                                                          
009100     ACCEPT DAGENS-DATUM FROM DATE                                        
009210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009300     .                                                                    
009400     EJECT                                                                
009500 B-SKAPA-LISTA SECTION.                                                   
009501                                                                          
009503     MOVE IN-IDKUNDRF      TO W001-IDKUNDRF                               
009504     MOVE IN-IDKOLLI       TO W001-IDKOLLI                                
009505     MOVE IN-IDKUNDNR      TO W001-IDKUNDNR                               
009508     MOVE IN-IDARTNR-KOLLI TO W001-IDARTNR-KOLLI                          
009509     MOVE IN-IDARTNR-PRIO  TO W001-IDARTNR-PRIO                           
009510     MOVE IN-IDARTNR-BO    TO W001-IDARTNR-BO                             
009511     MOVE IN-IDARTNR-NEW   TO W001-IDARTNR-NEW                            
009512     MOVE IN-TEINFO        TO W001-TEINFO                                 
009513     PERFORM S21-SKRIV-W61234-001                                         
009514     .                                                                    
009515     SKIP3                                                                
009520 Z-FINIT SECTION.                                                         
009600                                                                          
009601     CLOSE W61233                                                         
009610           W61234-001                                                     
009701                                                                          
009702     MOVE 'S' TO POSTSUM-OPKOD                                            
009710     CALL POSTSUM USING POSTSUM-PARM                                      
009800     .                                                                    
009901     EJECT                                                                
009902 S01-LAES-W61233  SECTION.                                                
009903                                                                          
009904     READ W61233 INTO IN-AREA                                             
009905     AT END                                                               
009907        SET END-OF-W61233 TO TRUE                                         
009908                                                                          
009909     NOT AT END                                                           
009910        MOVE 'W61233'   TO POSTSUM-FDNAMN                                 
009911        MOVE 'W61234D1' TO POSTSUM-DDNAMN2                                
009912        MOVE SPACE      TO POSTSUM-TRANSTYP                               
009913        CALL POSTSUM USING POSTSUM-PARM                                   
009914     END-READ                                                             
009920     .                                                                    
010101     EJECT                                                                
010102 S21-SKRIV-W61234-001  SECTION.                                           
010103                                                                          
010104     MOVE IN-IDDC TO WS-IDDC                                              
010105     MOVE 2 TO W001-SKIP                                                  
010106     IF W001-ANTAL-RADER > W001-MAX-RADER-PER-SIDA                        
010107       MOVE DAGENS-DATUM TO W001-DATUM                                    
010108       MOVE 'W61234-001' TO W001-LISTNR                                   
010109                            W001-LISTID                                   
010110       MOVE IN-IDFAKT    TO W001-IDFAKT                                   
010111       MOVE IN-IDLBBET   TO W001-IDLBBET                                  
010112       IF NDC                                                             
010113          MOVE 'ETA'            TO W001-ETA                               
010114          MOVE IN-DABERANK(3:6) TO W001-TIBERANK                          
010115       ELSE                                                               
010116          MOVE SPACE        TO W001-ETA                                   
010117                               W001-TIBERANK                              
010118       END-IF                                                             
010119       PERFORM S21A-SKRIV-RUBRIKER                                        
010120     END-IF                                                               
010121                                                                          
010122     MOVE W001-DETALJ1    TO W001-RAD                                     
010123     WRITE W61234-001-RAD FROM W001-RAD AFTER W001-SKIP                   
010124     MOVE SPACE TO W001-RAD                                               
010125     ADD  +2 TO W001-ANTAL-RADER                                          
010126     .                                                                    
010127     SKIP3                                                                
010128 S21A-SKRIV-RUBRIKER SECTION.                                             
010129                                                                          
010130     ADD +1 TO W001-SIDRAKNARE                                            
010131     MOVE W001-SIDRAKNARE TO W001-SID                                     
010132     IF NDC                                                               
010133       WRITE W61234-001-RAD FROM W001-RAD      AFTER PAGE                 
010134       WRITE W61234-001-RAD FROM W001-RUBRIK-1 AFTER 2                    
010135       WRITE W61234-001-RAD FROM W001-RUBRIK-2 AFTER 6                    
010136       WRITE W61234-001-RAD FROM W001-RUBRIK2  AFTER 4                    
010139       MOVE +19 TO W001-ANTAL-RADER                                       
010140       MOVE 3 TO W001-SKIP                                                
010141     ELSE                                                                 
010142       WRITE W61234-001-RAD FROM W001-RUBRIK-1 AFTER PAGE                 
010143       WRITE W61234-001-RAD FROM W001-RUBRIK-2 AFTER 4                    
010144       WRITE W61234-001-RAD FROM W001-RUBRIK2  AFTER 4                    
010145                                                                          
010146       MOVE +15 TO W001-ANTAL-RADER                                       
010147       MOVE 3 TO W001-SKIP                                                
010148     END-IF                                                               
010150     .                                                                    
010200     EJECT                                                                
