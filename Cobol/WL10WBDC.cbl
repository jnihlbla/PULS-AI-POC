000100*COMPOPT VRTEREUS=YES                                                     
000200*-- ABOVE COMPOPT NEEDED FOR CALLS FROM EPLUS                             
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     WL10WBDC.                                                
000500 AUTHOR.         ANDRE KJELL.                                             
000600 DATE-WRITTEN.   12/10/24.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNCTION:                                                            
001000*        SUBPROGRAM FOR TESTING IF A DC USES PULS WEB, AND IF SO          
001100*        WHICH "GROUP" IT BELONGS TO (EUROPE, CHINA, PACIFIC ...)         
001200                                                                          
001300 DATA DIVISION.                                                           
001400                                                                          
001500 WORKING-STORAGE SECTION.                                                 
001600                                                                          
001700 77  IDPGM                       PIC X(8)    VALUE 'WL10WBDC'.            
001800 77  YES                         PIC X       VALUE 'J'.                   
001900 77  NOO                         PIC X       VALUE 'N'.                   
002000                                                                          
002100     EJECT                                                                
002200 LINKAGE SECTION.                                                         
002300                                                                          
002400*    -COPY  WL10WBDC                                                      
002500                                                                          
002600     EJECT                                                                
002700 PROCEDURE DIVISION USING WBDC-AREA.                                      
002800                                                                          
002900*    -- FIRST, ASSUME IT IS NOT A WEB DC                                  
003000     MOVE NOO   TO WBDC-FLWEBDC                                           
003100     MOVE SPACE TO WBDC-KDMFUP                                            
003200                                                                          
003300     IF WBDC-IDDC = '21' OR '24' OR '26'                                  
003400     OR WBDC-IDDC(1:1) = '1' AND WBDC-IDDC(2:1) NOT NUMERIC               
003500     OR WBDC-IDDC(1:1) = '2' AND WBDC-IDDC(2:1) NOT NUMERIC               
003600     OR WBDC-IDDC(1:1) = '3'                                              
003700       MOVE YES   TO WBDC-FLWEBDC                                         
003800       MOVE 'MA'  TO WBDC-KDMFUP                                          
003900     END-IF                                                               
004000                                                                          
004010     IF WBDC-IDDC = '51' OR '43' OR '44' OR '45' OR '46' OR               
004010                    '47' OR '92'                                          
004020       MOVE YES   TO WBDC-FLWEBDC                                         
004030       MOVE 'NA'  TO WBDC-KDMFUP                                          
004040     END-IF                                                               
004000                                                                          
004010     IF WBDC-IDDC = '52'                                                  
004020       MOVE YES   TO WBDC-FLWEBDC                                         
004030       MOVE 'BR'  TO WBDC-KDMFUP                                          
004040     END-IF                                                               
004000                                                                          
004010     IF WBDC-IDDC = '53'                                                  
004020       MOVE YES   TO WBDC-FLWEBDC                                         
004030       MOVE 'MX'  TO WBDC-KDMFUP                                          
004040     END-IF                                                               
004041                                                                          
004042     IF WBDC-IDDC = '6A' OR '61' OR '62'                                  
004043       MOVE YES   TO WBDC-FLWEBDC                                         
004044       MOVE 'PF'  TO WBDC-KDMFUP                                          
004045     END-IF                                                               
004046                                                                          
004047     IF WBDC-IDDC = '63' OR '93'                                          
004048       MOVE YES   TO WBDC-FLWEBDC                                         
004049       MOVE 'TH'  TO WBDC-KDMFUP                                          
004050     END-IF                                                               
004046                                                                          
004047     IF WBDC-IDDC = '64'                                                  
004048       MOVE YES   TO WBDC-FLWEBDC                                         
004049       MOVE 'TW'  TO WBDC-KDMFUP                                          
004050     END-IF                                                               
004046                                                                          
004047     IF WBDC-IDDC = '65'                                                  
004048       MOVE YES   TO WBDC-FLWEBDC                                         
004049       MOVE 'KR'  TO WBDC-KDMFUP                                          
004050     END-IF                                                               
004051                                                                          
004052     IF WBDC-IDDC = '66'                                                  
004053       MOVE YES   TO WBDC-FLWEBDC                                         
004054       MOVE 'MY'  TO WBDC-KDMFUP                                          
004055     END-IF                                                               
004056                                                                          
004060     IF WBDC-IDDC = '67'                                                  
004070       MOVE YES   TO WBDC-FLWEBDC                                         
004080       MOVE 'AS'  TO WBDC-KDMFUP                                          
004090     END-IF                                                               
004091                                                                          
004100     IF WBDC-IDDC(1:1) = '7'                                              
004200       MOVE YES   TO WBDC-FLWEBDC                                         
004300       MOVE 'CN'  TO WBDC-KDMFUP                                          
004400     END-IF                                                               
004500                                                                          
004600     IF WBDC-IDDC = '85'                                                  
004700       MOVE YES   TO WBDC-FLWEBDC                                         
004800       MOVE 'ZA'  TO WBDC-KDMFUP                                          
004900     END-IF                                                               
004500                                                                          
004600     IF WBDC-IDDC = '86'                                                  
004700       MOVE YES   TO WBDC-FLWEBDC                                         
004800       MOVE 'TR'  TO WBDC-KDMFUP                                          
004900     END-IF                                                               
005000                                                                          
005010     IF WBDC-IDDC = '87'                                                  
005020       MOVE YES   TO WBDC-FLWEBDC                                         
005030       MOVE 'AE'  TO WBDC-KDMFUP                                          
005040     END-IF                                                               
005050                                                                          
005100     MOVE ZERO TO RETURN-CODE                                             
005200     GOBACK                                                               
005300     .                                                                    
