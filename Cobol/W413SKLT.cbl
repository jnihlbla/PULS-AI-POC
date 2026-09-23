000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W413SKLT.                                                
000300 AUTHOR.         GÖRAN KJELLSON    GUIDE                                  
000400 DATE-WRITTEN.   APRIL   2006                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KONVERTERAR IDLANDX2 TILL IDSKYLT                                
000900*        OM IDLANDX2 SAKNAS I PROGRAMMET RETURNERAS BLANK I IDSKYL        
001000*        RETURNERAS BLANK I IDSKYLT                                       
001100*                                                                         
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400 DATA DIVISION.                                                           
001500 WORKING-STORAGE SECTION.                                                 
001600                                                                          
001700 77  IDPGM                       PIC X(8)    VALUE 'W413SKLT'.            
001800                                                                          
001900 LINKAGE SECTION.                                                         
002000*    -COPY W413SKLT                                                       
002100                                                                          
002200 PROCEDURE DIVISION  USING SKLT-W413SKLT.                                 
002300 MAIN SECTION.                                                            
002400                                                                          
002500     EVALUATE SKLT-IDLANDX2                                               
002600        WHEN 'SE'                                                         
002700           MOVE 'S  ' TO SKLT-IDSKYLT                                     
002800        WHEN 'ES'                                                         
002900           MOVE 'E  ' TO SKLT-IDSKYLT                                     
003000        WHEN 'IT'                                                         
003100           MOVE 'I  ' TO SKLT-IDSKYLT                                     
003200        WHEN 'DE'                                                         
003300        WHEN 'AT'                                                         
003400        WHEN 'CH'                                                         
003500           MOVE 'D  ' TO SKLT-IDSKYLT                                     
003600        WHEN 'FI'                                                         
003610           MOVE 'SF ' TO SKLT-IDSKYLT                                     
003620        WHEN 'FR'                                                         
003630           MOVE 'F  ' TO SKLT-IDSKYLT                                     
003700        WHEN 'US'                                                         
003800           MOVE 'USA' TO SKLT-IDSKYLT                                     
003900        WHEN 'TR'                                                         
004000           MOVE 'TR'  TO SKLT-IDSKYLT                                     
004001        WHEN 'CN'                                                         
004002           MOVE 'RCN' TO SKLT-IDSKYLT                                     
004003*       WHEN 'JP'                                                         
004004*          MOVE 'J '  TO SKLT-IDSKYLT                                     
004200        WHEN OTHER                                                        
004300           MOVE 'GB ' TO SKLT-IDSKYLT                                     
004400     END-EVALUATE                                                         
004500                                                                          
004600     MOVE ZERO TO RETURN-CODE                                             
004700     GOBACK                                                               
004800     .                                                                    
