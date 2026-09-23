000100 01  REQU-WL0134I2.                                                       
000200*                                 REQUEST TO PGM WL0134                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDPRC-KEY.                                                   
000600*                                 PRODUKTIONSKANAL                        
000700        05 REQU-IDPRCBAS     PIC X(3).                                    
000800*                                 PRC-BAS                                 
000900        05 REQU-IDPRCVAR     PIC X.                                       
001000*                                 PRC-VARIANT                             
001100     03 REQU-DARFSDAT-KEY    PIC X(10).                                   
001200*                                 KLART F÷R TRANSPORT ≈≈≈≈MMDD            
001300*                                 OR ≈≈≈≈-MM-DD                           
001400     03 REQU-IDTRP-KEY.                                                   
001500*                                 TRANSPORTIDENTITET                      
001600        05 REQU-IDTRPLOS     PIC X(3).                                    
001700*                                 TRANSPORTL÷SNING                        
001800        05 REQU-IDTRPVAR     PIC X(2).                                    
001900*                                 TRANSPORTL÷SNINGSGRUPP                  
002000     03 REQU-FLPREPRINT      PIC X.                                       
002100*                                 FLAGGA PRE PRINT                        
002200     03 REQU-IDQUEUENR       PIC 9(3).                                    
002300*                                 PRE PRINT QUEUE NUMBER                  
002400     03 REQU-IDANSTNR        PIC 9(5).                                    
002500*                                 ANSTƒLLNINGSNUMMER                      
002600     03 REQU-IDBORD          PIC X(3).                                    
002700*                                 PACK-BORD                               
002800     03 REQU-KDMATT          PIC X.                                       
002900*                                 M≈TTKOD                                 
003000     03 REQU-KVRADER-MAX1    PIC 9(5).                                    
003100*                                 MAX INDEX KOPPLAT TILL OCCURS N         
003200*                                 EDAN.                                   
003300     03 REQU-IDTRANS         PIC X(4).                                    
003400*                                 BILDNUMMER                              
003500     03 REQU-TABELLRAD       OCCURS 1 TO 1100 TIMES                       
003600                             DEPENDING ON REQU-KVRADER-MAX1.              
003700*                                 GRUPP MED RADER TILL ORDERDEL           
003800        05 REQU-FLORDDEL     PIC X.                                       
003900*                                 ALLMƒN FLAGGA                           
004000        05 REQU-IDPRODNR     PIC 9(7).                                    
004100*                                 PRODUKTIONSNUMMER                       
004200        05 REQU-IDPLKLST     PIC 9(3).                                    
004300*                                 PLOCKLISTNUMMER                         
004400*** END OF VILMAII-COPY LENGTH= 12143 BYTES                               
