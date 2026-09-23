000100 01  RESP-W50293O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM W50293         
000300*                                 INVENTORY FOLLOW-UP SELECTION           
000400     03 RESP-KDCMDVAL        PIC X.                                       
000500*                                 GENERELL KOMMANDOKOD                    
000600*                                 GENERAL COMMAND-CODE                    
000700     03 RESP-KVRADER         PIC Z(4)9.                                   
000800*                                 ANTAL RADER                             
000900*                                 NUMBER OF LINES                         
001000     03 RESP-TABELLRAD       OCCURS 100 TIMES.                            
001100*                                 GRUPP MED TABELLRADER                   
001200        05 RESP-KVART-INVTOT-LINE                                         
001300                             PIC Z(6)9.                                   
001400*                                 ANT ART I INVENTERINGEN                 
001500*                                 QTY PARTS IN INVENTORY                  
001600        05 RESP-KVART-INVPRINT-LINE                                       
001700                             PIC Z(6)9.                                   
001800*                                 ANT ART SKRIVAS UT & INVENTERAS         
001900*                                 QTY PARTS PRINTED FOR INVENTORY         
002000        05 RESP-KVART-INVLEFT-LINE                                        
002100                             PIC Z(6)9.                                   
002200*                                 ANT ART KVAR ATT INVENTERAS             
002300*                                 QTY PARTS LEFT TO INVENTORY             
002400        05 RESP-ADLAGOMR-LINE                                             
002500                             PIC X(2).                                    
002600*                                 LAGEROMRÅDE                             
002700*                                 AREA                                    
002800        05 RESP-ADGANG-LINE  PIC X(2).                                    
002900*                                 GÅNG                                    
003000*                                 AISLE                                   
003100        05 RESP-IDPRTOMG-LINE                                             
003200                             PIC 9.                                       
003300*                                 PRINTOMGÅNG FÖR INVENT/JUSTERIN         
003400*                                 PRINT ROUND OF INVENTORY/ADJUST         
003500        05 RESP-IDACSNR-LINE PIC 9(6).                                    
003600*                                 NR.SERIE FÖR ACS-LISTOR                 
003700*                                 SERIAL NO. FOR ACS REPORTS              
003800        05 RESP-IDCOUNTER-LINE                                            
003900                             PIC X(20).                                   
004000*                                 RÄKNARE/INVENTERARE                     
004100*                                 COUNTER'S NAME IN INVENTORY             
004200*** END OF VILMAII-COPY LENGTH= 5206 BYTES                                
