000100 01  LOGA-WDP901.                                                         
000200*                                 LOGG AV ÄNDRADE KVANTITTER              
000300*                                 FYSISK NYCKEL: WDP901KY                 
000400*                                 (IDARTNR + IDDC +                       
000500*                                  TISEGKEY-9KOMPL)                       
000600     03 LOGA-IDARTNR         PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 LOGA-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 LOGA-TISEGKEY-9KOMPL PIC S9(9)           COMP-3.                  
001300*                                 TEKNISK SEG-NYCKEL ÅÅÅÅMMDDL            
001400*                                 TISEGKEY = 999999999 - DAT+LOP          
001500*                                 TECHNICAL SEGMENT KEY                   
001600     03 LOGA-IDSEGM          PIC X(6).                                    
001700*                                 SEGMENT                                 
001800     03 LOGA-IDSTYP          PIC X(4).                                    
001900*                                 LOGGTYP SALDOÄNDRING                    
002000*                                 LOGG TYPE CHANGE OF QUANTITY            
002100     03 LOGA-IDPGM           PIC X(8).                                    
002200*                                 PROGRAM IDENTITET                       
002300*                                 PROGRAM INTENTITY                       
002400     03 LOGA-IDTRANS         PIC X(4).                                    
002500*                                 BILDNUMMER                              
002600*                                 SCREEN NUMBER                           
002700     03 LOGA-IDUSER          PIC X(8).                                    
002800*                                 ANVÄNDARENS SÄKERHETS ID                
002900*                                 USER SECURITY-IDENTITY                  
003000     03 LOGA-KVANTAL         PIC S9(7)           COMP-3.                  
003100*                                 ANTAL                                   
003200*                                 NUMBER                                  
003300     03 LOGA-KVANTAL-F       PIC S9(9)           COMP-3.                  
003400*                                 ALLMÄNT ANTAL    KVANTAL-FÖRE           
003500     03 LOGA-KVANTAL-E       PIC S9(9)           COMP-3.                  
003600*                                 ALLMÄNT ANTAL    KVANTAL-EFTER          
003700     03 LOGA-FILLER          PIC X(57).                                   
003800*** END OF VILMAII-COPY LENGTH= 113 BYTES                                 
