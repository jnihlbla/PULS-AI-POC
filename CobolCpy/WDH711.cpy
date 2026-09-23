000100 01  INVH-WDH711.                                                         
000200*                                 INVENTERINGSHISTORIK                    
000300*                                 INVENTERINGS JUSTERINGAR                
000400*                                 FYSISK NYCKEL: WDH711KY                 
000500*                                 (TISEGKEY + IDDC)                       
000600     03 INVH-TISEGKEY        PIC S9(9)           COMP-3.                  
000700*                                 TEKNISK SEG-NYCKEL ÅÅÅÅMMDDL            
000800*                                 TISEGKEY = 999999999 - DAT+LOP          
000900*                                 TECHNICAL SEGMENT KEY                   
001000     03 INVH-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 INVH-DAREGDAT-CLO    PIC 9(8).                                    
001400*                                 KLARDATUM FÖR INVENTERING               
001500*                                 CLOSED DATE OF STOCK-TAKING             
001600     03 INVH-FLAUTLSJ        PIC X.                                       
001700*                                 AUTOMATISK SALDOJUSTERING               
001800*                                 AUTOMATIC STOCK BALANCE ADJUST.         
001900     03 INVH-IDPW            PIC X(8).                                    
002000*                                 PASSWORD   (LÖSENORD)                   
002100*                                 PASSWORD                                
002200     03 INVH-IDUSER-CLO      PIC X(8).                                    
002300*                                 ANVÄNDAR-ID VID AVSLUT                  
002400*                                 USER ID AT CLOSING                      
002500     03 INVH-KDJUSTYP        PIC S9              COMP-3.                  
002600*                                 JUSTERINGSTYP                           
002700     03 INVH-KVJUSTKV        PIC S9(7)           COMP-3.                  
002800*                                 JUSTERAD KVANTITET                      
002900*                                 ADJUSTED QUANTITY                       
003000     03 INVH-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
003100*                                 ARTIKELSTANDARDPRIS                     
003200*                                 STANDARD PRICE                          
003300     03 INVH-DAREGDAT-CRE    PIC 9(8).                                    
003400*                                 DATUM NÄR INVENTERING PÅBÖRJAS          
003500*                                 CREATION DATE                           
003600     03 INVH-DAREGDAT-PR1    PIC 9(8).                                    
003700*                                 DATUM FÖR FÖRSTA PRINTNING              
003800*                                 DATE FOR FIRST PRINTING                 
003900     03 INVH-DAREGDAT-PR2    PIC 9(8).                                    
004000*                                 DATUM FÖR ANDRA  PRINTNING              
004100*                                 DATE FOR SECOND PRINT                   
004200     03 INVH-DAREGDAT-PR3    PIC 9(8).                                    
004300*                                 DATUM FÖR TREDJE PRINTNING              
004400*                                 DATE FOR THIRD PRINT                    
004500     03 INVH-IDUSER-PR1      PIC X(8).                                    
004600*                                 ANVÄNDAR-ID PRINTN. 1                   
004700*                                 USER ID FIRST PRINTING                  
004800     03 INVH-IDUSER-PR2      PIC X(8).                                    
004900*                                 ANVÄNDAR-ID PRINTN. 2                   
005000*                                 USER ID SECOND PRINTING                 
005100     03 INVH-IDUSER-PR3      PIC X(8).                                    
005200*                                 ANVÄNDAR-ID PRINTN. 3                   
005300*                                 USER ID THIRD PRINTING                  
005400     03 INVH-IDUSER-CRE      PIC X(8).                                    
005500*                                 ANVÄNDAR-ID VID START                   
005600*                                 USER ID AT START                        
005700     03 INVH-KVANTAL         PIC S9(7)           COMP-3.                  
005800*                                 ANTAL                                   
005900*                                 NUMBER                                  
006000     03 INVH-FILLER          PIC X(10).                                   
006100*** END OF VILMAII-COPY LENGTH= 120 BYTES                                 
