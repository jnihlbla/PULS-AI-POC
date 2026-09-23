000100 01  4546-WDGX4546.                                                       
000200*                                 DHL KOLLIETIKETTER                      
000300*                                 UTSKRIFTS REGISTER                      
000400*                                 FYSISK NYCKEL: KY4546                   
000500*                                 (IDPRODNR + IDKOLLI                     
000600     03 4546-IDPRODNR        PIC S9(7)           COMP-3.                  
000700*                                 PRODUKTIONSNUMMER                       
000800*                                 PRODUCTION NUMBER                       
000900     03 4546-IDKOLLI         PIC S9(5)           COMP-3.                  
001000*                                 KOLLINUMMER                             
001100*                                 CASE NUMBER                             
001200     03 4546-IDKUNDNR        PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500     03 4546-IDORDNR5        PIC 9(5).                                    
001600*                                 ORDERNUMMER                             
001700*                                 ORDER NUMBER                            
001800     03 4546-BEGMT-RAD1      PIC X(35).                                   
001900*                                 GODSMOTTAGARNAMN RAD 1                  
002000*                                 GOODS RECEIVER NAME LINE 1              
002100     03 4546-BEGMT-RAD2      PIC X(35).                                   
002200*                                 GODSMOTTAGARNAMN RAD 2                  
002300*                                 GOODS RECEIVER NAME LINE 2              
002400     03 4546-ADGMT-GATA      PIC X(35).                                   
002500*                                 GODSMOTTAGARADRESS GATA                 
002600*                                 GOODS RECEIVER ADDRESS STREET           
002700     03 4546-ADGMT-PADR      PIC X(35).                                   
002800*                                 GODSMOTTAGARADRESS POSTADRESS           
002900*                                 GOODS RECEIVER ADDRESS TOWN             
003000     03 4546-ADGMT-LAND      PIC X(35).                                   
003100*                                 GODSMOTTAGARADRESS LAND                 
003200*                                 GOODS RECEIVER ADDRESS COUNTRY          
003300     03 4546-IDCITY          PIC X(3).                                    
003400*                                 3-STÄLLIG ORTSBETECKNINGSKOD            
003500*                                 3-LETTER CODE FOR CITY                  
003600     03 4546-IDKONTO         PIC 9(10).                                   
003700*                                 KONTO                                   
003800*                                 ACCOUNT                                 
003900     03 4546-IDAWB           PIC 9(9).                                    
004000*                                 FLYGFRAKTSNUMMER TILL DHL               
004100*                                 AIRWAYBILL NUMBER FOR DHL               
004200     03 4546-REKSIFFR-AWB    PIC S9              COMP-3.                  
004300*                                 KONTR.SIFFRA DHL-FLYGFRAKTSNR.          
004400*                                 CHECK DIGIT FOR AIRWAYBILL NO.          
004500     03 4546-KDORDKL         PIC S9              COMP-3.                  
004600*                                 ORDERKLASS                              
004700*                                 ORDER CLASS                             
004800     03 4546-VKORDBTO        PIC S9(6)V9(1)      COMP-3.                  
004900*                                 ORDERVIKT BRUTTO (KG)                   
005000*                                 GROSS WEIGHT (KG)                       
005100     03 4546-VLORDBTO        PIC S9(4)V9(3)      COMP-3.                  
005200*                                 ORDERVOLYM BRUTTO (M3)                  
005300*                                 GROSS VOLUME PER ORDER (M3)             
005400     03 4546-DIKOLLIL        PIC S9(5)           COMP-3.                  
005500*                                 KOLLI-LÄNGD                             
005600*                                 CASE LENGTH                             
005700     03 4546-DIKOLLIB        PIC S9(3)           COMP-3.                  
005800*                                 KOLLI-BREDD                             
005900*                                 CASE WIDTH                              
006000     03 4546-DIKOLLIH        PIC S9(3)           COMP-3.                  
006100*                                 KOLLI-HÖJD                              
006200*                                 CASE HEIGHT                             
006300*** END OF VILMAII-COPY LENGTH= 230 BYTES                                 
