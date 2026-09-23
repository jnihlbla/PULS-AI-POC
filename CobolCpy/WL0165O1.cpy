000100 01  RESP-WL0165O1.                                                       
000200*                                 RESPONS FROM PGM WL0165                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 RESP-IDORDNR-KEY     PIC Z(4)9.                                   
001300*                                 ORDERNUMMER                             
001400*                                 ORDER NUMBER                            
001500     03 RESP-KDORDKL-KEY     PIC 9.                                       
001600*                                 ORDERKLASS                              
001700*                                 ORDER CLASS                             
001800     03 RESP-KDFRAKT-KEY     PIC Z9.                                      
001900*                                 FRAKTSÄTT DC TILL KUND                  
002000*                                 FREIGHT CODE                            
002100     03 RESP-IDFTG           PIC 9(2).                                    
002200*                                 FÖRETAGSID EKONOM REDOVISNING           
002300*                                 COMPANY IDENTITY ACCOUNTING             
002400     03 RESP-IDKONTO         PIC Z(9)9.                                   
002500*                                 KONTO                                   
002600*                                 ACCOUNT                                 
002700     03 RESP-IDANALYS        PIC X(12).                                   
002800*                                 ANALYSNUMMER                            
002900*                                 ANALYSIS NUMBER                         
003000     03 RESP-IDKST           PIC X(10).                                   
003100*                                 KOSTNADSSTÄLLE                          
003200*                                 COST CENTRE                             
003300     03 RESP-IDDISTR-JUST    PIC Z(3)9.                                   
003400*                                 DISTRIKT FÖR JUSTERINGSORDER            
003500*                                 JUSTIFY ORDER DISTRICT                  
003600     03 RESP-IDKUNDNR-JUST   PIC Z(5)9.                                   
003700*                                 KUND FÖR JUSTERINGSORDER                
003800*                                 JUSTIFY ORDER CUSTOMER                  
003900     03 RESP-IDDISTR-MIX     PIC Z(3)9.                                   
004000*                                 DISTRIKT JUSTERING/BLAND.ART.           
004100*                                 JUSTIFY ORDER WITH MIXED PARTS          
004200     03 RESP-IDKUNDNR-MIX    PIC Z(5)9.                                   
004300*                                 KUND JUSTERING/BLAND.ART.               
004400*                                 JUSTIFY ORDER WITH MIXED PARTS          
004500     03 RESP-IDDISTR-QSKROT  PIC Z(3)9.                                   
004600*                                 KVALITET SKROT DISTRIKT                 
004700*                                 QUALITY SCRAP DISTRICT                  
004800     03 RESP-IDKUNDNR-QSKROT PIC Z(5)9.                                   
004900*                                 KUNDNUMMER FÖR KVALITET SKROT           
005000*                                 CUSTOMER NO FOR QUALITY SCRAP           
005100     03 RESP-IDDISTR-RSKROT  PIC Z(3)9.                                   
005200*                                 SKROT DISTRIKT FÖR RETURER              
005300*                                 SCRAP DISTRICT FOR RETURNS              
005400     03 RESP-IDKUNDNR-RSKROT PIC Z(5)9.                                   
005500*                                 KUNDNUMMER FÖR SKROT AV RETUR           
005600*                                 CUSTOMER NO SCRAP OF RETURNS            
005700     03 RESP-IDDISTR-LRENOV  PIC Z(3)9.                                   
005800*                                 LOKAL RENOVÖRS DISTRIKT                 
005900*                                 LOCAL REMANUFACT DISTRICT               
006000     03 RESP-IDKUNDNR-LRENOV PIC Z(5)9.                                   
006100*                                 KUNDNR FÖR LOKAL RENOVÖR                
006200*                                 LOCAL REMANUFACT CUSTOMER NO.           
006300     03 RESP-IDDISTR-BKONV   PIC Z(3)9.                                   
006400*                                 BYTESKONVERTERING DISTRIKT              
006500*                                 EXCHANGE CONVERSION DISTR               
006600     03 RESP-IDKUNDNR-BKONV  PIC Z(5)9.                                   
006700*                                 KUND FÖR BYTESKONV. ORDER               
006800*                                 EXCHANGE CONVERSION CUSTOMER            
006900     03 RESP-IDDISTR-OSKROT  PIC Z(3)9.                                   
007000*                                 OBJEKT SKROTDISTRIKT                    
007100*                                 CORE SCRAP DISTRICT                     
007200     03 RESP-IDKUNDNR-OSKROT PIC Z(5)9.                                   
007300*                                 KUNDNR FÖR OBJEKTSKROTNING              
007400*                                 CUSTOMER NO FOR CORE SCRAP              
007500*** END OF VILMAII-COPY LENGTH= 124 BYTES                                 
