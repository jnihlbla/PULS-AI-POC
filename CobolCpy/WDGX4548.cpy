000100 01  4548-WDGX4548.                                                       
000200*                                 TNT KOLLIETIKETTER                      
000300*                                 UTSKRIFTS REGISTER                      
000400*                                 FYSISK NYCKEL: KY4548                   
000500*                                 (IDPRODNR + IDKOLLI                     
000600     03 4548-IDPRODNR        PIC S9(7)           COMP-3.                  
000700*                                 PRODUKTIONSNUMMER                       
000800*                                 PRODUCTION NUMBER                       
000900     03 4548-IDKOLLI         PIC S9(5)           COMP-3.                  
001000*                                 KOLLINUMMER                             
001100*                                 CASE NUMBER                             
001200     03 4548-IDDISTR         PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 4548-IDKUNDNR        PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800     03 4548-IDORDNR5        PIC 9(5).                                    
001900*                                 ORDERNUMMER                             
002000*                                 ORDER NUMBER                            
002100     03 4548-ADGMT-GATA      PIC X(35).                                   
002200*                                 GODSMOTTAGARADRESS GATA                 
002300*                                 GOODS RECEIVER ADDRESS STREET           
002400     03 4548-ADGMT-PADR      PIC X(35).                                   
002500*                                 GODSMOTTAGARADRESS POSTADRESS           
002600*                                 GOODS RECEIVER ADDRESS TOWN             
002700     03 4548-ADGMT-LAND      PIC X(35).                                   
002800*                                 GODSMOTTAGARADRESS LAND                 
002900*                                 GOODS RECEIVER ADDRESS COUNTRY          
003000     03 4548-BEGMT-RAD1      PIC X(35).                                   
003100*                                 GODSMOTTAGARNAMN RAD 1                  
003200*                                 GOODS RECEIVER NAME LINE 1              
003300     03 4548-BEGMT-RAD2      PIC X(35).                                   
003400*                                 GODSMOTTAGARNAMN RAD 2                  
003500*                                 GOODS RECEIVER NAME LINE 2              
003600     03 4548-DIKOLLIL        PIC S9(5)           COMP-3.                  
003700*                                 KOLLI-LÄNGD                             
003800*                                 CASE LENGTH                             
003900     03 4548-DIKOLLIB        PIC S9(3)           COMP-3.                  
004000*                                 KOLLI-BREDD                             
004100*                                 CASE WIDTH                              
004200     03 4548-DIKOLLIH        PIC S9(3)           COMP-3.                  
004300*                                 KOLLI-HÖJD                              
004400*                                 CASE HEIGHT                             
004500     03 4548-IDKONTO         PIC 9(10).                                   
004600*                                 KONTO                                   
004700*                                 ACCOUNT                                 
004800     03 4548-IDKLITNT        PIC 9(8).                                    
004900*                                 NR.SERIE FÖR TNT-KOLLIN                 
005000*                                 SERIAL NO. FOR TNT CASES                
005100     03 4548-KDORDKL         PIC S9              COMP-3.                  
005200*                                 ORDERKLASS                              
005300*                                 ORDER CLASS                             
005400     03 4548-REKSIFFR-TNT    PIC S9              COMP-3.                  
005500*                                 KONTR.SIFFRA TNT KOLLI NR.              
005600*                                 CHECK DIGIT FOR TNT CASE NO.            
005700     03 4548-TITIDTNT        PIC X(5).                                    
005800*                                 SENAST ANKOMST TNT (HH:MM)              
005900*                                 LATEST ARRIVAL TNT (HH:MM)              
006000     03 4548-VKORDBTO        PIC S9(6)V9(1)      COMP-3.                  
006100*                                 ORDERVIKT BRUTTO (KG)                   
006200*                                 GROSS WEIGHT (KG)                       
006300     03 4548-VLORDBTO        PIC S9(4)V9(3)      COMP-3.                  
006400*                                 ORDERVOLYM BRUTTO (M3)                  
006500*                                 GROSS VOLUME PER ORDER (M3)             
006600     03 4548-FILLER          PIC X(3).                                    
006700*** END OF VILMAII-COPY LENGTH= 237 BYTES                                 
