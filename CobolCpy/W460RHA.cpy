000100 01  OHUV-W460RHA.                                                        
000200*                                 ORDERHEAD-TRANS. FROM VIPS TO           
000300*                                 NOAC.    RECORD TYPE  RHA               
000400     03 OHUV-IDPTYP          PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 OHUV-IDDISTR         PIC 9(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 OHUV-IDKUNDNR        PIC 9(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 OHUV-IDORDNR         PIC 9(7).                                    
001100*                                 ORDER NUMBER        IDORDNR-002         
001200     03 OHUV-BEVOLREF        PIC X(10).                                   
001300*                                 VOLVO REFERENCE                         
001400     03 OHUV-KDFRAKT         PIC 9(2).                                    
001500*                                 FREIGHT CODE                            
001600     03 OHUV-KDORDKL         PIC 9.                                       
001700*                                 ORDER CLASS                             
001800     03 OHUV-KDORDKL-IMP     PIC 9.                                       
001900*                                 ORDER CLASS FROM IMPORTER               
002000     03 OHUV-KDROPACK        PIC X.                                       
002100*                                 CONSOLIDATION BO/DO                     
002200     03 OHUV-KDORDURS        PIC X.                                       
002300*                                 ORDER ORIGIN                            
002400     03 OHUV-BEVARREF        PIC X(10).                                   
002500*                                 OUR REFERENCE                           
002600     03 OHUV-FLRESTN         PIC X.                                       
002700*                                 BACKORDERED ?                           
002800     03 OHUV-KDNCNOT         PIC X(2).                                    
002900*                                 CODE FOR ORDER ENTRY NOTES              
003000     03 OHUV-KDTPOTYP        PIC 9.                                       
003100*                                 TYPE OF TIME PLANNED ORDER              
003200     03 OHUV-IDKAMPRF        PIC 9(7).                                    
003300*                                 CAMPAIGN REFERENCE                      
003400     03 OHUV-BEGMT.                                                       
003500*                                 GOODS RECEIVER NAME                     
003600        05 OHUV-BEGMT-RAD1   PIC X(35).                                   
003700*                                 GOODS RECEIVER NAME LINE 1              
003800        05 OHUV-BEGMT-RAD2   PIC X(35).                                   
003900*                                 GOODS RECEIVER NAME LINE 2              
004000     03 OHUV-ADGMT.                                                       
004100*                                 GOODS RECEIVER ADDRESS                  
004200        05 OHUV-ADGMT-GATA   PIC X(35).                                   
004300*                                 GOODS RECEIVER ADDRESS STREET           
004400        05 OHUV-ADGMT-PADR   PIC X(35).                                   
004500*                                 GOODS RECEIVER ADDRESS TOWN             
004600        05 OHUV-ADPOST-PNRORT REDEFINES OHUV-ADGMT-PADR.                  
004700*                                 POSTAL CODE + CITY                      
004800           07 OHUV-ADPOSTNR  PIC X(10).                                   
004900*                                 POSTAL CODE IN ADDRESS                  
005000           07 OHUV-ADCITY    PIC X(25).                                   
005100*                                 CITY                                    
005200        05 OHUV-ADPOST-ORTPNR REDEFINES OHUV-ADGMT-PADR.                  
005300*                                 CITY + POSTAL CODE                      
005400           07 OHUV-ADCITY    PIC X(25).                                   
005500*                                 CITY                                    
005600           07 OHUV-ADPOSTNR  PIC X(10).                                   
005700*                                 POSTAL CODE IN ADDRESS                  
005800        05 OHUV-ADGMT-LAND   PIC X(35).                                   
005900*                                 GOODS RECEIVER ADDRESS COUNTRY          
006000     03 OHUV-TIBEGPAC        PIC 9(6).                                    
006100*                                 REQUESTED PACKING DATE (YYMMDD)         
006200*** END OF VILMAII-COPY LENGTH= 238 BYTES                                 
