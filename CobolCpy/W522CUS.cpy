000100 01  W522CUS.                                                             
000200*                                 CUSTOMS DATA FOR DISTRIBUTION           
000300     03 TIAA                 PIC 9(2).                                    
000400*                                 ÅR    (ÅÅ)                              
000500*                                 YEAR  (YY)                              
000600     03 TIRP                 PIC 9(2).                                    
000700*                                 REDOVISNINGSPERIOD                      
000800*                                 12 PER ÅR                               
000900*                                 ACCOUNTING PERIOD                       
001000*                                 12 PER YEAR                             
001100     03 IDLANDX3-BET         PIC X(3).                                    
001200*                                 LANDKOD BETALANDE KUND ETC              
001300*                                 COUNTRY CODE PAYING CUSTOMER ET         
001400*                                 C                                       
001500     03 IDLANDX3-SEND        PIC X(3).                                    
001600*                                 LANDKOD SÄNDANDE LAND                   
001700*                                 COUNTRY CODE SENDING COUNTRY            
001800     03 IDPARTNR             PIC X(9).                                    
001900*                                 PARTNERNUMMER                           
002000*                                 PARTNER NO                              
002100     03 KDFINDOC             PIC X(4).                                    
002200*                                 TYP FINANSIELLT DOKUMENT                
002300*                                 FINANCIAL DOCUMENT TYPE                 
002400     03 DAFINDOC             PIC 9(8).                                    
002500*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
002600*                                 INVOICING DATE   (YYYYMMDD)             
002700     03 IDFINDOC             PIC 9(9).                                    
002800*                                 FINANSIELLT DOKUMENT ID                 
002900*                                 FINANCIAL DOCUMENT ID                   
003000     03 IDDISTR              PIC 9(5).                                    
003100*                                 DISTRIKTNUMMER                          
003200*                                 DISTRICT NUMBER                         
003300     03 IDARTNR              PIC 9(9).                                    
003400*                                 ARTIKELNUMMER                           
003500*                                 PART NUMBER                             
003600     03 BEART                PIC X(25).                                   
003700*                                 ARTIKELBENÄMNING                        
003800*                                 PART DESCRIPTION                        
003900     03 KVLEVART             PIC 9(7).                                    
004000*                                 LEVERERAT ANTAL STYCK                   
004100*                                 DELIVERED QUANTITY                      
004200*** END OF VILMAII-COPY LENGTH= 86 BYTES                                  
