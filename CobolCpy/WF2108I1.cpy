000100 01  WF2108I1.                                                            
000200*                                 FEEDBACK DATA TO SYSTEM PULS-CU         
000300*                                 STOMS                                   
000400     03 DAEXDAT              PIC 9(8).                                    
000500*                                 EXEKVERINGSDATUM (≈≈≈≈MMDD)             
000600*                                 EXECUTION DATE (YYYYMMDD)               
000700     03 TIEXTID              PIC 9(6).                                    
000800*                                 EXEKVERINGSTIDPUNKT                     
000900*                                 EXECUTION TIME                          
001000     03 IDPTYP               PIC X(3).                                    
001100*                                 POSTTYP                                 
001200*                                 RECORD TYPE                             
001300     03 CUSTOMS-DATA.                                                     
001400        05 IDLANDX3-BET      PIC X(3).                                    
001500*                                 LANDKOD BETALANDE KUND ETC              
001600*                                 COUNTRY CODE PAYING CUSTOMER ET         
001700*                                 C                                       
001800        05 IDLANDX3-SEND     PIC X(3).                                    
001900*                                 LANDKOD SƒNDANDE LAND                   
002000*                                 COUNTRY CODE SENDING COUNTRY            
002100        05 IDPARTNR          PIC X(9).                                    
002200*                                 PARTNERNUMMER                           
002300*                                 PARTNER NO                              
002400        05 KDFINDOC          PIC X(4).                                    
002500*                                 TYP FINANSIELLT DOKUMENT                
002600*                                 FINANCIAL DOCUMENT TYPE                 
002700        05 DAFINDOC          PIC 9(8).                                    
002800*                                 DOKUMENT DATUM (≈≈≈≈MMDD)               
002900*                                 INVOICING DATE   (YYYYMMDD)             
003000        05 IDFINDOC          PIC 9(9).                                    
003100*                                 FINANSIELLT DOKUMENT ID                 
003200*                                 FINANCIAL DOCUMENT ID                   
003300        05 IDEXCUST-1        PIC X(15).                                   
003400*                                 EXTERNT KUNDID                          
003500*                                 EXTERNAL CUSTOMER ID                    
003600        05 IDARTNR-FINANCE   PIC X(50).                                   
003700*                                 ARTIKELNUMMER F÷R FINANSIELL BR         
003800*                                 UK                                      
003900*                                 PART NUMBER FOR FINANCIAL USE           
004000        05 BEART             PIC X(25).                                   
004100*                                 ARTIKELBENƒMNING                        
004200*                                 PART DESCRIPTION                        
004300        05 KVLEVART          PIC 9(7).                                    
004400*                                 LEVERERAT ANTAL STYCK                   
004500*                                 DELIVERED QUANTITY                      
004600*** END OF VILMAII-COPY LENGTH= 150 BYTES                                 
