000010*** EDIT ALLOWED                                                          
000100 01  W4754R.                                                              
000200*                                 INVOICE LINE AND CREDIT LINE            
000300*                                 INFORMATION TO SDC 26, AUSTRIA.         
000400*                                 CUSTOMS INFORMATION FILE                
000500*                                                                         
000600     03 IDFTG                PIC 9(2)           VALUE 78.                 
000800*                                 COMPANY CODE                            
000810*                                 (MANDANT)                               
000900     03 IDWERK               PIC 9(2)           VALUE 01.                 
001100*                                 FACTORY                                 
001110*                                 (WERK)                                  
001200     03 IDFAKT               PIC X(15).                                   
001400*                                 INVOICE NO.                             
001410*                                 (KOPFNUMMER)                            
001500     03 IDRADNR              PIC 9(4).                                    
001700*                                 SEQUENCE NO. WITHIN INVOICE             
001710*                                 (POSITIONSNUMMER)                       
001800     03 TIFAKT               PIC 9(6).                                    
002000*                                 INVOICE DATE YYMMDD                     
002100*                                 (VERZOLLUNGSDATUM)                      
002200     03 KDARTURS             PIC X(3).                                    
002400*                                 COUNTRY OF ORIGIN                       
002410*                                 (URSRPUNGSLAND)                         
002420     03 IDLANDX3-HANDEL      PIC X(3)           VALUE 'SE '.              
002430*                                 COMMERCIAL COUNTRY                      
002440*                                 (HANDELSLAND)                           
002450     03 IDLANDX3-SENDER      PIC X(3)           VALUE 'SE '.              
002460*                                 SENDING COUNTRY                         
002470*                                 (HERKUNFTSLAND)                         
002480     03 IDLEVNR              PIC X(15)   VALUE '00071          '.         
002490*                                 SUPPLIER NO.                            
002491*                                 (LIEFERANTENNUMMER)                     
002492     03 VKORDBTO-FAKT        PIC 9(9)V9(2).                               
002493*                                 GROSS WEIGHT PER INVOICE                
002494*                                 (ROHMASSE IN KG)                        
002495     03 SUORDV-FAKT-LOC      PIC 9(11)V9(2).                              
002496*                                 INVOICED ORDER VALUE LOCAL CURR         
002497*                                 (LIEFERWERT IN WÄHRUNG, ATS)            
002500     03 KDVALISO             PIC X(3)           VALUE 'ATS'.              
002700*                                 CURRENCY CODE                           
002710*                                 (WÄHRUNG ZU LIEFERWERT)                 
002800     03 KDBETVIL             PIC X(3)           VALUE 'DDP'.              
003000*                                 TERMS OF PAYMENT                        
003100*                                 (LIEFERBEDINGSUNGSCODE)                 
003600     03 IDKUNDRF             PIC X(20).                                   
003800*                                 ORDER NUMBER 7 POS                      
003810*                                 (BESTELLNUMMER)                         
003900     03 IDARTNR              PIC X(20).                                   
004100*                                 PART NUMBER, RIGHT ADJUSTED             
004110*                                 (MATERIALNUMMER)                        
004120     03 VKLEV-RAD            PIC s9(7)V9(3).                              
004130*                                 NET WEIGHT * DEL QTY PER LINE           
004140*                                 (VERZOLLUNGSGEWICHT IN KG)              
004150     03 VKLEV2-RAD           PIC s9(7)V9(3).                              
004160*                                 NET WEIGHT * DEL QTY PER LINE           
004170*                                 (EIGENMASSE IN KG)                      
004600     03 KVLEVART             PIC s9(9)V9(2).                              
004700*                                 DELIVERED QUANTITY                      
004800*                                 (MENGE)                                 
004801     03 KDSORT               PIC X(4).                                    
004802*                                 PARTS UNIT                              
004803*                                 (MENGENEINHEIT)                         
004810     03 SUFKTUTL-RAD         PIC s9(11)V9(2).                             
004820*                                 LINE PRICE IN LOCAL CURR                
004830*                                 (POSITIONSWERT IN WÄHRUNG)              
004900     03 KDVALISO-RAD         PIC X(3)           VALUE 'ATS'.              
005000*                                 CURRENCY CODE                           
005100*                                 (WÄHRUNG ZU LIEFERWERT)                 
005110     03 KOD-ZEICHEN          PIC X(1)           VALUE 'K'.                
005120*                                 ???                                     
005130*                                 (BESTANDSFüRUNGSKENNZEICHEN)            
005140     03 KOD-BESTART          PIC X(6)           VALUE 'DR    '.           
005150*                                 ???                                     
005160*                                 (BESTANDSART)                           
005170     03 KOD-EINFUHR          PIC X(6)           VALUE 'F     '.           
005180*                                 ???                                     
005190*                                 (EINFUHRVERFAHRENSKüRZEL)               
005191     03 KOD-GRENZE           PIC X(2)           VALUE '3 '.               
005192*                                 ???                                     
005193*                                 (VERKEHRSZWEIG/GRENZE)                  
005194     03 KOD-INTRA            PIC X(2)           VALUE '3 '.               
005195*                                 KIND OF DELIVERY                        
005196*                                 (GESCHÄFTSART)                          
005197     03 KOD-KZ               PIC X(1)           VALUE 'B '.               
005198*                                 ???                                     
005199*                                 (KZ I/B)                                
005200*** END OF VILMAII-COPY LENGTH= 175 BYTES                                 
