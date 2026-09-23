000100 01  CARR-W403CARR.                                                       
000200*                                 3IV CARRIER SUMMARY POST                
000300*                                 3IV CARRIER SUMMARY RECORD              
000400*                                 IDRTYP3IV="CarrierSummary"              
000500     03 CARR-IDRTYP3IV       PIC X(30).                                   
000600*                                 3IV RECORD-TYP                          
000700*                                 3IV RECORD TYPE                         
000800     03 CARR-IDPRODNR        PIC 9(7).                                    
000900*                                 PRODUKTIONSNUMMER                       
001000*                                 PRODUCTION NUMBER                       
001100     03 CARR-IDPLKLST        PIC 9(3).                                    
001200*                                 PLOCKLISTNUMMER                         
001300*                                 PICKING LIST NUMBER                     
001400     03 CARR-IDPRC.                                                       
001500*                                 PRODUKTIONSKANAL                        
001600*                                 PRODUCTION CHANNEL                      
001700        05 CARR-IDPRCBAS     PIC X(3).                                    
001800*                                 PRC-BAS                                 
001900*                                 PRC-BASIC                               
002000        05 CARR-IDPRCVAR     PIC X.                                       
002100*                                 PRC-VARIANT                             
002200*                                 PRC-VARIANT                             
002300     03 CARR-IDLOPNR-ORD     PIC 9(3).                                    
002400*                                 ORDERNS ORDNINGSNUMMER INOM             
002500*                                 EN PLOCKSATS                            
002600*                                 SEQUENCE-NUMBER FOR AN ORDER            
002700*                                 WITHIN A PICKING UNIT                   
002800     03 CARR-FLSISTAK        PIC 9.                                       
002900*                                 SISTA KOLLI I ORDERN?                   
003000*                                 LAST CASE IN ORDER?                     
003100     03 CARR-KDEMBTYP        PIC 9.                                       
003200*                                 EMBALLAGETYP                            
003300*                                 PACKAGE TYPE                            
003400     03 CARR-KDKOLLI         PIC X(8).                                    
003500*                                 KOLLIKOD                                
003600*                                 KOLLI CODE                              
003700     03 CARR-DIKOLLIH        PIC 9(3).                                    
003800*                                 KOLLI-HÖJD                              
003900*                                 CASE HEIGHT                             
004000     03 CARR-DIKOLLIL        PIC 9(4).                                    
004100*                                 KOLLI-LÄNGD                             
004200*                                 CASE LENGTH                             
004300     03 CARR-DIKOLLIB        PIC 9(3).                                    
004400*                                 KOLLI-BREDD                             
004500*                                 CASE WIDTH                              
004600*** END OF VILMAII-COPY LENGTH= 67 BYTES                                  
