000100 01  LOG-W414LOGG.                                                        
000200*                                 LOGG NDC-ORDER CLASS 1                  
000300*                                 SKAPAS I W411SDCA OM                    
000400*                                 ORDERRADEN FLYTTAS TILL                 
000500*                                 ANNAT LAGER                             
000600     03 LOG-TIREGDAT         PIC 9(6).                                    
000700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000800     03 LOG-TIREGTID         PIC 9(6).                                    
000900*                                 REGISTRERINGSTID                        
001000     03 LOG-IDARTNR          PIC 9(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 LOG-KVBEART-Q        PIC 9(7).                                    
001300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
001400     03 LOG-IDDC             PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 LOG-KVLS             PIC -(6)9.                                   
001700*                                 LAGERSALDO                              
001800     03 LOG-KVDISP           PIC -(6)9.                                   
001900*                                 DISPONIBELT LAGER                       
002000     03 LOG-KVAKS-SDC        PIC -(7)9.                                   
002100*                                 DEL AV AK SOM LIGGER I SDC              
002200     03 LOG-KVAKS-PAV        PIC -(7)9.                                   
002300*                                 DEL AV AK PÅ VÄG                        
002400     03 LOG-KVOKS-BULK       PIC -(6)9.                                   
002500*                                 ORDERKÖSALDO, KLASS 2-4                 
002600     03 LOG-KVOKS-DAG        PIC -(6)9.                                   
002700*                                 ORDERKÖSALDO, KLASS 1                   
002800     03 LOG-TIBUFF           PIC 9(6).                                    
002900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003000     03 LOG-KVBEART-BUFF     PIC 9(7).                                    
003100*                                 BESTÄLLT ANTAL STYCKEN                  
003200     03 LOG-KVLS-AVAILABLE   PIC -(6)9.                                   
003300*                                 LAGERSALDO                              
003400     03 LOG-KVLS-ON-HAND     PIC -(6)9.                                   
003500*                                 LAGERSALDO                              
003600     03 LOG-IDDC-REF         PIC X(2).                                    
003700*                                 SÄNDANDE LAGER FÖR REFILL               
003800     03 LOG-KVDISP-REF       PIC -(6)9.                                   
003900*                                 DISPONIBELT LAGER                       
004000     03 LOG-KVOKS-BULK-REF   PIC -(6)9.                                   
004100*                                 ORDERKÖSALDO, KLASS 2-4                 
004200     03 LOG-KVOKS-DAG-REF    PIC -(6)9.                                   
004300*                                 ORDERKÖSALDO, KLASS 1                   
004400     03 LOG-FLLEVOK          PIC X.                                       
004500*                                 JA/NEJ-FLAGGA                           
004600*** END OF VILMAII-COPY LENGTH= 125 BYTES                                 
