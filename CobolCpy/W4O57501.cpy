000100 01  MOD-W4O57501.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O57501                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-TIORDREG-IN      PIC X(6).                                    
001700*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
001800     03 MOD-TIORDREG-UT      PIC X(6).                                    
001900*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002000     03 MOD-KDORDKL-IN       PIC X.                                       
002100*                                 ORDERKLASS                              
002200     03 MOD-KDORDKL-UT       PIC X.                                       
002300*                                 ORDERKLASS                              
002400     03 MOD-RAD              OCCURS 14 TIMES.                             
002500*                                  RAD FÖR FÖRÄNDRINGAR                   
002600*                                                                         
002700        05 MOD-BILDNR-ATTR   PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-BILDNR        PIC X(4).                                    
003000*                                 BILDNUMMER                              
003100        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
003200*                                 KUNDNUMMER                              
003300        05 MOD-IDORDNR       PIC Z(5).                                    
003400*                                 ORDERNUMMER                             
003500        05 MOD-IDARTNR       PIC Z(8)9.                                   
003600*                                 ARTIKELNUMMER                           
003700        05 MOD-KVRO          PIC Z(5)9.                                   
003800*                                 ANTAL RESTNOTERADE ARTIKLAR             
003900        05 MOD-BERADREF      PIC X(10).                                   
004000*                                 KUNDENS RADREFERENS                     
004100        05 MOD-TIORDREG      PIC 9(6).                                    
004200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
004300        05 MOD-IDKUNDRF-RO   PIC X(10).                                   
004400*                                 KUND REF PÅ RO                          
004500     03 MOD-TEMFSINF         PIC X(55).                                   
004600*                                 INFORMATIONSMEDDELANDE                  
004700*** END OF VILMAII-COPY LENGTH= 945 BYTES                                 
