000100 01  MOD-W4O22701.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDANSK-IN        PIC X(3).                                    
000800*                                 ANSKAFFARNUMMER                         
000900     03 MOD-IDANSK-UT        PIC X(3).                                    
001000*                                 ANSKAFFARNUMMER                         
001100     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 MOD-IDARTNR-IN       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-RAD              OCCURS 14 TIMES.                             
002000*                                 GRUPP MED RADER                         
002100        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-KDCMDVAL      PIC X(3).                                    
002400*                                 GENERELL KOMMANDOKOD                    
002500        05 MOD-TIREGDAT-URSP PIC 9(6).                                    
002600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002700        05 MOD-TEVORMRK      PIC X(2).                                    
002800*                                 MÄRKNINGSTEXT FÖR                       
002900*                                 VOR-KÖN                                 
003000        05 MOD-IDDISTR       PIC Z(3)9.                                   
003100*                                 DISTRIKTNUMMER                          
003200        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
003300*                                 KUNDNUMMER                              
003400        05 MOD-IDORDNR5      PIC Z(4)9.                                   
003500*                                 ORDERNUMMER                             
003600        05 MOD-IDLEVNR       PIC X(5).                                    
003700*                                 LEVERANTÖRNUMMER                        
003800        05 MOD-IDARTNR       PIC Z(8)9.                                   
003900*                                 ARTIKELNUMMER                           
004000        05 MOD-KVBEART-URSP  PIC Z(6)9.                                   
004100*                                 BESTÄLLT ANTAL STYCKEN                  
004200        05 MOD-KVBEART       PIC Z(6)9.                                   
004300*                                 BESTÄLLT ANTAL STYCKEN                  
004400        05 MOD-KDORDBEK      PIC 9(2).                                    
004500*                                 ORDERBEKRÄFTELSEKOD                     
004600        05 MOD-IDDC          PIC X(2).                                    
004700*                                 IDENTIFIERARE LAGER                     
004800        05 MOD-FLAGGA-VOR    PIC X.                                       
004900*                                 ALLMÄN FLAGGA                           
005000        05 MOD-FLAGGA-SC     PIC X.                                       
005100*                                 ALLMÄN FLAGGA                           
005200        05 MOD-FLAGGA-ANSK   PIC X.                                       
005300*                                 ALLMÄN FLAGGA                           
005400        05 MOD-FLAGGA-LOSN   PIC X.                                       
005500*                                 ALLMÄN FLAGGA                           
005600     03 MOD-TEMFSINF         PIC X(55).                                   
005700*                                 INFORMATIONSMEDDELANDE                  
005800*** END OF VILMAII-COPY LENGTH= 1029 BYTES                                
