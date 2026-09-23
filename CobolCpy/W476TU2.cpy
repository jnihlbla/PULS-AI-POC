000100 01  TU2-W476TU2-CTX.                                                     
000200*                                 TULLSYSTEM                              
000300*                                 TULL RAD-UPPGIFTER PTYP TU2             
000400     03 TU2-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 TU2-IDFAKT           PIC S9(7)           COMP-3.                  
000700*                                 FAKTURANUMMER                           
000800     03 TU2-IDTULL.                                                       
000900*                                 IDENTITET TULL SÄNDNING                 
001000        05 TU2-IDTULFTG      PIC X(2).                                    
001100*                                 IDENTIFIERARE TULLANDE FÖRETAG          
001200*                                                                         
001300        05 TU2-IDTULLNR      PIC 9(7).                                    
001400*                                 NUMMERSERIE INGÅENDE I TULLID           
001500*                                                                         
001600        05 TU2-RETULKS       PIC 9.                                       
001700*                                 KONTROLLSIFFRA TULLID                   
001800     03 TU2-IDSTATNR         PIC S9(9)           COMP-3.                  
001900*                                 STATISTISKT NUMMER                      
002000*                                 1 = NORSKT                              
002100*                                 2 = ENGELSKT                            
002200*                                 3 = BELGISKT                            
002300*                                 4 = PERUANSKT                           
002400*                                 5 = SVENSKT                             
002500*                                 6 =                                     
002600     03 TU2-IDPRODNR         PIC S9(7)           COMP-3.                  
002700*                                 PRODUKTIONSNUMMER                       
002800     03 TU2-IDKOLLI          PIC S9(5)           COMP-3.                  
002900*                                 KOLLINUMMER                             
003000     03 TU2-IDARTNR          PIC S9(9)           COMP-3.                  
003100*                                 ARTIKELNUMMER                           
003200     03 TU2-KDARTURS         PIC X(2).                                    
003300*                                 ARTIKELURSPRUNGSKOD                     
003400     03 TU2-KVLEVART         PIC S9(7)           COMP-3.                  
003500*                                 LEVERERAT ANTAL STYCK                   
003600     03 TU2-KDVALISO         PIC X(3).                                    
003700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003800     03 TU2-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
003900*                                 ARTIKELPRIS NETTO                       
004000     03 TU2-SUARTNTO         PIC S9(9)V9(2)      COMP-3.                  
004100*                                 SUMMA RADVÄRDE TILL NETTOPRIS           
004200     03 TU2-VKRAD-NTO-KG     PIC S9(6)V9(3)      COMP-3.                  
004300*                                 ARTIKELVIKT NETTO RADVÄRDE              
004400     03 TU2-KDSORT           PIC X(2).                                    
004500*                                 SORT-KOD                                
004600     03 TU2-KDEMBTYP         PIC S9              COMP-3.                  
004700*                                 EMBALLAGETYP                            
004800     03 TU2-IDKOLLI-MIC      PIC X(24).                                   
004900     03 TU2-IDPARTNER        PIC X(9).                                    
005000*                                 PARTNER ID                              
005100     03 TU2-FLPCOO           PIC X.                                       
005200*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
005300     03 TU2-KDVALISO-AVC     PIC X(3).                                    
005400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005500     03 TU2-PRAVCOST         PIC S9(7)V9(2)      COMP-3.                  
005600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005700     03 TU2-VKORDBTO-KOLLI   PIC S9(6)V9(1)      COMP-3.                  
005800*                                 ORDERVIKT BRUTTO PER KOLLI              
005900     03 TU2-VLORDBTO-KOLLI   PIC S9(4)V9(3)      COMP-3.                  
006000*                                 ORDERVOLYM BRUTTO KOLLI                 
006100     03 TU2-BEPSNUN          PIC X(6).                                    
006200*                                 DEL AV PSN BENÄMNINGEN                  
006300     03 TU2-KDORSAK          PIC X(5).                                    
006400*                                 ORSAKSKOD EUDR                          
006500     03 TU2-IDREFDDS         PIC X(25).                                   
006600*                                 REFERENS ID DDS                         
006700*** END OF VILMAII-COPY LENGTH= 148 BYTES                                 
