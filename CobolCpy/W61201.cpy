000100 01  W61201.                                                              
000200*                                 PRE-PACKAGE                             
000300     03 ADINLOMR             PIC X(4).                                    
000400*                                 INLEVERANSOMRÅDE                        
000500*                                 RECEIVING AREA                          
000600     03 FILLER1              PIC X.                                       
000700     03 IDARTNR              PIC Z(7)9.                                   
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 FILLER2              PIC X.                                       
001100     03 IDLEVNR-KOLLI        PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER KOLLI                  
001300*                                 SUPPLIER NUMBER CASE                    
001400     03 FILLER3              PIC X.                                       
001500     03 IDOKOLLI             PIC Z(8)9.                                   
001600*                                 ODETTE KOLLINUMMER                      
001700*                                 ODETTE CASE NUMBER                      
001800     03 FILLER4              PIC X.                                       
001900     03 IDINLVGN             PIC 9(3).                                    
002000*                                 VAGNSIDENTITET                          
002100*                                 INTERNAL CARRIER ID                     
002200     03 FILLER5              PIC X.                                       
002300     03 IDLOPNRM             PIC Z(7)9.                                   
002400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002500*                                 (0VVDLLLLK)                             
002600*                                 SERIAL NO RECEIVING REPORT              
002700*                                 (0WWDLLLLC)                             
002800     03 FILLER6              PIC X.                                       
002900     03 BEART                PIC X(25).                                   
003000*                                 ARTIKELBENÄMNING                        
003100*                                 PART DESCRIPTION                        
003200     03 FILLER7              PIC X.                                       
003300     03 KVAVIS               PIC Z(5)9.                                   
003400*                                 AVISERAT ANTAL                          
003500*                                 QUANTITY NOTIFIED                       
003600     03 FILLER8              PIC X.                                       
003700     03 KDFORP.                                                           
003800*                                 FÖRPACKNINGSKOD                         
003900*                                 PACKAGING CODE                          
004000        05 KDFORPPL          PIC 9.                                       
004100*                                 FÖRPACKNINGSPLATS                       
004200*                                 PREPACKING PLACE                        
004300        05 KDFORPGP          PIC 9(2).                                    
004400*                                 FÖRPACKNINGSGRUPP                       
004500*                                 PREPACKING GROUP                        
004600        05 KDFORPUF          PIC 9.                                       
004700*                                 UPPRÄKNINGSFAKTOR                       
004800*                                 ENUMERATION                             
004900     03 FILLER9              PIC X.                                       
005000     03 IDARTNR-EMBQ0        PIC Z(7)9.                                   
005100*                                 EMBALLAGEARTIKELNR FÖR Q0               
005200     03 FILLER10             PIC X.                                       
005300     03 IDARTNR-EMBQ1        PIC Z(7)9.                                   
005400*                                 EMBALLAGEARTIKELNR FÖR Q1               
005500     03 FILLER11             PIC X.                                       
005600     03 IDARTNR-EMBQ2        PIC Z(7)9.                                   
005700*                                 EMBALLAGEARTIKELNR FÖR Q2               
005800     03 FILLER12             PIC X.                                       
005900     03 EMBINFO              PIC X(9).                                    
006000     03 FILLER13             PIC X.                                       
006100     03 KVROS                PIC -(7)9.                                   
006200*                                 RESTORDERSALDO                          
006300*                                 BACKORDER QTY                           
006400     03 FILLER14             PIC X.                                       
006500     03 IDARTNR-EMBQ3        PIC Z(7)9.                                   
006600*                                 EMBALLAGEARTIKELNR FÖR Q3               
006700     03 FILLER15             PIC X.                                       
006800     03 BEFT                 PIC Z(2)9.                                   
006900*                                 FÖRPACKNINGSTYP                         
007000*                                 PACKAGING TYPE                          
007100     03 FILLER16             PIC X.                                       
007200     03 KVQPACK-3            PIC Z(4)9.                                   
007300*                                 ANTAL I Q3 FÖRPACKNING                  
007400*                                 QUANTITY IN BULK PACK Q3                
007500     03 FILLER17             PIC X.                                       
007600     03 TITIDPAK             PIC X(6).                                    
007700*                                 PACKAGE TIME (HHH:MM)                   
007800*                                 PACKAGE TIME (HHH:MM)                   
007900     03 FILLER18             PIC X.                                       
008000     03 TIM-MIN              PIC Z(7)9.9.                                 
008100     03 FILLER19             PIC X.                                       
008200*** END OF VILMAII-COPY LENGTH= 164 BYTES                                 
