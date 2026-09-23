000100 01  W440058.                                                             
000200*                                 KOMPLETTERAD VOR-RAD INFO               
000300     03 IDDISTR              PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC 9(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 IDORDNR7-URSP        PIC 9(7).                                    
000800*                                 ORDERNUMMER                             
000900     03 TIREGDAT-URSP        PIC X(10).                                   
001000     03 TIREGTID-URSP        PIC X(8).                                    
001100     03 TIREGDAT-AVV         PIC X(10).                                   
001200     03 TIREGTID-AVV         PIC X(8).                                    
001300     03 IDARTNR              PIC 9(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 IDORDNR7-LEV         PIC 9(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 TIREGDAT-LEV         PIC X(10).                                   
001800     03 TIREGTID-LEV         PIC X(8).                                    
001900     03 IDROLL               PIC X(5).                                    
002000*                                 VOR ROLL ID                             
002100     03 IDANSK               PIC 9(3).                                    
002200*                                 ANSKAFFARNUMMER                         
002300     03 IDLEVNR              PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500     03 KVBEART              PIC 9(7).                                    
002600*                                 BESTÄLLT ANTAL STYCKEN                  
002700     03 KVBEART-Q            PIC 9(7).                                    
002800*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002900     03 IDDC                 PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 KDORDBEK             PIC 9(2).                                    
003200*                                 ORDERBEKRÄFTELSEKOD                     
003300     03 TIKLAR               PIC X(10).                                   
003400     03 TIKLATID             PIC X(8).                                    
003500     03 WDQ3-GRP.                                                         
003600*                                 ORDER PART INFORMATION                  
003700        05 TIRFS-DATE-URSP   PIC X(10).                                   
003800        05 TIRFS-TIME-URSP   PIC X(8).                                    
003900        05 TIRFS-DATE-LEV    PIC X(10).                                   
004000        05 TIRFS-TIME-LEV    PIC X(8).                                    
004100        05 DATRPAVD-URSP     PIC X(10).                                   
004200        05 TIHHMM-URSP       PIC X(8).                                    
004300        05 DATRPAVD-LEV      PIC X(10).                                   
004400        05 TIHHMM-LEV        PIC X(8).                                    
004500     03 WDE2-GRP.                                                         
004600*                                 KOLLI INFORMATION                       
004700        05 TISKEPPN-LEV      PIC X(10).                                   
004800        05 TISKPTID-LEV      PIC X(8).                                    
004900     03 WDE6-GRP.                                                         
005000*                                 KOLLI INFORMATION                       
005100        05 TIUTSKR-LEV       PIC X(10).                                   
005200        05 TIUTSTID-LEV      PIC X(8).                                    
005300        05 TIPACKN-LEV       PIC X(10).                                   
005400        05 TIPACTID-LEV      PIC X(8).                                    
005500        05 TIFAKT-LEV        PIC X(10).                                   
005600        05 TIFAKTID-LEV      PIC X(8).                                    
005700        05 IDFAKT            PIC 9(7).                                    
005800*                                 FAKTURANUMMER                           
005900        05 IDPLOCK           PIC Z(5)9.                                   
006000*                                 PLOCKARE                                
006100*** END OF VILMAII-COPY LENGTH= 293 BYTES                                 
