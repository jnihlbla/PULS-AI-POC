000100 01  W5O10201.                                                            
000200     03 TRANS-NUMMER.                                                     
000300        05 TRANS-SIFF-1      PIC X.                                       
000400        05 TRANS-SIFF-2      PIC X.                                       
000500        05 TRANS-SIFF-3      PIC X.                                       
000600        05 TRANS-SIFF-4      PIC X.                                       
000700     03 MESSAGE              PIC X(38).                                   
000800*                                 MEDDELANDE          MESSAGE-002         
000900     03 IDARTNR-IN           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDARTNR-UT           PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 BINDESTRECK          PIC X.                                       
001400     03 AREA.                                                             
001500        05 REKSIFFR          PIC 9.                                       
001600*                                 KONTROLLSIFFRA                          
001700        05 BEART             PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900        05 KDPRODSL          PIC Z(2)9.                                   
002000*                                 PRODUKTSLAG                             
002100        05 HEADING           PIC X(13).                                   
002200        05 KDPSLLOC          PIC X(2).                                    
002300*                                 PRODUKTSLAG LOKALT                      
002400        05 KDVVKL            PIC 9.                                       
002500*                                 VOLYMVÄRDESKLASS                        
002600        05 KDGK              PIC 9.                                       
002700*                                 GODSMOTTAGAREKOD                        
002800        05 KDLTK             PIC 9.                                       
002900*                                 LAGERTILLHÖRIGHETSKOD                   
003000        05 IDLEVNR           PIC X(5).                                    
003100*                                 LEVERANTÖRNUMMER                        
003200        05 IDANSK            PIC Z(2)9.                                   
003300*                                 ANSKAFFARNUMMER                         
003400        05 PRHEMTAG          PIC Z(6)9.9(2).                              
003500*                                 HEMTAGNINGSKOSTNAD                      
003600        05 PRINK             PIC Z(6)9.9(2).                              
003700*                                 INKÖPSPRIS                              
003800        05 PRARTSTD          PIC Z(6)9.9(2).                              
003900*                                 ARTIKELSTANDARDPRIS                     
004000        05 PRARTBES          PIC Z(6)9.9(2).                              
004100*                                 BESTÄLLNINGSPRIS I KRONOR               
004200        05 PRARTSJK          PIC Z(6)9.9(2).                              
004300*                                 ARTIKELNS SJÄLVKOSTNAD                  
004400        05 PRDIRLON          PIC Z(3)9.9(3).                              
004500*                                 DIREKT LÖN                              
004600        05 PRDMTRL           PIC Z(5)9.9(3).                              
004700*                                 DIREKT MATERIAL                         
004800        05 PROVRPAL          PIC Z(3)9.9(3).                              
004900*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
005000        05 KDTIPPR           PIC 9.                                       
005100*                                 TIPPAT PRIS KOD                         
005200        05 KDVTH             PIC 9.                                       
005300*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
005400        05 KDKG              PIC 9.                                       
005500*                                 KURANSGRUPP                             
005600        05 KDERS             OCCURS 2 TIMES                               
005700                             PIC Z9(2).                                   
005800*                                 ERSÄTTNINGSKOD                          
005900        05 IDLEVNR-SEN       OCCURS 2 TIMES                               
006000                             PIC X(5).                                    
006100*                                 LEVERANTÖRNUMMER                        
006200        05 KVAKS             OCCURS 2 TIMES                               
006300                             PIC -(7)9B(4).                               
006400*                                 ANKOMSTSALDO                            
006500        05 KVEFRS            OCCURS 2 TIMES                               
006600                             PIC -(7)9B(4).                               
006700*                                 EJ FAKTURERAT ANTAL STYCK               
006800        05 KVRESS            OCCURS 2 TIMES                               
006900                             PIC -(7)9B(4).                               
007000*                                 RESERVERAT ANTAL ARTIKLAR               
007100        05 KVUTRS            OCCURS 2 TIMES                               
007200                             PIC -(7)9B(4).                               
007300*                                 UTREDNINGSSALDO                         
007400        05 KVDISP            OCCURS 2 TIMES                               
007500                             PIC -(7)9B(4).                               
007600*                                 DISPONIBELT LAGER                       
007700        05 VAERDE1           OCCURS 2 TIMES                               
007800                             PIC -(7)9.9(2)B.                             
007900        05 VAEWDE2           OCCURS 2 TIMES                               
008000                             PIC -(7)9.9(2)B.                             
008100        05 VAERDE3           OCCURS 2 TIMES                               
008200                             PIC -(7)9.9(2)B.                             
008300        05 VAEWDE4           OCCURS 2 TIMES                               
008400                             PIC -(7)9.9(2)B.                             
008500*** END OF VILMAII-COPY LENGTH= 427 BYTES                                 
