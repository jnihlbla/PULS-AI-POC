000100 01  MOD-W6O16901.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-STRECK           PIC X.                                       
001200     03 MOD-REKSIFFR         PIC 9.                                       
001300*                                 KONTROLLSIFFRA                          
001400     03 MOD-BEART            PIC X(25).                                   
001500*                                 ARTIKELBENÄMNING                        
001600     03 MOD-VKART-NTO        PIC 9(8).                                    
001700*                                 ARTIKELNS NETTOVIKT                     
001800     03 MOD-VKART-BTO        PIC 9(8).                                    
001900*                                 ART BRUTTOVIKT MASKINELLT               
002000     03 MOD-VLARTNTO         PIC Z(7)9.9.                                 
002100*                                 ARTIKELVOLYM (CM3)                      
002200     03 MOD-KVLENGTH-NTO     PIC Z(3)9.9.                                 
002300*                                 ARTIKELNS NETTOLÄNGD                    
002400     03 MOD-KVWIDTH-NTO      PIC Z(3)9.9.                                 
002500*                                 ARTIKELNS NETTOBREDD                    
002600     03 MOD-KVHEIGHT-NTO     PIC Z(3)9.9.                                 
002700*                                 ARTIKELNS NETTOHÖJD                     
002800     03 MOD-KVLENGTH-BTO     PIC Z(3)9.9.                                 
002900*                                 ARTIKELNS BRUTTOLÄNGD                   
003000     03 MOD-KVWIDTH-BTO      PIC Z(3)9.9.                                 
003100*                                 ARTIKELNS BRUTTOBREDD                   
003200     03 MOD-KVHEIGHT-BTO     PIC Z(3)9.9.                                 
003300*                                 ARTIKELNS BRUTTOHÖJD                    
003400     03 MOD-KDVSOP           PIC Z(2)9.                                   
003500*                                 VSOP-KOD                                
003600     03 MOD-VKART-NTO-UT     PIC 9(8).                                    
003700*                                 ARTIKELNS NETTOVIKT                     
003800     03 MOD-VKART-NTO-IN-ATTR                                             
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-VKART-NTO-IN     PIC 9(8).                                    
004200*                                 ARTIKELNS NETTOVIKT                     
004300     03 MOD-VKART-BTO-UT     PIC 9(8).                                    
004400*                                 ART BRUTTOVIKT MASKINELLT               
004500     03 MOD-VKART-BTO-IN-ATTR                                             
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-VKART-BTO-IN     PIC 9(8).                                    
004900*                                 ART BRUTTOVIKT MASKINELLT               
005000     03 MOD-VLARTNTO-UT      PIC Z(7)9.9.                                 
005100*                                 ARTIKELVOLYM (CM3)                      
005200     03 MOD-VLARTNTO-IN-ATTR PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-VLARTNTO-IN      PIC X(9).                                    
005500*                                 ARTIKELVOLYM (CM3)                      
005600     03 MOD-KDVSOP-UT        PIC Z(2)9.                                   
005700*                                 VSOP-KOD                                
005800     03 MOD-KDVSOP-IN-ATTR   PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-KDVSOP-IN        PIC 9(3).                                    
006100*                                 VSOP-KOD                                
006200     03 MOD-VKART-NTO-KDP    PIC 9(8).                                    
006300*                                 ARTIKELNS NETTOVIKT                     
006400     03 MOD-KVANTAL-KDP      PIC Z(5)9.                                   
006500*                                 ANTAL                                   
006600     03 MOD-IDUSER           PIC X(8).                                    
006700*                                 ANVÄNDARENS SÄKERHETS ID                
006800     03 MOD-TIUPPDAT         PIC 9(6).                                    
006900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
007000     03 MOD-TIUPPDAT-MASK    PIC 9(6).                                    
007100*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
007200     03 MOD-TEMFSINF         PIC X(55).                                   
007300*                                 INFORMATIONSMEDDELANDE                  
007400*** END OF VILMAII-COPY LENGTH= 308 BYTES                                 
