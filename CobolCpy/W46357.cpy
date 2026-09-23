000100 01  W46357.                                                              
000200*                                 PACKNINGSTRANS DIREKTLEVERANS           
000300*                                                                         
000400     03 IDANSTNR             PIC X(5).                                    
000500*                                 ANSTÄLLNINGSNUMMER                      
000600*                                 IDENTIFICATION NO EMPLOYEE              
000700     03 IDDISTR              PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900*                                 DISTRICT NUMBER                         
001000     03 IDKUNDNR             PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200*                                 CUSTOMER NO                             
001300     03 IDORDNR              PIC X(5).                                    
001400*                                 ORDERNUMMER UTGÅR PD90                  
001500*                                 ORDER NUMBER                            
001600     03 IDPRODNR             PIC X(7).                                    
001700*                                 PRODUKTIONSNUMMER                       
001800*                                 PRODUCTION NUMBER                       
001900     03 IDDC                 PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 IDKOLLI              PIC X(5).                                    
002300*                                 KOLLINUMMER                             
002400*                                 CASE NUMBER                             
002500     03 IDSUPREF             PIC X(10).                                   
002600*                                 LEVERANTöRSREF.                         
002700*                                 SUPPLIER REF.                           
002800     03 DASUPREF             PIC X(8).                                    
002900*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
003000*                                 SHIPPING DATE DIRECT SUPPLIER           
003100     03 TISUPTID             PIC X(5).                                    
003200*                                 SÄNDNINGSTID DIREKTLEVERANTÖR           
003300*                                 SHIPPING TIME DIRECT SUPPLIER           
003400     03 VKORDBTO-KOLLI       PIC X(8).                                    
003500*                                 ORDERVIKT BRUTTO PER KOLLI              
003600*                                 ORDER WEIGHT GROSS PER CASE             
003700     03 KDEMBTYP             PIC X.                                       
003800*                                 EMBALLAGETYP                            
003900*                                 PACKAGE TYPE                            
004000     03 DIKOLLIL             PIC X(4).                                    
004100*                                 KOLLI-LÄNGD                             
004200*                                 CASE LENGTH                             
004300     03 DIKOLLIB             PIC X(3).                                    
004400*                                 KOLLI-BREDD                             
004500*                                 CASE WIDTH                              
004600     03 DIKOLLIH             PIC X(3).                                    
004700*                                 KOLLI-HÖJD                              
004800*                                 CASE HEIGHT                             
004900     03 FLSLUT-VORD          PIC X.                                       
005000*                                 AVSLUTNINGSFLAGGA                       
005100     03 FLSLUT               PIC X.                                       
005200*                                 AVSLUTNINGSFLAGGA                       
005300     03 IDSNDNOD             PIC X(8).                                    
005400*                                 SÄNDANDE NODE IDENTITET                 
005500*                                 IDENTITY OF SENDING NODE                
005600     03 VLORDBTO-KOLLI       PIC X(8).                                    
005700*                                 ORDERVOLYM BRUTTO KOLLI                 
005800*                                 ORDER VOL GR/CASE                       
005900     03 RAD                  OCCURS 74 TIMES.                             
006000        05 IDRADNR           PIC X(4).                                    
006100*                                 RADNUMMER                               
006200*                                 LINE NO                                 
006300        05 IDARTNR           PIC 9(8).                                    
006400*                                 ARTIKELNUMMER                           
006500*                                 PART NUMBER                             
006600        05 KVLEVART          PIC X(6).                                    
006700*                                 LEVERERAT ANTAL STYCK                   
006800*                                 DELIVERED QUANTITY                      
006900        05 KDARTURS          PIC X(2).                                    
007000*                                 ARTIKELURSPRUNGSKOD                     
007100*                                 COUNTRY OF ORIGIN                       
007200*** END OF VILMAII-COPY LENGTH= 1574 BYTES                                
