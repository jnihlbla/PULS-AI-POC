000100 01  W37109.                                                              
000200*                                 SUPERTRANSACTION FOR BYTESVECKO         
000300*                                 RAPPORTER                               
000400*                                 TILL BYTES (W371)                       
000500     03 DAAAVV               PIC 9(6).                                    
000600*                                 ÅR - VECKA  (ÅÅÅÅVV)                    
000700     03 DADATUM              PIC 9(8).                                    
000800*                                 DATUM ENLIGT KDDATFORM                  
000900     03 IDPTYP               PIC X(3).                                    
001000*                                 POSTTYP                                 
001100     03 IDDISTR              PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 IDDISTR-BET          PIC S9(5)           COMP-3.                  
001400*                                 BATALANDE DISTRIKT                      
001500     03 IDARTNR              PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001800*                                 FUNKTIONSGRUPP                          
001900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100     03 IDBYTRAD             PIC S9(5)           COMP-3.                  
002200*                                 RADNUMMER                               
002300     03 IDORDER              PIC S9(7)           COMP-3.                  
002400*                                 VOLVO PARTS ORDERNUMMER                 
002500     03 IDBYTRAP             PIC S9(7)           COMP-3.                  
002600*                                 RAPPORTNUMMER  BYTES                    
002700     03 KDEXCHA              PIC S9(3)           COMP-3.                  
002800*                                 EXCHANGE ACCOUNT CODE                   
002900     03 KDBYTREF             PIC X(3).                                    
003000*                                 CENTRAL REFERENS                        
003100     03 KDBYTSTA-RAPP        PIC X.                                       
003200*                                 STATUSKOD BYTESOBJEKT                   
003300     03 KDBYTSTA-OBJ         PIC X.                                       
003400*                                 STATUSKOD BYTESOBJEKT                   
003500     03 FLBYTKND             PIC X.                                       
003600*                                 FLAGGA BYTESKUND                        
003700     03 FLINKLBS             PIC X.                                       
003800*                                 FLAGGA BYTESBALANS                      
003900     03 BEART-ENG            PIC X(25).                                   
004000*                                 ENGELSK ARTIKELBENÄMNING                
004100     03 KVPOINT              PIC S9(7)           COMP-3.                  
004200*                                 POINT VALUE                             
004300     03 KVANTAL              PIC S9(7)           COMP-3.                  
004400*                                 ANTAL                                   
004500     03 KVRETUR-GODK         PIC S9(7)           COMP-3.                  
004600*                                 ANTAL GODKÄNDA BYTESOBJEKT              
004700     03 KVVECKOR             PIC S9(3)           COMP-3.                  
004800*                                 ANTAL VECKOR                            
004900     03 TENOTE               PIC X(40).                                   
005000*                                 NOTERINGSFÄLT                           
005100     03 SUPOINT              PIC S9(9)           COMP-3.                  
005200*                                 POINT VALUE                             
005300     03 FILLER               PIC X(20).                                   
005400*** END OF VILMAII-COPY LENGTH= 159 BYTES                                 
