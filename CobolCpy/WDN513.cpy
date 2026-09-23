000100 01  VADIS-WDN513.                                                        
000200*                                 KATALOG VADISINFORMATION                
000300*                                 OCH STATUSFLAGGOR                       
000400*                                 FYSISK NYCKEL: WDN513KY                 
000500*                                 (IDRADNR + KDCATPUB-FOM)                
000600     03 VADIS-IDRADN         PIC S9(5)           COMP-3.                  
000700*                                 RADNUMMER                               
000800*                                 LINE NO                                 
000900     03 VADIS-KDCATPUB-FOM   PIC X(6).                                    
001000*                                 PUBLICERINGS TIDKOD, F.O.M.             
001100*                                 RELEASE TIME CODE, FROM                 
001200     03 VADIS-KDCATPUB-TOM   PIC X(6).                                    
001300*                                 PUBLICERINGS TIDKOD, T.O.M.             
001400*                                 RELEASE TIME CODE, TO                   
001500     03 VADIS-IDKOL          PIC X.                                       
001600*                                 KOLUMN-ID (A-E)                         
001700*                                 COLUMN ID (A-E)                         
001800     03 VADIS-FLEXCL         PIC X.                                       
001900*                                 NYCKELVƒRDEN EXCLUDERAS?                
002000*                                 KEY VALUES TO BE EXCLUDED?              
002100     03 VADIS-IDMODELL       PIC X(3).                                    
002200*                                 BILENS NUMERISKA MODELLBET.             
002300*                                 NUMERIC MODEL ID FOR A VECHICLE         
002400     03 VADIS-TIMODAAR-STA   PIC 9(4).                                    
002500*                                 MODELL≈R (≈≈≈≈) START≈R                 
002600*                                 MODEL YEAR (YYYY) START YEAR            
002700     03 VADIS-TIMODAAR-STO   PIC 9(4).                                    
002800*                                 MODELL≈R (≈≈≈≈) STOPP≈R                 
002900*                                 MODEL YEAR (YYYY) STOP YEAR             
003000     03 VADIS-IDVARIANT      PIC X(15).                                   
003100*                                 BILVARIANT                              
003200*                                 VEHICLE VARIANT                         
003300     03 VADIS-IDVARIANT-2    PIC X(15).                                   
003400*                                 ASSOCIERAD (2:A) BILVARIANT             
003500*                                 ASSOCIATED/2:ND VEHICLE VARIANT         
003600     03 VADIS-KDCHATYP       PIC 9.                                       
003700*                                 CHASSINUMMER-TYP                        
003800*                                 CHASSI NUMBER TYPE                      
003900     03 VADIS-IDCHASSI-STA   PIC 9(6).                                    
004000*                                 CHASSINUMMER START                      
004100*                                 CHASSI NUMBER START                     
004200     03 VADIS-IDCHASSI-STO   PIC 9(6).                                    
004300*                                 CHASSINUMMER STOPP                      
004400*                                 CHASSI NUMBER STOP                      
004500*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
