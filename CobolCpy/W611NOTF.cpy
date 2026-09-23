000100 01  NOTF-W611NOTF.                                                       
000200*                                 NOTAFISCAL FOR BR12    -                
000300     03 NOTF-KDEKHHT         PIC X(3).                                    
000400*                                 EKONOMISK HUVUDHÄNDELSE                 
000500*                                 ECONOMIC MAIN EVENT                     
000600     03 NOTF-KDEKSHT         PIC X(3).                                    
000700*                                 EKONOMISK SUBHÄNDELSE                   
000800*                                 ECONOMIC SUB EVENT                      
000900     03 NOTF-DAVERDAT        PIC 9(8).                                    
001000*                                 VERIFIKATIONSDATUM (ÅÅÅÅMMDD)           
001100*                                 VERIFICATION DATE (YYYYMMDD)            
001200     03 NOTF-TIREGTID        PIC 9(6).                                    
001300*                                 REGISTRERINGSTID                        
001400*                                 GENERAL REGISTRATION TIME               
001500     03 NOTF-IDSEKVNR        PIC 9(5).                                    
001600*                                 GENERELLT SEKVENSNUMMER                 
001700*                                 GENERAL SEQUENCE NUMBER                 
001800     03 NOTF-IDVERGL         PIC X(10).                                   
001900*                                 VERIFIKATIONSID FÖR HUVUDBOKEN          
002000*                                 VERIFICATION IDENTITY FOR THE           
002100*                                 GENERAL LEDGER                          
002200     03 NOTF-IDDC            PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400*                                 WAREHOUSE IDENTIFIER                    
002500     03 NOTF-IDFAKT          PIC 9(7).                                    
002600*                                 FAKTURANUMMER                           
002700*                                 INVOICE NO.                             
002800     03 NOTF-IDKUNDNR        PIC 9(6).                                    
002900*                                 KUNDNUMMER                              
003000*                                 CUSTOMER NO                             
003100     03 NOTF-IDORDER         PIC 9(7).                                    
003200*                                 VOLVO PARTS ORDERNUMMER                 
003300*                                 VOLVO PARTS ORDER NUMBER                
003400     03 NOTF-IDKOLLI         PIC 9(5).                                    
003500*                                 KOLLINUMMER                             
003600*                                 CASE NUMBER                             
003700     03 NOTF-IDARTNR20       PIC X(20) JUST.                              
003800*                                 20-STÄLLIGT ARTIKELNUMMER FÖR A         
003900*                                 S400 (VIPS)                             
004000*                                 FORMATET ÄR HÖGERJUSTERAT MED I         
004100*                                 NLEDANDE                                
004200*                                 BLANKTECKEN, OCH UTAN INLEDANDE         
004300*                                  NOLLOR.                                
004400*                                 20 CHARACTER PART NUMBER FOR AS         
004500*                                 400 (VIPS)                              
004600*                                 THE FORMAT IS RIGHT JUSTIFIED W         
004700*                                 ITH LEADING                             
004800*                                 SPACES. NO LEADING ZEROES.              
004900*                                                                         
005000     03 NOTF-KVANTAL         PIC +9(7).                                   
005100*                                 ANTAL                                   
005200*                                 NUMBER                                  
005300*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
