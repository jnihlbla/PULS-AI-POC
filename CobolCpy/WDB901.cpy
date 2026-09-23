000100 01  DOK-WDB901.                                                          
000200*                                 PARAMETERREGISTER TILL DOKUMENT         
000300*                                 UTSKRIFT                                
000400*                                 FYSISK NYCKEL: WDB901KY                 
000500*                                 (IDDC + IDDISTR + IDKUND-GRP +          
000600*                                  IDDC-REC)                              
000700     03 DOK-IDDC             PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 DOK-IDDISTR          PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 DOK-IDKUND-GRP.                                                   
001400*                                 KUNDNUMMER ELLER LEVERANTÖRSNR          
001500*                                 CUSTOMER OR SUPPLIER NO.                
001600        05 DOK-IDKUND        PIC X(10).                                   
001700*                                 KUND/LEV ID                             
001800*                                 CUSTOMER/SUPPL ID                       
001900        05 DOK-IDKUNDNR-FILLER REDEFINES DOK-IDKUND.                      
002000           07 DOK-IDKUNDNR   PIC 9(6).                                    
002100*                                 KUNDNUMMER                              
002200*                                 CUSTOMER NO                             
002300           07 FILLER         PIC X(4).                                    
002400        05 DOK-IDLEVNR-FILLER REDEFINES DOK-IDKUND.                       
002500           07 DOK-IDLEVNR    PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002800           07 FILLER         PIC X(5).                                    
002900     03 DOK-IDDC-REC         PIC X(2).                                    
003000*                                 MOTTAGANDE LAGER                        
003100*                                 RECEIVING WAREHOUSE                     
003200     03 DOK-IDPRTLST         PIC X(8).                                    
003300*                                 LOGISK PRINTER+LISTA IDENTITET          
003400*                                 LOGICAL PRINTER+LIST IDENTITY           
003500     03 DOK-IDUSER           PIC X(8).                                    
003600*                                 ANVÄNDARENS SÄKERHETS ID                
003700*                                 USER SECURITY-IDENTITY                  
003800     03 DOK-KVCOPIES-GMTL    PIC X.                                       
003900*                                 ANTAL COPIOR VID PRINTNING              
004000*                                 NUMBER OF PRINTED COPIES                
004100     03 DOK-KVCOPIES-KLIS    PIC X.                                       
004200*                                 ANTAL COPIOR VID PRINTNING              
004300*                                 NUMBER OF PRINTED COPIES                
004400     03 DOK-KVCOPIES-PACK    PIC X.                                       
004500*                                 ANTAL COPIOR VID PRINTNING              
004600*                                 NUMBER OF PRINTED COPIES                
004700     03 DOK-KVCOPIES-SPED    PIC X.                                       
004800*                                 ANTAL COPIOR VID PRINTNING              
004900*                                 NUMBER OF PRINTED COPIES                
005000     03 DOK-KVCOPIES-STAT    PIC X.                                       
005100*                                 ANTAL COPIOR VID PRINTNING              
005200*                                 NUMBER OF PRINTED COPIES                
005300     03 DOK-KVCOPIES-VERS    PIC X.                                       
005400*                                 ANTAL COPIOR VID PRINTNING              
005500*                                 NUMBER OF PRINTED COPIES                
005600     03 DOK-TIUPPDAT         PIC S9(7)           COMP-3.                  
005700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
005800*                                 UPDATING DATE     (YYMMDD)              
005900     03 DOK-KVDAGAR          PIC S9(3)           COMP-3.                  
006000*                                 ANTAL DAGAR                             
006100     03 DOK-FLSKRIV-ONDEM    PIC X.                                       
006200*                                 J/Y = SKRIV BEGÄRD LISTA                
006300*                                 J/Y = PRINT REPORT NOW                  
006400     03 DOK-KVCOPIES-KULB    PIC X.                                       
006500*                                 ANTAL COPIOR VID PRINTNING              
006600*                                 NUMBER OF PRINTED COPIES                
006700     03 DOK-KVCOPIES-NAPR    PIC X.                                       
006800*                                 ANTAL COPIOR VID PRINTNING              
006900*                                 NUMBER OF PRINTED COPIES                
007000     03 DOK-KVCOPIES-BLAD    PIC X.                                       
007100*                                 ANTAL COPIOR VID PRINTNING              
007200*                                 NUMBER OF PRINTED COPIES                
007300     03 DOK-KVCOPIES-TRPT    PIC X.                                       
007400*                                 ANTAL COPIOR VID PRINTNING              
007500*                                 NUMBER OF PRINTED COPIES                
007600*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
