000100 01  RESP-WF0268O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0268         
000300*                                 RECEIVING COUNTRY MAINTENANCE           
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDLANDX3-KEY    PIC X(3).                                    
000800*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
000900*                                 3-LETTER CODE FOR COUNTRY.              
001000     03 RESP-KDSTATUS-KEY    PIC 9(3).                                    
001100*                                 STATUSKOD          KDSTATUS-002         
001200     03 RESP-BELEGRAD-1      PIC X(35).                                   
001300*                                 DEL AV LEGAL SELLER NAMN                
001400*                                 PART OF LEGAL SELLER NAME               
001500     03 RESP-FLCOMING        PIC X.                                       
001600*                                 ALLMÄN FLAGGA                           
001700*                                 GENERAL FLAG                            
001800     03 RESP-BELAND          PIC X(35).                                   
001900*                                 LANDSBETECKNING                         
002000*                                 NAME OF COUNTRY                         
002100     03 RESP-IDSPRAK         PIC X(2).                                    
002200*                                 2-STÄLLIG ISO SPRÅKKOD                  
002300*                                 2-LETTER ISO LANGUAGE CODE              
002400     03 RESP-BERESPRA-1      PIC X(35).                                   
002500*                                 DEL AV ANSV AVDELNINGS NAMN             
002600*                                 PART OF RESP DEPT  NAME                 
002700     03 RESP-BERESPRA-2      PIC X(35).                                   
002800*                                 DEL AV ANSV AVDELNINGS NAMN             
002900*                                 PART OF RESP DEPT  NAME                 
003000     03 RESP-ADRESP-STREET   PIC X(35).                                   
003100*                                 ANSVARIG AVDELNINGS GATUADRESS          
003200*                                 STREET ADDRESS OF RESPONSIBLE D         
003300*                                 PT.                                     
003400     03 RESP-ADRESP-BOX      PIC X(10).                                   
003500*                                 BOX ADRESS ANSVARIG AVDELNING           
003600*                                 BOX ADDRESS RESPONSIBLE DPT.            
003700     03 RESP-ADRESP-CITY     PIC X(35).                                   
003800*                                 ANSVARIG AVDELNINGS STAD (ELLER         
003900*                                  LIKNANDE)                              
004000*                                 CITY OF REPSONSIBLE DPT (OR SIM         
004100*                                 ILAR)                                   
004200     03 RESP-ADRESP-PCODE    PIC X(10).                                   
004300*                                 ANSVARIG AVDELNINGS POSTNUMMER          
004400*                                 RESPONSIBLE DPT. POSTAL CODE            
004500     03 RESP-IDTFN           PIC X(20).                                   
004600*                                 TELEFONNUMMER EXTERNT                   
004700*                                 TELEPHONE NUMBER  EXTERNAL              
004800     03 RESP-IDTFX           PIC X(20).                                   
004900*                                 TELEFAXNUMMER                           
005000*                                 FAXNUMBER                               
005100     03 RESP-IDMAIL          PIC X(60).                                   
005200*                                 MAIL ADRESS                             
005300*                                 MAIL ADDRESS                            
005400     03 RESP-BECONT          PIC X(35).                                   
005500*                                 KONTAKTPERSON                           
005600*                                 CONTACT PERSON                          
005700     03 RESP-IDVAT           PIC X(17).                                   
005800*                                 MOMSREGISTRERINGSNUMMER                 
005900*                                 VAT REGISTRATION NUMBER                 
006000     03 RESP-IDVAT-AGENT     PIC X(17).                                   
006100*                                 MOMSREGISTRERINGSNUMMER AGENT           
006200*                                 VAT REGISTRATION VAT AGENT              
006300     03 RESP-IDBG            PIC X(15).                                   
006400*                                 BANKGIRO                                
006500*                                 BANC CHEQUE ACCOUNT                     
006600     03 RESP-IDPG            PIC X(15).                                   
006700*                                 POSTGIRO                                
006800*                                 POSTAL CHEQUE ACCOUNT                   
006900     03 RESP-SUDOCLIM        PIC Z(10)9.                                  
007000*                                 MINIMUM VALUE FOR PPRINTOUT OF          
007100*                                 FINANCIAL DOCUMENT (INVOICE ETC         
007200*                                 .)                                      
007300     03 RESP-DAREGDAT        PIC Z(8).                                    
007400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
007500*                                 REGISTRATION DATE (YYYYMMDD)            
007600     03 RESP-DAUPPDAT        PIC Z(8).                                    
007700*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
007800*                                                                         
007900*                                 UPDATING DATE     (YYYYMMDD)            
008000*                                                                         
008100     03 RESP-DADELDAT        PIC Z(8).                                    
008200*                                 BORTTAGSDATUM      (ÅÅÅÅMMDD)           
008300*                                 DELETION DATE     (YYYYMMDD)            
008400     03 RESP-IDUSER          PIC X(8).                                    
008500*                                 ANVÄNDARENS SÄKERHETS ID                
008600*                                 USER SECURITY-IDENTITY                  
008700*** END OF VILMAII-COPY LENGTH= 485 BYTES                                 
