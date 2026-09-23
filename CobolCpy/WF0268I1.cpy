000100 01  REQU-WF0268I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0268             
000300*                                 RECEIVING COUNTRY MAINTENANCE           
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-IDLANDX3-KEY    PIC X(3).                                    
000800*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
000900*                                 3-LETTER CODE FOR COUNTRY.              
001000     03 REQU-KDSTATUS-KEY    PIC 9(3).                                    
001100*                                 STATUSKOD          KDSTATUS-002         
001200     03 REQU-FLCOMING        PIC X.                                       
001300*                                 ALLMÄN FLAGGA                           
001400*                                 GENERAL FLAG                            
001500     03 REQU-BELAND          PIC X(35).                                   
001600*                                 LANDSBETECKNING                         
001700*                                 NAME OF COUNTRY                         
001800     03 REQU-IDSPRAK         PIC X(2).                                    
001900*                                 2-STÄLLIG ISO SPRÅKKOD                  
002000*                                 2-LETTER ISO LANGUAGE CODE              
002100     03 REQU-BERESPRA-1      PIC X(35).                                   
002200*                                 DEL AV ANSV AVDELNINGS NAMN             
002300*                                 PART OF RESP DEPT  NAME                 
002400     03 REQU-BERESPRA-2      PIC X(35).                                   
002500*                                 DEL AV ANSV AVDELNINGS NAMN             
002600*                                 PART OF RESP DEPT  NAME                 
002700     03 REQU-ADRESP-STREET   PIC X(35).                                   
002800*                                 ANSVARIG AVDELNINGS GATUADRESS          
002900*                                 STREET ADDRESS OF RESPONSIBLE D         
003000*                                 PT.                                     
003100     03 REQU-ADRESP-BOX      PIC X(10).                                   
003200*                                 BOX ADRESS ANSVARIG AVDELNING           
003300*                                 BOX ADDRESS RESPONSIBLE DPT.            
003400     03 REQU-ADRESP-CITY     PIC X(35).                                   
003500*                                 ANSVARIG AVDELNINGS STAD (ELLER         
003600*                                  LIKNANDE)                              
003700*                                 CITY OF REPSONSIBLE DPT (OR SIM         
003800*                                 ILAR)                                   
003900     03 REQU-ADRESP-PCODE    PIC X(10).                                   
004000*                                 ANSVARIG AVDELNINGS POSTNUMMER          
004100*                                 RESPONSIBLE DPT. POSTAL CODE            
004200     03 REQU-IDTFN           PIC X(20).                                   
004300*                                 TELEFONNUMMER EXTERNT                   
004400*                                 TELEPHONE NUMBER  EXTERNAL              
004500     03 REQU-IDTFX           PIC X(20).                                   
004600*                                 TELEFAXNUMMER                           
004700*                                 FAXNUMBER                               
004800     03 REQU-IDMAIL          PIC X(60).                                   
004900*                                 MAIL ADRESS                             
005000*                                 MAIL ADDRESS                            
005100     03 REQU-BECONT          PIC X(35).                                   
005200*                                 KONTAKTPERSON                           
005300*                                 CONTACT PERSON                          
005400     03 REQU-IDVAT           PIC X(17).                                   
005500*                                 MOMSREGISTRERINGSNUMMER                 
005600*                                 VAT REGISTRATION NUMBER                 
005700     03 REQU-IDVAT-AGENT     PIC X(17).                                   
005800*                                 MOMSREGISTRERINGSNUMMER AGENT           
005900*                                 VAT REGISTRATION VAT AGENT              
006000     03 REQU-IDBG            PIC X(15).                                   
006100*                                 BANKGIRO                                
006200*                                 BANC CHEQUE ACCOUNT                     
006300     03 REQU-IDPG            PIC X(15).                                   
006400*                                 POSTGIRO                                
006500*                                 POSTAL CHEQUE ACCOUNT                   
006600     03 REQU-SUDOCLIM        PIC 9(11).                                   
006700*                                 MINIMUM VALUE FOR PPRINTOUT OF          
006800*                                 FINANCIAL DOCUMENT (INVOICE ETC         
006900*                                 .)                                      
007000     03 REQU-DAUPPDAT        PIC X(8).                                    
007100*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
007200*                                                                         
007300*                                 UPDATING DATE     (YYYYMMDD)            
007400*                                                                         
007500*** END OF VILMAII-COPY LENGTH= 426 BYTES                                 
