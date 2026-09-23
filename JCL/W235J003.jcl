//W235J003 JOB (640W2350100W235J003,W100),'RTN W235B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*IDFTG=&IDFTG                                                                 
//*IDLEVNR=&IDLEVNR                                                             
//*KDPRODSL=&KDPRODSL                                                           
//*IDUSER=&IDUSER                                                               
//*MAIL=&MAIL                                                                   
//*                                                                             
//W235    EXEC W235P003                                                         
//*                                                                             
//*       PARAMETRAR FRÅN SOP                                                   
//W23504.W23504D1 DD *                                                          
&IDFTG.&IDLEVNR.&KDPRODSL.&IDUSER.&MAIL.                                        
/*                                                                              
//*                                                                             
//MAIL  EXEC WMAILSND                                                           
)SEND                                                                           
TITLE SUPPLIERS PERFORMANCE LIST SCREEN 2323                                    
TO     &MAIL                                                                    
ATTACH W235.W235B1.W23504(+1) W23504.DOC TEXT                                   
MAIL                                                                            
 SUPPLIER PERFORMANCE LIST,                                                     
 ORDERED FROM SCREEN 2323                                                       
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W235J003                                         
