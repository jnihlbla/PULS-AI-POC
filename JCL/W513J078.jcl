//W513J078 JOB (650W5130100W513J078,W100),'RTN W513B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  DC=&IDDC KVINVBEG=&KVINVBEG VVKL=&KDVVKL                                   
//*  ARTADR=&ADLAGOMR &ADGANG &ADPLATS                                          
//*  PRODSL=&KDPRODSL FKNGRP=&IDFKNGRP                                          
//*                                                                             
//W513    EXEC W513P078,                                                        
//             INDIN=W513                                                       
//*                                                                             
&IDDC &KVINVBEG &KDVVKL &ADLAGOMR &ADGANG &ADPLATS &KDPRODSL &IDFKNGRP          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513J078                                         
