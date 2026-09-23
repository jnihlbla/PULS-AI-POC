//W440J05G JOB (640W4400100W440J05G,W100),'RTN W440V4',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//*+JBS BIND IMG0                                                               
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//COPY    EXEC W001HFSC,CONV='(BPXFX311)',                                      
//             DSIN='W440.W440V4.W4405F(+0)',                                   
//             PATHOUT='/app/vccs/qase/w440/data/w4405f.xls'                    
//*                                                                             
//*** old path PATHOUT='/volvo/vccsroot/w440/data/w4405f.xls'                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440J05G                                         
