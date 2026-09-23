//W440J05E JOB (640W4400100W440J05E,W100),'RTN W440V4',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//*+JBS BIND IMG0                                                               
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W440    EXEC W440P05E                                                         
//*                                                                             
//COPY    EXEC W001HFSC,CONV='(BPXFX311)',                                      
//             DSIN='W440.W440V4.W4405G(+1)',                                   
//             PATHOUT='/app/vccs/qase/w440/data/w4405g.xls'                    
//*                                                                             
//*** old path PATHOUT='/volvo/vccsroot/w440/data/w4405g.xls'                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440J05E                                         
