<script type="text/javascript">
    function getMapHeight() {
        return (window.innerHeight - 90);
    }

    $(function () {
        let options = {
            autoResize: true,
            // width: '100%',
            height: getMapHeight() + "px",
        };

        let network = new vis.Network($("#map"), data, options);

        $(window).on('resize', function(){
            network.setOptions({
                // width: (window.innerWidth - 100) + "px",
                height: getMapHeight() + "px",
            });
        });
    });
</script>
